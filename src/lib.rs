//
// Copyright (c) 2018, The MesaLock Linux Project Contributors
// All rights reserved.
//
// This work is licensed under the terms of the BSD 3-Clause License.
// For a copy, see the LICENSE file.
//

#[macro_use]
extern crate clap;
extern crate failure;
#[macro_use]
extern crate failure_derive;
extern crate libc;

#[cfg(feature = "byteorder")]
extern crate byteorder;
#[cfg(feature = "chrono")]
extern crate chrono;
#[cfg(feature = "crossbeam")]
extern crate crossbeam;
#[cfg(feature = "fnv")]
extern crate fnv;
#[cfg(feature = "globset")]
extern crate globset;
#[cfg(feature = "mio")]
extern crate mio;
#[cfg(feature = "nix")]
extern crate nix;
#[cfg(feature = "pnet")]
extern crate pnet;
#[cfg(feature = "regex")]
extern crate regex;
#[cfg(feature = "socket2")]
extern crate socket2;
#[cfg(feature = "trust-dns-resolver")]
extern crate trust_dns_resolver;
#[cfg(feature = "uucore")]
extern crate uucore;
#[cfg(feature = "walkdir")]
extern crate walkdir;

use clap::{App, SubCommand};
use failure::{Error, Fail};
use libc::EXIT_FAILURE;
use std::cell::{RefCell, RefMut};
use std::convert::From;
use std::ffi::{OsStr, OsString};
use std::fmt::{self, Display};
use std::fs::File;
use std::io::{self, BufRead, BufReader, BufWriter, Read, Write};
use std::iter;
use std::os::unix::io::{AsRawFd, RawFd};
use std::path::{Path, PathBuf};
use std::result::Result as StdResult;

#[allow(unused)]
pub(crate) use util::*;

#[macro_use]
#[allow(unused_macros)]
mod macros;
#[allow(dead_code)]
mod util;

// contains all the "mod"s which allow us to use the utils
include!(concat!(env!("OUT_DIR"), "/utils.rs"));

#[derive(Debug)]
pub struct MesaError {
    pub(crate) progname: Option<OsString>,
    pub exitcode: libc::c_int,
    pub err: Option<Error>,
}

impl MesaError {
    pub fn new(progname: Option<OsString>, exitcode: libc::c_int, err: Option<Error>) -> Self {
        Self {
            progname: progname,
            exitcode: exitcode,
            err: err,
        }
    }

    pub fn with_exitcode(mut self, code: libc::c_int) -> Self {
        self.exitcode = code;
        self
    }
}

impl<E: Fail + Send + Sync + 'static> From<E> for MesaError {
    fn from(error: E) -> Self {
        Self {
            progname: None,
            exitcode: 1,
            err: Some(error.into()),
        }
    }
}

impl Display for MesaError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match (&self.progname, &self.err) {
            (Some(ref progname), Some(ref err)) => {
                write!(f, "{}: {}", progname.to_string_lossy(), err)
            }
            (None, Some(ref err)) => err.fmt(f),
            _ => Ok(()),
        }
    }
}

#[derive(Fail, Debug)]
#[fail(display = "{}: failed to lock", file)]
pub struct LockError {
    file: String,
}

pub struct UtilData<I, O, E>
where
    I: for<'a> UtilRead<'a>,
    O: for<'a> UtilWrite<'a>,
    E: for<'a> UtilWrite<'a>,
{
    pub stdin: RefCell<I>,
    pub stdout: RefCell<O>,
    pub stderr: RefCell<E>,
    pub env: Box<Iterator<Item = (OsString, OsString)>>,
    pub current_dir: Option<PathBuf>,
}

impl<I, O, E> UtilData<I, O, E>
where
    I: for<'a> UtilRead<'a>,
    O: for<'a> UtilWrite<'a>,
    E: for<'a> UtilWrite<'a>,
{
    pub fn new(
        stdin: I,
        stdout: O,
        stderr: E,
        env: Box<Iterator<Item = (OsString, OsString)>>,
        current_dir: Option<PathBuf>,
    ) -> Self {
        Self {
            stdin: RefCell::new(stdin),
            stdout: RefCell::new(stdout),
            stderr: RefCell::new(stderr),
            env: env,
            current_dir: current_dir,
        }
    }
}

pub trait UtilSetup {
    type Input: for<'a> UtilRead<'a>;
    type Output: for<'a> UtilWrite<'a>;
    type Error: for<'a> UtilWrite<'a>;

    fn input(&self) -> RefMut<Self::Input>;
    fn output(&self) -> RefMut<Self::Output>;
    fn error(&self) -> RefMut<Self::Error>;

    fn stdio(
        &self,
    ) -> (
        RefMut<Self::Input>,
        RefMut<Self::Output>,
        RefMut<Self::Error>,
    );

    fn current_dir(&self) -> Option<&Path>;
}

impl<I, O, E> UtilSetup for UtilData<I, O, E>
where
    I: for<'a> UtilRead<'a>,
    O: for<'a> UtilWrite<'a>,
    E: for<'a> UtilWrite<'a>,
{
    type Input = I;
    type Output = O;
    type Error = E;

    fn input(&self) -> RefMut<Self::Input> {
        self.stdin.borrow_mut()
    }

    fn output(&self) -> RefMut<Self::Output> {
        self.stdout.borrow_mut()
    }

    fn error(&self) -> RefMut<Self::Error> {
        self.stderr.borrow_mut()
    }

    fn stdio(
        &self,
    ) -> (
        RefMut<Self::Input>,
        RefMut<Self::Output>,
        RefMut<Self::Error>,
    ) {
        (self.input(), self.output(), self.error())
    }

    fn current_dir(&self) -> Option<&Path> {
        self.current_dir.as_ref().map(|p| p.as_path())
    }
}

pub trait UtilRead<'a>: Read + Send + Sync {
    type Lock: BufRead + 'a;

    fn lock_reader<'b: 'a>(&'b mut self) -> StdResult<Self::Lock, LockError>;

    fn raw_fd(&self) -> Option<RawFd> {
        None
    }
}
pub trait UtilWrite<'a>: Write + Send + Sync {
    type Lock: Write + 'a;

    fn lock_writer<'b: 'a>(&'b mut self) -> StdResult<Self::Lock, LockError>;

    fn raw_fd(&self) -> Option<RawFd> {
        None
    }
}

impl<'a, 'b, T: UtilRead<'a>> UtilRead<'a> for &'b mut T {
    type Lock = T::Lock;

    fn lock_reader<'c: 'a>(&'c mut self) -> StdResult<Self::Lock, LockError> {
        (**self).lock_reader()
    }

    fn raw_fd(&self) -> Option<RawFd> {
        (**self).raw_fd()
    }
}

impl<'a, 'b, T: UtilWrite<'a>> UtilWrite<'a> for &'b mut T {
    type Lock = T::Lock;

    fn lock_writer<'c: 'a>(&'c mut self) -> StdResult<Self::Lock, LockError> {
        (**self).lock_writer()
    }

    fn raw_fd(&self) -> Option<RawFd> {
        (**self).raw_fd()
    }
}

// TODO: implement for other common things like File, BufReader, etc.

impl<'a, 'b> UtilRead<'a> for &'b [u8] {
    type Lock = &'a [u8];

    fn lock_reader<'c: 'a>(&'c mut self) -> StdResult<Self::Lock, LockError> {
        Ok(self)
    }
}

impl<'a> UtilWrite<'a> for Vec<u8> {
    type Lock = &'a mut Self;

    fn lock_writer<'b: 'a>(&'b mut self) -> StdResult<Self::Lock, LockError> {
        Ok(self)
    }
}

impl<'a> UtilRead<'a> for File {
    type Lock = BufReader<&'a mut Self>;

    fn lock_reader<'b: 'a>(&'b mut self) -> StdResult<Self::Lock, LockError> {
        Ok(BufReader::new(self))
    }

    fn raw_fd(&self) -> Option<RawFd> {
        Some(self.as_raw_fd())
    }
}

impl<'a> UtilRead<'a> for io::Stdin {
    type Lock = io::StdinLock<'a>;

    fn lock_reader<'b: 'a>(&'b mut self) -> StdResult<Self::Lock, LockError> {
        Ok(self.lock())
    }

    fn raw_fd(&self) -> Option<RawFd> {
        Some(self.as_raw_fd())
    }
}

impl<'a> UtilWrite<'a> for File {
    type Lock = BufWriter<&'a mut Self>;

    fn lock_writer<'b: 'a>(&'b mut self) -> StdResult<Self::Lock, LockError> {
        Ok(BufWriter::new(self))
    }

    fn raw_fd(&self) -> Option<RawFd> {
        Some(self.as_raw_fd())
    }
}

impl<'a> UtilWrite<'a> for io::Stdout {
    type Lock = io::StdoutLock<'a>;

    fn lock_writer<'b: 'a>(&'b mut self) -> StdResult<Self::Lock, LockError> {
        Ok(self.lock())
    }

    fn raw_fd(&self) -> Option<RawFd> {
        Some(self.as_raw_fd())
    }
}

impl<'a> UtilWrite<'a> for io::Stderr {
    type Lock = io::StderrLock<'a>;

    fn lock_writer<'b: 'a>(&'b mut self) -> StdResult<Self::Lock, LockError> {
        Ok(self.lock())
    }

    fn raw_fd(&self) -> Option<RawFd> {
        Some(self.as_raw_fd())
    }
}

pub trait ArgsIter: Iterator<Item = <Self as ArgsIter>::ArgItem> {
    type ArgItem: Into<OsString> + Clone;
}

impl<'a, T: Into<OsString> + Clone, U: Iterator<Item = T>> ArgsIter for &'a mut U {
    type ArgItem = T;
}

pub type Result<T> = StdResult<T, MesaError>;

fn execute_util<S, T>(setup: &mut S, name: &OsStr, args: T) -> Option<Result<()>>
where
    S: UtilSetup,
    T: ArgsIter,
{
    include!(concat!(env!("OUT_DIR"), "/execute_utils.rs"))
}

// generate a clap::App such that the available utils are set up as subcommands without any
// arguments (adding all the arguments would slow down startup time)
fn generate_app() -> App<'static, 'static> {
    include!(concat!(env!("OUT_DIR"), "/generate_app.rs"))
}

pub fn execute<S, T, U, V>(setup: &mut S, args: T) -> Result<()>
where
    S: UtilSetup,
    T: IntoIterator<IntoIter = V, Item = U>,
    U: Into<OsString> + Clone,
    V: ArgsIter<ArgItem = U>,
{
    let mut args = args.into_iter();

    // assume that we are using symlinks first (i.e. "command args" format).  if not, check for
    // "mesabox command args" format
    let res = start(setup, &mut args)
        .or_else(|| start(setup, &mut args))
        .or_else(|| {
            // no valid util was found, so just display a help menu
            let _ = generate_app().write_help(&mut *setup.error());
            let _ = writeln!(setup.error());

            Some(Err(MesaError::new(None, EXIT_FAILURE, None)))
        })
        .unwrap();

    let _ = setup.output().flush();
    let _ = setup.error().flush();

    res
}

fn start<S, T>(setup: &mut S, args: &mut T) -> Option<Result<()>>
where
    S: UtilSetup,
    T: ArgsIter,
{
    if let Some(progname) = args.next() {
        if let Some(filename) = Path::new(&progname.clone().into()).file_name() {
            // we pass along the args in case the util requires non-standard argument
            // parsing (e.g. dd)
            return execute_util(setup, filename, &mut iter::once(progname).chain(args)).map(
                |res| {
                    // XXX: note that this currently is useless as we are temporarily overriding -V and --help
                    res.or_else(|mut mesa_err| {
                        if let Some(ref e) = mesa_err.err {
                            if let Some(clap_err) = e.downcast_ref::<clap::Error>() {
                                if clap_err.kind == clap::ErrorKind::HelpDisplayed
                                    || clap_err.kind == clap::ErrorKind::VersionDisplayed
                                {
                                    write!(setup.output(), "{}", clap_err)?;
                                    return Ok(());
                                }
                            }
                        }
                        // TODO: check for --help and -V/--version probably
                        if mesa_err.progname.is_none() {
                            mesa_err.progname = Some(filename.to_os_string());
                        }
                        Err(mesa_err)
                    })
                },
            );
        }
    }

    None
}
