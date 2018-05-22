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
extern crate globset;
extern crate libc;
extern crate nix;
extern crate chrono;
extern crate crossbeam;
extern crate uucore;
// TODO: convert to use failure instead
#[macro_use]
extern crate quick_error;

use clap::{App, SubCommand};
use failure::{Error, Fail};
use libc::EXIT_FAILURE;
use std::convert::From;
use std::result::Result as StdResult;
use std::ffi::{OsStr, OsString};
use std::fmt::{self, Display};
use std::fs::File;
use std::io::{self, Read, Write};
use std::iter;
use std::os::unix::io::AsRawFd;
use std::path::Path;

pub(crate) use util::*;

#[macro_use]
mod macros;
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
            (Some(ref progname), Some(ref err)) => write!(f, "{}: {}", progname.to_string_lossy(), err),
            (None, Some(ref err)) => err.fmt(f),
            _ => Ok(()),
        }
    }
}

pub struct UtilSetup<I, O, E>
where
    I: for<'a> UtilRead<'a>,
    O: for<'a> UtilWrite<'a>,
    E: for<'a> UtilWrite<'a>,
{
    pub stdin: I,
    pub stdout: O,
    pub stderr: E,
}

impl<I, O, E> UtilSetup<I, O, E>
where
    I: for<'a> UtilRead<'a>,
    O: for<'a> UtilWrite<'a>,
    E: for<'a> UtilWrite<'a>,
{
    pub fn new(stdin: I, stdout: O, stderr: E) -> Self {
        Self {
            stdin: stdin,
            stdout: stdout,
            stderr: stderr,
        }
    }
}
/*
impl<'a, I: UtilRead<'a, IL>, O: UtilWrite<'a, OL>, E: UtilWrite<'a, EL>, IL: Read, OL: Write, EL: Write> Write for UtilSetup<'a, I, O, E, IL, OL, EL> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.stdout.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.stdout.flush()
    }
}

impl<'a, I: UtilRead<'a, IL>, O: UtilWrite<'a, OL>, E: UtilWrite<'a, EL>, IL: Read, OL: Write, EL: Write> Read for UtilSetup<'a, I, O, E, IL, OL, EL> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.stdin.read(buf)
    }
}
*/

pub trait UtilRead<'a>: Read + AsRawFd + Send + Sync {
    type Lock: Read + 'a;

    fn lock_reader<'b: 'a>(&'b mut self) -> Result<Self::Lock>;
}
pub trait UtilWrite<'a>: Write + AsRawFd + Send + Sync {
    type Lock: Write + 'a;

    fn lock_writer<'b: 'a>(&'b mut self) -> Result<Self::Lock>;
}

// TODO: implement for other common things like File, BufReader, etc.

impl<'a> UtilRead<'a> for File {
    type Lock = &'a mut Self;

    fn lock_reader<'b: 'a>(&'b mut self) -> Result<Self::Lock> {
        Ok(self)
    }
}

impl<'a> UtilRead<'a> for io::Stdin {
    type Lock = io::StdinLock<'a>;

    fn lock_reader<'b: 'a>(&'b mut self) -> Result<Self::Lock> {
        Ok(self.lock())
    }
}

impl<'a> UtilWrite<'a> for File {
    type Lock = &'a mut Self;

    fn lock_writer<'b: 'a>(&'b mut self) -> Result<Self::Lock> {
        Ok(self)
    }
}

impl<'a> UtilWrite<'a> for io::Stdout {
    type Lock = io::StdoutLock<'a>;

    fn lock_writer<'b: 'a>(&'b mut self) -> Result<Self::Lock> {
        Ok(self.lock())
    }
}

impl<'a> UtilWrite<'a> for io::Stderr {
    type Lock = io::StderrLock<'a>;

    fn lock_writer<'b: 'a>(&'b mut self) -> Result<Self::Lock> {
        Ok(self.lock())
    }
}

pub type Result<T> = StdResult<T, MesaError>;

fn execute_util<I, O, E, T, U>(
    setup: &mut UtilSetup<I, O, E>,
    name: &OsStr,
    args: T,
) -> Option<Result<()>>
where
    I: for<'a> UtilRead<'a>,
    O: for<'a> UtilWrite<'a>,
    E: for<'a> UtilWrite<'a>,
    T: Iterator<Item = U>,
    U: Into<OsString> + Clone,
{
    include!(concat!(env!("OUT_DIR"), "/execute_utils.rs"))
}

// generate a clap::App such that the available utils are set up as subcommands without any
// arguments (adding all the arguments would slow down startup time)
fn generate_app() -> App<'static, 'static> {
    include!(concat!(env!("OUT_DIR"), "/generate_app.rs"))
}

pub fn execute<I, O, E, T, U>(setup: &mut UtilSetup<I, O, E>, args: T) -> Result<()>
where
    I: for<'a> UtilRead<'a>,
    O: for<'a> UtilWrite<'a>,
    E: for<'a> UtilWrite<'a>,
    T: IntoIterator<Item = U>,
    U: Into<OsString> + Clone,
{
    let mut args = args.into_iter();

    // assume that we are using symlinks first (i.e. "command args" format).  if not, check for
    // "mesatools command args" format
    let res = start(setup, &mut args).or_else(|| start(setup, args)).or_else(|| {
        // no valid util was found, so just display a help menu
        let _ = generate_app().write_help(&mut setup.stderr);
        let _ = writeln!(setup.stderr);

        Some(Err(MesaError::new(None, EXIT_FAILURE, None)))
    }).unwrap();

    let _ = setup.stdout.flush();
    let _ = setup.stderr.flush();

    res
}

fn start<I, O, E, T, U>(setup: &mut UtilSetup<I, O, E>, mut args: T) -> Option<Result<()>>
where
    I: for<'a> UtilRead<'a>,
    O: for<'a> UtilWrite<'a>,
    E: for<'a> UtilWrite<'a>,
    T: Iterator<Item = U>,
    U: Into<OsString> + Clone,
{
    if let Some(progname) = args.next() {
        if let Some(filename) = Path::new(&progname.clone().into()).file_name() {
            // we pass along the args in case the util requires non-standard argument
            // parsing (e.g. dd)
            return execute_util(
                setup,
                filename,
                iter::once(progname).chain(args),
            ).map(|res| {
                // XXX: note that this currently is useless as we are temporarily overriding -V and --help
                res.or_else(|mut mesa_err| {
                    if let Some(ref e) = mesa_err.err {
                        if let Some(clap_err) = e.downcast_ref::<clap::Error>() {
                            if clap_err.kind == clap::ErrorKind::HelpDisplayed || clap_err.kind == clap::ErrorKind::VersionDisplayed {
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
            });
        }
    }

    None
}
