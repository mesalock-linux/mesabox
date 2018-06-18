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
use libc::EXIT_FAILURE;
use std::cell::{RefCell, RefMut};
use std::ffi::{OsStr, OsString};
use std::io::{BufRead, Read, Write};
use std::iter;
use std::os::unix::io::RawFd;
use std::path::{Path, PathBuf};
use std::result::Result as StdResult;

pub use error::*;
pub use setup::*;
#[allow(unused)]
pub(crate) use util::*;

mod error;
#[macro_use]
#[allow(unused_macros)]
mod macros;
mod setup;
#[allow(dead_code)]
mod util;

// contains all the "mod"s which allow us to use the utils
include!(concat!(env!("OUT_DIR"), "/utils.rs"));

pub struct UtilData<I, O, E, T>
where
    I: for<'a> UtilRead<'a>,
    O: for<'a> UtilWrite<'a>,
    E: for<'a> UtilWrite<'a>,
    T: Iterator<Item = (OsString, OsString)>,
{
    pub stdin: RefCell<I>,
    pub stdout: RefCell<O>,
    pub stderr: RefCell<E>,
    pub env: T,
    pub current_dir: Option<PathBuf>,
}

impl<I, O, E, T> UtilData<I, O, E, T>
where
    I: for<'a> UtilRead<'a>,
    O: for<'a> UtilWrite<'a>,
    E: for<'a> UtilWrite<'a>,
    T: Iterator<Item = (OsString, OsString)>,
{
    pub fn new(stdin: I, stdout: O, stderr: E, env: T, current_dir: Option<PathBuf>) -> Self {
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
    type Env: Iterator<Item = (OsString, OsString)>;

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

    fn env(&mut self) -> &mut Self::Env;

    fn current_dir(&self) -> Option<&Path>;
}

impl<I, O, E, T> UtilSetup for UtilData<I, O, E, T>
where
    I: for<'a> UtilRead<'a>,
    O: for<'a> UtilWrite<'a>,
    E: for<'a> UtilWrite<'a>,
    T: Iterator<Item = (OsString, OsString)>,
{
    type Input = I;
    type Output = O;
    type Error = E;
    type Env = T;

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

    fn env(&mut self) -> &mut Self::Env {
        &mut self.env
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
            return execute_util(setup, filename, &mut iter::once(progname).chain(args))
                .map(|res| handle_util_result(setup, filename, res));
        }
    }

    None
}

fn handle_util_result<S>(setup: &mut S, filename: &OsStr, res: Result<()>) -> Result<()>
where
    S: UtilSetup,
{
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
}
