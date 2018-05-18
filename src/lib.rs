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
// TODO: convert to use failure instead
#[macro_use]
extern crate quick_error;

use clap::{App, SubCommand};
use failure::Error;
use libc::EXIT_FAILURE;
use std::convert::From;
use std::error::Error as StdError;
use std::result::Result as StdResult;
use std::ffi::{OsStr, OsString};
use std::fmt::{self, Display};
use std::io::{self, Read, Write};
use std::iter::{self, Chain, Once};
use std::os::unix::io::AsRawFd;
use std::path::Path;

#[macro_use]
mod macros;

// contains all the "mod"s which allow us to use the utils
include!(concat!(env!("OUT_DIR"), "/utils.rs"));

pub struct MesaError {
    pub exitcode: libc::c_int,
    pub err: Option<Error>,
}

impl MesaError {
    pub fn new(exitcode: libc::c_int, err: Option<Error>) -> Self {
        Self {
            exitcode: exitcode,
            err: err,
        }
    }

    pub fn with_exitcode(mut self, code: libc::c_int) -> Self {
        self.exitcode = code;
        self
    }
}

impl<E: StdError + Send + Sync + 'static> From<E> for MesaError {
    fn from(error: E) -> Self {
        Self {
            exitcode: 1,
            err: Some(error.into()),
        }
    }
}

impl Display for MesaError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(ref err) = self.err {
            err.fmt(f)
        } else {
            Ok(())
        }
    }
}

pub struct UtilSetup<I: UtilRead, O: UtilWrite, E: UtilWrite> {
    pub stdin: I,
    pub stdout: O,
    pub stderr: E,
}

impl<I: UtilRead, O: UtilWrite, E: UtilWrite> UtilSetup<I, O, E> {
    pub fn new(stdin: I, stdout: O, stderr: E) -> Self {
        Self {
            stdin: stdin,
            stdout: stdout,
            stderr: stderr,
        }
    }
}

impl<I: UtilRead, O: UtilWrite, E: UtilWrite> Write for UtilSetup<I, O, E> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.stdout.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.stdout.flush()
    }
}

impl<I: UtilRead, O: UtilWrite, E: UtilWrite> Read for UtilSetup<I, O, E> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.stdin.read(buf)
    }
}

pub trait UtilRead: Read + AsRawFd { }
pub trait UtilWrite: Write + AsRawFd { }

impl<T: Read + AsRawFd> UtilRead for T { }
impl<T: Write + AsRawFd> UtilWrite for T { }

pub type Result<T> = std::result::Result<T, MesaError>;

pub type ArgsIter<T, U> = Chain<Once<U>, T>;

pub(crate) fn set_exitcode<T, E: StdError + Send + Sync + 'static>(
    error: StdResult<T, E>,
    code: libc::c_int,
) -> Result<T> {
    error.map_err(|e| {
        let mut err: MesaError = e.into();
        err.exitcode = code;
        err
    })
}

pub(crate) fn is_tty<T: AsRawFd>(stream: &T) -> bool {
    unsafe { libc::isatty(stream.as_raw_fd()) == 1 }
}

fn execute_util<I, O, E, T, U>(
    setup: &mut UtilSetup<I, O, E>,
    name: &OsStr,
    args: ArgsIter<T, U>,
) -> Option<Result<()>>
where
    I: UtilRead,
    O: UtilWrite,
    E: UtilWrite,
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
    I: UtilRead,
    O: UtilWrite,
    E: UtilWrite,
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

        Some(Err(MesaError::new(EXIT_FAILURE, None)))
    }).unwrap();

    let _ = setup.stdout.flush();
    let _ = setup.stderr.flush();

    res
}

fn start<I, O, E, T, U>(setup: &mut UtilSetup<I, O, E>, mut args: T) -> Option<Result<()>>
where
    I: UtilRead,
    O: UtilWrite,
    E: UtilWrite,
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
                res.or_else(|err| {
                    if let Some(ref e) = err.err {
                        if let Some(clap_err) = e.downcast_ref::<clap::Error>() {
                            if clap_err.kind == clap::ErrorKind::HelpDisplayed || clap_err.kind == clap::ErrorKind::VersionDisplayed {
                                return Ok(());
                            }
                        }
                    }
                    // TODO: check for --help and -V/--version probably
                    Err(err)
                })
            });
        }
    }

    None
}
