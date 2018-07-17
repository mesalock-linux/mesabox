use clap;
use failure::{self, Compat};
use nix;

use std::ffi::OsString;
use std::io;
use std::num::ParseIntError;
use std::os::unix::io::RawFd;
use std::result::Result as StdResult;

use ::error::LockError;

pub type Result<T> = StdResult<T, ShellError>;
pub type CmdResult<T> = StdResult<T, CommandError>;

#[derive(Fail, Debug)]
pub enum ShellError {
    /// Indicate that a command failed to start
    #[fail(display = "{}: {}", cmdname, err)]
    Command {
        #[cause] err: CommandError,
        cmdname: String,
    },

    #[fail(display = "error while running subshell: {}", _0)]
    SubShell(#[cause] CommandError),

    #[fail(display = "failed to spawn subshell: {}", _0)]
    Spawn(#[cause] CommandError),
}

#[derive(Fail, Debug)]
pub enum CommandError {
    #[fail(display = "{}", _0)]
    StartRealCommand(#[cause] io::Error),

    #[fail(display = "could not get exit status: {}", _0)]
    RealCommandStatus(#[cause] io::Error),

    #[fail(display = "could not duplicate fd {}: {}", fd, err)]
    DupFd {
        #[cause] err: nix::Error,
        fd: RawFd,
    },

    #[fail(display = "bad fd number ({})", _0)]
    InvalidFd(u8),

    #[fail(display = "{}", _0)]
    Pipe(#[cause] nix::Error),

    #[fail(display = "{}", _0)]
    PipeIo(#[cause] io::Error),

    #[fail(display = "could not set up fd {} as file {}: {}", fd, filename, err)]
    FdAsFile {
        #[cause] err: io::Error,
        fd: RawFd,
        filename: String,
    },

    #[fail(display = "{}", _0)]
    Builtin(#[cause] BuiltinError),

    // this should only occur when spawning a non-simple, non-subshell command
    #[fail(display = "{}", _0)]
    Shell(#[cause] Box<Compat<ShellError>>),

    // XXX: depending on any features we decide to add, this may expand to include functions
}

#[derive(Fail, Debug)]
pub enum BuiltinError {
    #[fail(display = "{}", _0)]
    Clap(#[cause] clap::Error),

    /// Wrapper for a generic I/O error
    #[fail(display = "{}", _0)]
    Io(#[cause] io::Error),

    /// Wrapper for a generic nix error (most likely I/O related)
    #[fail(display = "{}", _0)]
    Nix(#[cause] nix::Error),

    /// Wrapper for a LockError (note that this may be removed if UtilRead/UtilWrite are never
    /// implemented for something that can fail to lock)
    #[fail(display = "{}", _0)]
    Lock(#[cause] LockError),

    #[fail(display = "illegal number {:?}: {}", string, err)]
    ParseInt {
        #[cause] err: ParseIntError,
        string: OsString,
    },

    #[fail(display = "illegal number {:?}", _0)]
    InvalidNumber(OsString),

    #[fail(display = "invalid string {:?}", _0)]
    InvalidUtf8(OsString),

    #[fail(display = "{}", _0)]
    Other(#[cause] Compat<failure::Error>),
}

impl From<clap::Error> for BuiltinError {
    fn from(err: clap::Error) -> Self {
        BuiltinError::Clap(err)
    }
}

impl From<io::Error> for BuiltinError {
    fn from(err: io::Error) -> Self {
        BuiltinError::Io(err)
    }
}

impl From<nix::Error> for BuiltinError {
    fn from(err: nix::Error) -> Self {
        BuiltinError::Nix(err)
    }
}

impl From<LockError> for BuiltinError {
    fn from(err: LockError) -> Self {
        BuiltinError::Lock(err)
    }
}