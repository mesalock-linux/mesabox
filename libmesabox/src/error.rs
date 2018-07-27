use failure::{Error, Fail};
use std::ffi::OsString;
use std::fmt::{self, Display};

use super::ExitCode;

#[derive(Debug)]
pub struct MesaError {
    pub(crate) progname: Option<OsString>,
    pub exitcode: ExitCode,
    pub err: Option<Error>,
}

impl MesaError {
    pub fn new(progname: Option<OsString>, exitcode: ExitCode, err: Option<Error>) -> Self {
        Self {
            progname: progname,
            exitcode: exitcode,
            err: err,
        }
    }

    pub fn with_exitcode(mut self, code: ExitCode) -> Self {
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
