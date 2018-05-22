//
// Copyright (c) 2018, The MesaLock Linux Project Contributors
// All rights reserved.
// 
// This work is licensed under the terms of the BSD 3-Clause License.
// For a copy, see the LICENSE file.
//

use super::{MesaError, Result};

use failure;
use libc;
use std::error::Error as StdError;
use std::result::Result as StdResult;
use std::path::Path;
use std::os::unix::io::AsRawFd;

#[allow(dead_code)]
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

pub(crate) fn string_to_err<T>(error: StdResult<T, String>) -> Result<T> {
    error.map_err(|e| {
        failure::err_msg(e).compat().into()
    })
}

pub(crate) fn is_tty<T: AsRawFd>(stream: &T) -> bool {
    unsafe { libc::isatty(stream.as_raw_fd()) == 1 }
}

#[allow(dead_code)]
pub(crate) fn one_filesystem<T, U>(_start_dir: T, _func: U) -> Result<()>
where
    T: AsRef<Path>,
    U: FnMut() -> Result<()>,
{
    // TODO: should probably loop over specified directory or something and call the function
    Ok(())
}
