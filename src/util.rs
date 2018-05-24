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
use std::str::FromStr;

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

pub fn parse_num_with_suffix(s: &str) -> Option<usize> {
    const SUFFIXES: [char; 8] = ['K', 'M', 'G', 'T', 'P', 'E', 'Z', 'Y'];

    let mut chars = s.chars();
    let mut found_si = false;
    let mut base = 1;
    let mut power = 1;
    {
        loop {
            let ch = chars.clone().rev().next()?;
            let mut rchars = (&mut chars).rev();
            match ch {
                'b' if !found_si => {
                    // special case this one because it's slightly different
                    base = 512;
                    let _ = rchars.next();
                    break;
                }
                'B' if !found_si => {
                    found_si = true;
                }
                _ => {
                    for (i, &suffix) in SUFFIXES.iter().enumerate() {
                        if suffix == ch {
                            base = if found_si {
                                1000
                            } else {
                                1024
                            };
                            power = i as u32 + 1;
                            let _ = rchars.next();
                            break;
                        }
                    }
                    break;
                }
            }
        }
    }

    usize::from_str(chars.as_str()).ok()?.checked_mul(pow(base, power)?)
}

// usize::pow() can panic, and the versions that don't panic are not yet stable
fn pow(mut base: usize, mut exp: u32) -> Option<usize> {
    let mut acc: usize = 1;

    while exp > 1 {
        if (exp & 1) == 1 {
            acc = acc.checked_mul(base)?;
        }
        exp /= 2;
        base = base.checked_mul(base)?;
    }

    if exp == 1 {
        acc = acc.checked_mul(base)?;
    }

    Some(acc)
}
