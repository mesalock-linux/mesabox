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
use std::os::unix::io::AsRawFd;
use std::path::Path;
use std::result::Result as StdResult;
use std::str::FromStr;

// defined out here rather than in parse_num_with_suffix() because we need the array for testing
const SUFFIXES: [char; 8] = ['K', 'M', 'G', 'T', 'P', 'E', 'Z', 'Y'];
const OBSOLETE_SUFFIXES: [char; 2] = ['k', 'm'];

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
    error.map_err(|e| failure::err_msg(e).compat().into())
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
    parse_num_common(s, &SUFFIXES, false)
}

pub fn parse_obsolete_num(s: &str) -> Option<usize> {
    parse_num_common(s, &OBSOLETE_SUFFIXES, true)
}

fn parse_num_common(s: &str, suffixes: &[char], obsolete: bool) -> Option<usize> {
    let mut chars = s.chars();
    let mut found_si = false;
    let mut base = 1;
    let mut power = 1;
    loop {
        let ch = chars.clone().rev().next()?;
        let mut rchars = (&mut chars).rev();
        match ch {
            'b' if !found_si => {
                // special case this one because it's slightly different
                base = 512;
                let _ = rchars.next();
                if obsolete {
                    found_si = true;
                } else {
                    break;
                }
            }
            'B' if !found_si && !obsolete => {
                let _ = rchars.next();
                found_si = true;
            }
            _ => {
                for (i, &suffix) in suffixes.iter().enumerate() {
                    if suffix == ch {
                        base = if found_si { 1000 } else { 1024 };
                        power = i as u32 + 1;
                        let _ = rchars.next();
                        break;
                    }
                }
                break;
            }
        }
    }

    usize::from_str(chars.as_str())
        .ok()?
        .checked_mul(pow(base, power)?)
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

#[test]
fn parse_num_invalid() {
    let strings = ["  1", "1  ", "  1  ", "1X", "b", "1 b", "-1"];
    for s in strings.iter() {
        assert_eq!(parse_num_with_suffix(s), None);
    }

    for suffix in &SUFFIXES {
        assert_eq!(parse_num_with_suffix(&suffix.to_string()), None);
        assert_eq!(parse_num_with_suffix(&format!("{}B", suffix)), None);
        assert_eq!(parse_num_with_suffix(&format!("1 {}", suffix)), None);
        assert_eq!(parse_num_with_suffix(&format!("1 {}B", suffix)), None);

        // TODO: add tests ensuring too large values fail as well
    }

    assert_eq!(
        parse_num_with_suffix(&format!("{}1", usize::max_value())),
        None
    );
}

#[test]
fn parse_num_valid() {
    let strings = [("0", 0), ("1", 1), ("1b", 512)];
    for s in strings.iter() {
        assert_eq!(parse_num_with_suffix(s.0), Some(s.1));
    }

    for (i, suffix) in SUFFIXES.iter().enumerate() {
        let exp = i as u32 + 1;

        assert_eq!(
            parse_num_with_suffix(&format!("1{}", suffix)),
            pow(1024, exp)
        );
        assert_eq!(
            parse_num_with_suffix(&format!("1{}B", suffix)),
            pow(1000, exp)
        );

        // TODO: add tests ensuring values that are almost too large pass
    }

    assert_eq!(
        parse_num_with_suffix(&format!("{}", usize::max_value())),
        Some(usize::max_value())
    );
}

#[test]
fn pow_overflow() {
    let root = (usize::max_value() as f64).sqrt().ceil() as usize;

    assert_eq!(pow(root, 2), None);
    assert!(pow(root - 1, 2).is_some());
}

#[test]
fn pow_correct() {
    assert_eq!(pow(1, 0), Some(1));
    assert_eq!(pow(2, 16), Some(65536));
    assert_eq!(pow(256, 2), Some(65536));
}
