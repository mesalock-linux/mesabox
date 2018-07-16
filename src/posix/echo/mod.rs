//
// Copyright (c) 2018, The MesaLock Linux Project Contributors
// All rights reserved.
// 
// This work is licensed under the terms of the BSD 3-Clause License.
// For a copy, see the LICENSE file.
//

use super::{ArgsIter, Result, UtilSetup, UtilWrite};

use std::ffi::OsStr;
use std::io::Write;
use std::os::unix::ffi::OsStrExt;
use std::str;

#[allow(unused)]
pub(crate) const NAME: &str = "echo";
pub(crate) const DESCRIPTION: &str = "Write string(s) to stdout with a trailing newline";

pub fn execute<S, T>(setup: &mut S, mut args: T) -> Result<()>
where
    S: UtilSetup,
    T: ArgsIter,
{
    // this is the program name, so just ignore it
    args.next();

    let output = setup.output();
    let mut output = output.lock_writer()?;

    let mut print_newline = true;

    if let Some(s) = args.next() {
        if write_str(&mut output, s.as_ref())? {
            print_newline = false;
        } else {
            for s in args {
                write!(output, " ")?;
                if write_str(&mut output, s.as_ref())? {
                    print_newline = false;
                    break;
                }
            }
        }
    }

    if print_newline {
        writeln!(output)?;
    }

    Ok(())
}

/// Write a string given on the command-line to output.  If the string contains \c, return
/// `Ok(true)`.
fn write_str<W: Write>(output: &mut W, s: &OsStr) -> Result<bool> {
    let mut found_c = false;

    let mut iter = s.as_bytes().iter();
    while let Some(&byte) = iter.next() {
        let out_byte = match byte {
            b'\\' => match iter.next() {
                Some(b'a') => b'\x07',
                Some(b'b') => b'\x08',
                Some(b'c') => {
                    found_c = true;
                    break;
                }
                Some(b'f') => b'\x0c',
                Some(b'n') => b'\n',
                Some(b'r') => b'\r',
                Some(b't') => b'\t',
                Some(b'v') => b'\x0b',
                Some(b'\\') => b'\\',
                Some(b'0') => {
                    let mut buffer = [0; 3];
                    let mut found = 0;
                    for num_byte in &mut buffer {
                        match iter.clone().next() {
                            Some(&byte @ b'0'...b'7') => *num_byte = byte,
                            _ => break
                        };
                        found += 1;
                        iter.next();
                    }
                    if found > 0 {
                        // this is guaranteed to be valid, but it's unlikely the slight performance
                        // boost gained by using unsafe will make a difference
                        let num = u8::from_str_radix(str::from_utf8(&buffer[..found])?, 8)?;
                        num
                    } else {
                        b'\0'
                    }
                }
                Some(&other) => {
                    output.write_all(&[b'\\', other])?;
                    continue;
                }
                None => b'\\',
            }
            _ => byte,
        };
        output.write_all(&[out_byte])?;
    }

    Ok(found_c)
}
