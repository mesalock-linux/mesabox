//
// Copyright (c) 2018, The MesaLock Linux Project Contributors
// All rights reserved.
// 
// This work is licensed under the terms of the BSD 3-Clause License.
// For a copy, see the LICENSE file.
//

use {ArgsIter, MesaError, Result, UtilSetup, UtilWrite};

use std::ffi::OsStr;
use std::io::Write;
use std::iter;
use std::str;

use util::OsStrExt;

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
    let mut output = output.lock()?;

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

    for res in map_bytes(s.try_as_bytes()?) {
        match res {
            ByteResult::Stop => {
                // found \c
                found_c = true;
                break;
            }
            ByteResult::Slice(slice) => output.write_all(slice)?,
            ByteResult::Escape(byte, slice) => {
                output.write_all(&[byte])?;
                output.write_all(slice)?
            }
            ByteResult::Num(num, slice) => {
                output.write_all(&[num?])?;
                output.write_all(slice)?
            }
            ByteResult::Backslash => output.write_all(&[b'\\'])?,
            ByteResult::None => {}
        }
    }

    Ok(found_c)
}

enum ByteResult<'a> {
    None,
    Escape(u8, &'a [u8]),
    Num(Result<u8>, &'a [u8]),
    Slice(&'a [u8]),
    Backslash,
    Stop,
}

fn map_bytes(s: &[u8]) -> impl Iterator<Item = ByteResult> {
    let mut it = s.split(|&byte| byte == b'\\');
    let first = it.next().unwrap();

    let scanner = it.scan(false, |found_backslash, data| {
        let res = if data.len() == 0 {
            let res = if !*found_backslash {
                ByteResult::Backslash
            } else {
                // XXX: might want to return None, but then we'd have to check None result twice in
                //      write_str()
                ByteResult::None
            };
            *found_backslash = !*found_backslash;
            res
        } else {
            // the leading character is not a backslash
            match data[0] {
                b'a' => ByteResult::Escape(b'\x07', &data[1..]),
                b'b' => ByteResult::Escape(b'\x08', &data[1..]),
                b'c' => ByteResult::Stop,
                b'f' => ByteResult::Escape(b'\x0c', &data[1..]),
                b'n' => ByteResult::Escape(b'\n', &data[1..]),
                b'r' => ByteResult::Escape(b'\r', &data[1..]),
                b't' => ByteResult::Escape(b'\t', &data[1..]),
                b'v' => ByteResult::Escape(b'\x0b', &data[1..]),
                b'\\' => ByteResult::Escape(b'\\', &data[1..]),
                b'0' => {
                    let mut buffer = [0; 3];
                    let mut found = 0;
                    for (num_byte, &byte) in buffer.iter_mut().zip(data.iter().skip(1).take(3)) {
                        match byte {
                            b'0'...b'7' => *num_byte = byte,
                            _ => break,
                        }
                        found += 1;
                    }
                    let num = if found > 0 {
                        // this is guaranteed to be valid, but it's unlikely the slight performance
                        // boost gained by using unsafe will make a difference
                        str::from_utf8(&buffer[..found]).map_err(|e| {
                            let err: MesaError = e.into();
                            err
                        }).and_then(|s| u8::from_str_radix(s, 8).map_err(|e| e.into()))
                    } else {
                        Ok(b'\0')
                    };
                    ByteResult::Num(num, &data[found + 1..])
                }
                _ => ByteResult::Escape(b'\\', data),
            }
        };
        Some(res)
    });

    iter::once(ByteResult::Slice(first)).chain(scanner)
}
