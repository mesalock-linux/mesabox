//
// Copyright (c) 2018, The MesaLock Linux Project Contributors
// All rights reserved.
// 
// This work is licensed under the terms of the BSD 3-Clause License.
// For a copy, see the LICENSE file.
//
// This file incorporates work covered by the following copyright and
// permission notice:
//
//     Copyright (c) 2013-2018, Jordi Boggiano
//     Copyright (c) 2013-2018, Alex Lyon
//
//     Permission is hereby granted, free of charge, to any person obtaining a
//     copy of this software and associated documentation files (the
//     "Software"), to deal in the Software without restriction, including
//     without limitation the rights to use, copy, modify, merge, publish,
//     distribute, sublicense, and/or sell copies of the Software, and to
//     permit persons to whom the Software is furnished to do so, subject to
//     the following conditions:
//
//     The above copyright notice and this permission notice shall be included
//     in all copies or substantial portions of the Software.
//
//     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
//     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
//     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
//     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
//     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
//     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
//     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
//

use super::{UtilSetup, Result, ArgsIter, UtilRead, UtilWrite};

use clap::Arg;
use std::borrow::Cow;
use std::ffi::OsString;
use std::io;

pub(crate) const DESCRIPTION: &str = "Repeatedly print 'y' or a series of user-provided strings to stdout";

// it's possible that using a smaller or larger buffer might provide better performance on some
// systems, but this is probably good enough
const BUF_SIZE: usize = 16 * 1024;

pub fn execute<I, O, E, T, U>(setup: &mut UtilSetup<I, O, E>, args: ArgsIter<T, U>) -> Result<()>
where
    I: UtilRead,
    O: UtilWrite,
    E: UtilWrite,
    T: Iterator<Item = U>,
    U: Into<OsString> + Clone,
{
    let mut app = util_app!("yes").arg(Arg::with_name("STRING").index(1).multiple(true));

    let matches = get_matches!(setup, app, args);

    let string = if let Some(values) = matches.values_of("STRING") {
        let mut result = values.fold(String::new(), |res, s| res + s + " ");
        result.pop();
        result.push('\n');
        Cow::from(result)
    } else {
        Cow::from("y\n")
    };

    let mut buffer = [0; BUF_SIZE];
    let bytes = prepare_buffer(&string, &mut buffer);

    run(setup, bytes)?;

    Ok(())
}

#[cfg(not(feature = "latency"))]
fn prepare_buffer<'a>(input: &'a str, buffer: &'a mut [u8; BUF_SIZE]) -> &'a [u8] {
    if input.len() < BUF_SIZE / 2 {
        let mut size = 0;
        while size < BUF_SIZE - input.len() {
            let (_, right) = buffer.split_at_mut(size);
            right[..input.len()].copy_from_slice(input.as_bytes());
            size += input.len();
        }
        &buffer[..size]
    } else {
        input.as_bytes()
    }
}

#[cfg(feature = "latency")]
fn prepare_buffer<'a>(input: &'a str, _buffer: &'a mut [u8; BUF_SIZE]) -> &'a [u8] {
    input.as_bytes()
}

pub fn run<I, O, E>(setup: &mut UtilSetup<I, O, E>, bytes: &[u8]) -> io::Result<()>
where
    I: UtilRead,
    O: UtilWrite,
    E: UtilWrite,
{
    loop {
        setup.stdout.write_all(bytes)?;
    }
}
