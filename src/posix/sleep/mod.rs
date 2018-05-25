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

use super::{Result, UtilRead, UtilWrite, UtilSetup, string_to_err};

use std::ffi::OsString;
use std::thread;
use std::time::Duration;

use clap::Arg;
use uucore;

pub(crate) const DESCRIPTION: &str = "Pause for a given number of seconds";

pub fn execute<I, O, E, T, U>(setup: &mut UtilSetup<I, O, E>, args: T) -> Result<()>
where
    I: for<'a> UtilRead<'a>,
    O: for<'a> UtilWrite<'a>,
    E: for<'a> UtilWrite<'a>,
    T: Iterator<Item = U>,
    U: Into<OsString> + Clone,
{
    let app = util_app!("sleep", setup).arg(Arg::with_name("NUMBER[SUFFIX]").index(1).multiple(true).required(true).help("hi"));

    let matches = app.get_matches_from_safe(args)?;

    // XXX: should this fail on invalid bytes?
    let args = matches.values_of_lossy("NUMBER[SUFFIX]").unwrap();

    sleep(args)
}

fn sleep<T: IntoIterator<Item = String>>(args: T) -> Result<()> {
    let mut sleep_dur = Duration::new(0, 0);
    for arg in args {
        sleep_dur += string_to_err(uucore::parse_time::from_str(&arg[..]))?;
    }

    thread::sleep(sleep_dur);

    Ok(())
}
