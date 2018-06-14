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
//     Copyright (c) 2013,      Jordy Dickinson
//     Copyright (c) 2016,      Jian Zeng
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

use ::{Result, UtilRead, UtilWrite, UtilSetup};
use util;

use clap::Arg;
use uucore::encoding::{self, Data, Format};

use std::str::FromStr;
use std::ffi::{OsStr, OsString};
use std::fs::File;
use std::io::{BufReader, Read, Write};
use std::result::Result as StdResult;

struct Options {
    line_wrap: usize,
    decode: bool,
    ignore_garbage: bool,
    format: Format,
}

pub(crate) fn execute_base<I, O, E, T, U>(setup: &mut UtilSetup<I, O, E>, args: T, name: &str, desc: &str, format: Format) -> Result<()>
where
    I: for<'a> UtilRead<'a>,
    O: for<'a> UtilWrite<'a>,
    E: for<'a> UtilWrite<'a>,
    T: Iterator<Item = U>,
    U: Into<OsString> + Clone,
{
    let matches = {
        let app = util_app!(name, setup, desc)
                    .arg(Arg::with_name("decode")
                            .short("d")
                            .long("decode")
                            .help("decode rather than encode input data"))
                    .arg(Arg::with_name("ignore-garbage")
                            .short("i")
                            .long("ignore-garbage")
                            .help("when decoding, ignore non-alphabetic characters"))
                    .arg(Arg::with_name("wrap")
                            .short("w")
                            .long("wrap")
                            .takes_value(true)
                            .value_name("COLS")
                            .validator_os(is_valid_wrap)
                            .help("wrap encoded lines after COLS characters (the default is 76 and 0 disables wrapping)"))
                    .arg(Arg::with_name("FILE")
                            .index(1));

        app.get_matches_from_safe(args)?
    };

    let line_wrap = match matches.value_of("wrap") {
        Some(s) => usize::from_str(s).unwrap(),
        None => 76,
    };

    let decode = matches.is_present("decode");

    let ignore_garbage = matches.is_present("ignore-garbage");

    let options = Options {
        line_wrap: line_wrap,
        decode: decode,
        ignore_garbage: ignore_garbage,
        format: format,
    };

    let output = setup.stdout.lock_writer()?;
    match matches.value_of_os("FILE") {
        Some(filename) if filename != OsStr::new("-") => {
            let path = util::actual_path(&setup.current_dir, filename);
            let file = File::open(path)?;
            handle_data(output, BufReader::new(file), options)
        }
        _ => {
            let input = setup.stdin.lock_reader()?;
            handle_data(output, input, options)
        }
    }
}

fn handle_data<W, S>(mut output: W, source: S, options: Options) -> Result<()>
where
    W: Write,
    S: Read,
{
    let mut data = Data::new(source, options.format)
        .line_wrap(options.line_wrap)
        .ignore_garbage(options.ignore_garbage);

    if options.decode {
        let s = data.decode()?;
        output.write_all(&s)?;
    } else {
        encoding::wrap_write(output, options.line_wrap, data.encode())?;
    }

    Ok(())
}

fn is_valid_wrap(val: &OsStr) -> StdResult<(), OsString> {
    if val.to_str().and_then(|s| usize::from_str(s).ok()).is_some() {
        Ok(())
    } else {
        Err(OsString::from(format!("'{}' is not a number", val.to_string_lossy())))
    }
}
