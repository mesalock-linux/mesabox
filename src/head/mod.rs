//
// Copyright (c) 2018, The MesaLock Linux Project Contributors
// All rights reserved.
// 
// This work is licensed under the terms of the BSD 3-Clause License.
// For a copy, see the LICENSE file.
//

use super::{Result, UtilRead, UtilWrite, UtilSetup};
use clap::{Arg, AppSettings};
use std::ffi::{OsString, OsStr};
use std::fs::File;
use std::io::{self, BufReader, BufRead, Read, Write};
use std::result::Result as StdResult;
use std::str::FromStr;
use std::path::Path;

pub const NAME: &str = "head";
pub const DESCRIPTION: &str = "Read the first N bytes or lines from a file";

enum Mode {
    Bytes(isize),
    Lines(isize),
}

struct Options {
    method: Mode,
    verbose: bool,
    quiet: bool,
    previous_printed: bool,
}

pub fn execute<I, O, E, T, U>(setup: &mut UtilSetup<I, O, E>, args: T) -> Result<()>
where
    I: for<'a> UtilRead<'a>,
    O: for<'a> UtilWrite<'a>,
    E: for<'a> UtilWrite<'a>,
    T: Iterator<Item = U>,
    U: Into<OsString> + Clone,
{
    // TODO: check for obsolete arg style
    let mut app = util_app!("head")
                    .setting(AppSettings::AllowNegativeNumbers)
                    .arg(Arg::with_name("bytes")
                            .short("c")
                            .long("bytes")
                            .takes_value(true)
                            .value_name("NUMBER")
                            .validator_os(is_valid_num))
                    .arg(Arg::with_name("lines")
                            .short("n")
                            .long("lines")
                            .takes_value(true)
                            .value_name("NUMBER")
                            .validator_os(is_valid_num)
                            .conflicts_with("bytes"))
                    .arg(Arg::with_name("quiet")
                            .short("q")
                            .long("quiet")
                            /* TODO: add silent */)
                            // TODO: i assume quiet and verbose override but ensure they don't conflict
                    .arg(Arg::with_name("verbose")
                            .short("v")
                            .long("verbose"))
                    .arg(Arg::with_name("FILES")
                            .index(1)
                            .multiple(true));
    
    let matches = get_matches!(setup, app, args);

    let verbose = matches.is_present("verbose");
    let quiet = matches.is_present("quiet");

    let method = if matches.is_present("bytes") {
        Mode::Bytes(parse_num(matches.value_of("bytes").unwrap()))
    } else if matches.is_present("lines") {
        Mode::Lines(parse_num(matches.value_of("lines").unwrap()))
    } else {
        // just dump the first ten lines
        Mode::Lines(10)
    };

    // TODO: probably just move verbose/quiet into an enum
    let mut options = Options {
        method: method,
        verbose: verbose,
        quiet: quiet,
        previous_printed: false,
    };

    let mut output = setup.stdout.lock_writer()?;
    if matches.is_present("FILES") {
        let mut result = Ok(());
        let mut err_stream = setup.stderr.lock_writer()?;

        let file_count = matches.occurrences_of("FILES");

        for file in matches.values_of_os("FILES").unwrap() {
            let filename = if file_count > 1 {
                Some(file)
            } else {
                None
            };
            let res = if file == OsStr::new("-") {
                handle_stdin(&mut output, &mut setup.stdin, filename, &mut options)
            } else {
                handle_file(&mut output, file, filename, &mut options)
            };

            if let Err(mut e) = res {
                display_msg!(err_stream, "{}", e)?;
                e.err = None;
                result = Err(e);
            }
        }

        result
    } else {
        handle_stdin(output, &mut setup.stdin, None, &mut options)
    }
}

fn handle_stdin<I, O>(output: O, stdin: &mut I, filename: Option<&OsStr>, options: &mut Options) -> Result<()>
where
    I: for<'a> UtilRead<'a>,
    O: Write,
{
    let stdin = stdin.lock_reader()?;
    handle_data(output, stdin, filename, options)
}

fn handle_file<O: Write>(output: O, filename: &OsStr, disp_filename: Option<&OsStr>, options: &mut Options) -> Result<()> {
    let file = File::open(filename)?;
    let reader = BufReader::new(file);
    handle_data(output, reader, disp_filename, options)
}

// FIXME: can only seek if R: implements it
fn handle_data<W, R>(mut output: W, mut input: R, filename: Option<&OsStr>, options: &mut Options) -> Result<()>
where
    W: Write,
    R: BufRead,
{
    if let Some(name) = filename {
        let path = Path::new(name);
        if options.previous_printed {
            writeln!(output, "\n==> {} <==", path.display())?;
        } else {
            writeln!(output, "==> {} <==", path.display())?;
            options.previous_printed = true;
        }
    }
    match options.method {
        Mode::Lines(mut lines) => {
            // NOTE: need to keep reading until we hit "lines" lines, so slow
            if lines >= 0 {
                let mut buffer = vec![];
                while lines > 0 {
                    // NOTE: it would be faster to just continuously read into the buffer and then
                    //       write once, but that could potentially take a lot of memory
                    let count = input.read_until(b'\n', &mut buffer)?;
                    if count == 0 {
                        break;
                    }
                    output.write_all(&buffer)?;

                    buffer.clear();
                    lines -= 1;
                }
            } else {
                // TODO: negative number
                // NOTE: we might need to seek
            }
        }
        Mode::Bytes(bytes) => {
            if bytes >= 0 {
                io::copy(&mut input.take(bytes as u64), &mut output)?;
            } else {
                // TODO: negative number
                // NOTE: we need to seek from the end, so R needs to implement Seek or we need to emulate it
            }
        }
    }

    Ok(())
}

// FIXME: need to add suffixes
fn parse_num(s: &str) -> isize {
    isize::from_str(s).unwrap()
}

// FIXME: need to add suffixes
fn is_valid_num(val: &OsStr) -> StdResult<(), OsString> {
    if val.to_str().and_then(|s| isize::from_str(s).ok()).is_some() {
        Ok(())
    } else {
        Err(OsString::from(format!("'{}' is not a number", val.to_string_lossy())))
    }
}
