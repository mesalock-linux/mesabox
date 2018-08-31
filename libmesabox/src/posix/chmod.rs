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

use util;
use {ArgsIter, MesaError, Result, UtilSetup, UtilWrite};

use clap::{AppSettings, Arg, ArgGroup, OsValues};
use std::ffi::{OsStr, OsString};
use std::fs;
use std::io::{self, Write};
use std::os::unix::fs::{MetadataExt, PermissionsExt};
use std::path::{Path, PathBuf};
use std::result::Result as StdResult;
use uucore::fs::display_permissions_unix;
use uucore::mode;
use walkdir::WalkDir;

const NAME: &str = "chmod";
pub(crate) const DESCRIPTION: &str = "Change the file permissions of given files";
const MODE_SYNTAX: &str = "
 Each MODE is of the form '[ugoa]*([-+=]([rwxXst]*|[ugo]))+|[-+=]?[0-7]+'.
";

#[derive(Fail, Debug)]
enum ChmodError {
    #[fail(display = "cannot stat attributes of '{}': {}", _0, _1)]
    Stat(String, #[cause] io::Error),
}

#[derive(PartialEq)]
enum Verbosity {
    None,
    Changes,
    Quiet,
    Verbose,
}

enum MessageKind {
    Stdout,
    Stderr,
}

// FIXME: find a better way to store this (preferably avoid allocating)
// NOTE: the message setup is to avoid duplicating chmod_file() and change_file() for every generic
//       type
struct Message {
    kind: MessageKind,
    data: String,
}

impl Message {
    pub fn stdout(data: String) -> Self {
        Self {
            kind: MessageKind::Stdout,
            data,
        }
    }

    pub fn stderr(data: String) -> Self {
        Self {
            kind: MessageKind::Stderr,
            data,
        }
    }
}

struct ChmodOptions<'a> {
    verbosity: Verbosity,
    preserve_root: bool,
    recursive: bool,
    fmode: Option<u32>,
    cmode: Option<&'a str>,
    current_dir: Option<PathBuf>,
}

pub fn execute<S, T>(setup: &mut S, args: T) -> Result<()>
where
    S: UtilSetup,
    T: ArgsIter,
{
    let matches = {
        let app = util_app!(NAME)
                    .after_help(MODE_SYNTAX)
                    .setting(AppSettings::AllowLeadingHyphen)
                    .arg(Arg::with_name("recursive")
                            .long("recursive")
                            .short("R")
                            .help("change files and directories recursively"))
                    .arg(Arg::with_name("reference")
                            .long("reference")
                            .takes_value(true)
                            .value_name("RFILE")
                            .help("use RFILE's mode instead of provided MODE values"))
                    .arg(Arg::with_name("preserve-root")
                            .long("preserve-root")
                            .help("fail to operate recursively on '/'"))
                    .arg(Arg::with_name("no-preserve-root")
                            .long("no-preserve-root")
                            .overrides_with("preserve-root")
                            .help("do not treat '/' specially (the default)"))
                    .arg(Arg::with_name("verbose")
                            .long("verbose")
                            .short("v")
                            .help("output a diagnostic for every file processed"))
                    .arg(Arg::with_name("quiet")
                            .long("quiet")
                            .short("f")
                            .visible_alias("silent")
                            .help("suppress most error messages"))
                    .arg(Arg::with_name("changes")
                            .long("changes")
                            .short("c")
                            .help("like verbose but report only when a change is made"))
                    .group(ArgGroup::with_name("verbosity")
                            .args(&["verbose", "quiet", "changes"]))
                    // FIXME: not sure how to tell clap that MODE can be missing if --reference is
                    //        given by the user.  clap is also unhappy that FILES (which has an
                    //        index that occurs later than MODE) is required while MODE is not
                    .arg(Arg::with_name("MODE")
                            .index(1)
                            .validator_os(validate_mode)
                            .required(true))
                            //.conflicts_with("reference"))
                    .arg(Arg::with_name("FILES")
                            .index(2)
                            .required(true)
                            .multiple(true));

        app.get_matches_from_safe(args)?
    };

    let verbosity = if matches.is_present("changes") {
        Verbosity::Changes
    } else if matches.is_present("quiet") {
        Verbosity::Quiet
    } else if matches.is_present("verbose") {
        Verbosity::Verbose
    } else {
        Verbosity::None
    };

    let preserve_root = matches.is_present("preserve-root");

    let recursive = matches.is_present("recursive");
    let fmode = match matches.value_of_os("reference") {
        Some(ref_file) => Some(fs::metadata(ref_file)
            .map(|data| data.mode())
            .map_err(|e| ChmodError::Stat(ref_file.to_string_lossy().into(), e))?),
        None => None,
    };

    let current_dir = setup.current_dir().map(|p| p.to_owned());
    let (_, stdout, stderr) = setup.stdio();
    let mut chmoder = Chmoder {
        stdout: stdout.lock()?,
        stderr: stderr.lock()?,
    };

    let options = ChmodOptions {
        verbosity: verbosity,
        preserve_root: preserve_root,
        recursive: recursive,
        fmode: fmode,
        cmode: matches.value_of("MODE"),
        current_dir: current_dir,
    };

    let exitcode = chmoder.chmod(&options, matches.values_of_os("FILES").unwrap())?;

    if exitcode == 0 {
        Ok(())
    } else {
        Err(MesaError {
            exitcode: exitcode,
            progname: None,
            err: None,
        })
    }
}

fn validate_mode(arg: &OsStr) -> StdResult<(), OsString> {
    // NOTE: used to use regex to match the mode, but that caused the binary size to increase
    //       considerably
    arg.to_str()
        .ok_or_else(|| "mode was not a string (must be encoded using UTF-8)".into())
        .and_then(|s| {
            for mode in s.split(',') {
                if mode::parse_numeric(0, mode).is_err()
                    && mode::parse_symbolic(0, mode, false).is_err()
                {
                    return Err("found invalid character in mode string".into());
                }
            }
            Ok(())
        })
}

struct Chmoder<O, E>
where
    O: Write,
    E: Write,
{
    stdout: O,
    stderr: E,
}

impl<'a, O, E> Chmoder<O, E>
where
    O: Write,
    E: Write,
{
    fn chmod<'b>(&mut self, options: &ChmodOptions, files: OsValues<'b>) -> Result<i32> {
        let mut r = 0;

        let mut msgs = [None, None];

        for filename in files {
            let file = util::actual_path(&options.current_dir, filename);

            r |= if file.is_dir() && options.recursive {
                self.chmod_dir(options, &mut msgs, &file)
            } else {
                let res = chmod_file(options, &mut msgs, &file);
                self.write_msgs(&mut msgs).map(|_| res)
            }?;
        }

        Ok(r)
    }

    fn chmod_dir(
        &mut self,
        options: &ChmodOptions,
        msgs: &mut [Option<Message>; 2],
        file: &Path,
    ) -> Result<i32> {
        let mut r = 0;

        if !options.preserve_root || file != Path::new("/") {
            let walker = WalkDir::new(file).contents_first(true);
            for entry in walker {
                match entry {
                    Ok(entry) => {
                        r |= chmod_file(options, msgs, &entry.path());
                        self.write_msgs(msgs)?;
                    }
                    Err(f) => display_msg!(self.stderr, "{}", f)?,
                }
            }
        } else {
            display_msg!(
                self.stderr,
                "could not change permissions of directory '{}'",
                file.display()
            )?;
            r = 1;
        }

        Ok(r)
    }

    fn write_msgs(&mut self, msgs: &mut [Option<Message>; 2]) -> Result<()> {
        for msg in msgs {
            if let Some(msg) = msg {
                match msg.kind {
                    MessageKind::Stdout => display_msg!(self.stdout, "{}", msg.data)?,
                    MessageKind::Stderr => display_msg!(self.stderr, "{}", msg.data)?,
                }
            }
            *msg = None;
        }
        Ok(())
    }
}

#[cfg(any(unix, target_os = "redox"))]
fn chmod_file(options: &ChmodOptions, msgs: &mut [Option<Message>; 2], file: &Path) -> i32 {
    let mut fperm = match fs::metadata(file) {
        Ok(meta) => meta.mode() & 0o7777,
        Err(err) => {
            if options.verbosity != Verbosity::Quiet {
                msgs[0] = Some(Message::stderr(format!(
                    "could not stat '{}': {}",
                    file.display(),
                    err
                )));
            }
            return 1;
        }
    };
    match options.fmode {
        Some(mode) => change_file(options, msgs, fperm, mode, file),
        None => {
            let cmode_unwrapped = options.cmode.clone().unwrap();
            for mode in cmode_unwrapped.split(',') {
                // cmode is guaranteed to be Some in this case
                let arr: &[char] = &['0', '1', '2', '3', '4', '5', '6', '7'];
                let result = if mode.contains(arr) {
                    mode::parse_numeric(fperm, mode)
                } else {
                    mode::parse_symbolic(fperm, mode, file.is_dir())
                };
                match result {
                    Ok(mode) => {
                        change_file(options, msgs, fperm, mode, file);
                        fperm = mode;
                    }
                    Err(f) => {
                        if options.verbosity != Verbosity::Quiet {
                            msgs[0] = Some(Message::stderr(format!("failed to parse mode: {}", f)));
                        }
                        return 1;
                    }
                }
            }
            0
        }
    }
}

#[cfg(unix)]
fn change_file(
    options: &ChmodOptions,
    msgs: &mut [Option<Message>; 2],
    fperm: u32,
    mode: u32,
    file: &Path,
) -> i32 {
    if fperm == mode {
        if options.verbosity == Verbosity::Verbose {
            msgs[0] = Some(Message::stdout(format!(
                "mode of '{}' retained as {:o} ({})",
                file.display(),
                fperm,
                display_permissions_unix(fperm)
            )));
        }
        return 0;
    }

    let mut exitcode = 0;

    let res = fs::set_permissions(file, fs::Permissions::from_mode(mode));
    if let Err(err) = res {
        let mut count = 0;
        if options.verbosity != Verbosity::Quiet {
            msgs[0] = Some(Message::stderr(format!(
                "could not set permissions: {}",
                err
            )));
            count += 1;
        }
        if options.verbosity == Verbosity::Verbose {
            msgs[count] = Some(Message::stdout(format!(
                "failed to change mode of file '{}' from {:o} ({}) to {:o} ({})",
                file.display(),
                fperm,
                display_permissions_unix(fperm),
                mode,
                display_permissions_unix(mode)
            )));
        }
        exitcode = 1;
    } else {
        if options.verbosity == Verbosity::Verbose || options.verbosity == Verbosity::Changes {
            msgs[0] = Some(Message::stdout(format!(
                "mode of '{}' changed from {:o} ({}) to {:o} ({})",
                file.display(),
                fperm,
                display_permissions_unix(fperm),
                mode,
                display_permissions_unix(mode)
            )));
        }
    }

    exitcode
}
