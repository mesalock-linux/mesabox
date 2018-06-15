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

use super::{MesaError, UtilSetup, Result, ArgsIter, UtilWrite};
use util;

use clap::{Arg, ArgGroup, AppSettings, OsValues};
use regex::Regex;
use std::ffi::{OsStr, OsString};
use std::fs;
use std::io::{self, Write};
use std::os::unix::fs::{MetadataExt, PermissionsExt};
use std::path::Path;
use std::result::Result as StdResult;
use walkdir::WalkDir;
use uucore::mode;
use uucore::fs::display_permissions_unix;

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

pub fn execute<S, T>(setup: &mut S, args: T) -> Result<()>
where
    S: UtilSetup,
    T: ArgsIter,
{
    let matches = {
        let app = util_app!(NAME, setup)
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
        Some(ref_file) => Some(fs::metadata(ref_file).map(|data| data.mode()).map_err(|e| ChmodError::Stat(ref_file.to_string_lossy().into(), e))?),
        None => None,
    };

    let mut stdout = setup.output();
    let mut stderr = setup.error();
    let mut chmoder = Chmoder {
        verbosity: verbosity,
        preserve_root: preserve_root,
        recursive: recursive,
        fmode: fmode,
        cmode: matches.value_of("MODE"),
        stdout: stdout.lock_writer()?,
        stderr: stderr.lock_writer()?,
        current_dir: setup.current_dir(),
    };

    let exitcode = chmoder.chmod(matches.values_of_os("FILES").unwrap())?;

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
    let re = Regex::new("[ugoa]*([-+=]([rwxXst]*|[ugo]))+|[-+=][0-7]+|[0-7]+").expect("invalid regex");

    arg.to_str()
        .ok_or_else(|| "mode was not a string (must be encoded using UTF-8)".into())
        .and_then(|s| {
            let mut valid = true;
            for mode in s.split(',') {
                if mode.len() <= 1 || !re.is_match(mode) {
                    valid = false;
                    break;
                }
            }
            if valid && s.len() > 1 {
                Ok(())
            } else {
                Err("found invalid character in mode string".into())
            }
        })
}

struct Chmoder<'a, O, E>
where
    O: Write,
    E: Write,
{
    verbosity: Verbosity,
    preserve_root: bool,
    recursive: bool,
    fmode: Option<u32>,
    cmode: Option<&'a str>,
    stdout: O,
    stderr: E,
    current_dir: Option<&'a Path>,
}

impl<'a, O, E> Chmoder<'a, O, E>
where
    O: Write,
    E: Write,
{
    fn chmod<'b>(&mut self, files: OsValues<'b>) -> Result<i32> {
        let mut r = 0;

        for filename in files {
            let file = util::actual_path(&self.current_dir, filename);

            r &= if file.is_dir() && self.recursive {
                self.chmod_dir(&file)
            } else {
                self.chmod_file(&file)
            }?;
        }

        Ok(r)
    }

    fn chmod_dir(&mut self, file: &Path) -> Result<i32> {
        let mut r = 0;

        if !self.preserve_root || file != Path::new("/") {
            let walker = WalkDir::new(file).contents_first(true);
            for entry in walker {
                match entry {
                    Ok(entry) => r &= self.chmod_file(&entry.path())?,
                    Err(f) => writeln!(self.stderr, "{}", f)?,
                }
            }
        } else {
            display_err!(self.stderr, "could not change permissions of directory '{}'", file.display())?;
            r = 1;
        }

        Ok(r)
    }

    #[cfg(any(unix, target_os = "redox"))]
    fn chmod_file(&mut self, file: &Path) -> Result<i32> {
        let mut fperm = match fs::metadata(file) {
            Ok(meta) => meta.mode() & 0o7777,
            Err(err) => {
                if self.verbosity != Verbosity::Quiet {
                    display_err!(self.stderr, "could not stat '{}': {}", file.display(), err)?;
                }
                return Ok(1);
            }
        };
        match self.fmode {
            Some(mode) => self.change_file(fperm, mode, file),
            None => {
                let cmode_unwrapped = self.cmode.clone().unwrap();
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
                            self.change_file(fperm, mode, file)?;
                            fperm = mode;
                        }
                        Err(f) => {
                            if self.verbosity != Verbosity::Quiet {
                                display_err!(self.stderr, "failed to parse mode: {}", f)?;
                            }
                            return Ok(1);
                        }
                    }
                }
                Ok(0)
            }
        }
    }

    #[cfg(unix)]
    fn change_file(&mut self, fperm: u32, mode: u32, file: &Path) -> Result<i32> {
        if fperm == mode {
            if self.verbosity == Verbosity::Verbose {
                display_msg!(self.stdout, "mode of '{}' retained as {:o} ({})", file.display(), fperm, display_permissions_unix(fperm))?;
            }
            return Ok(0);
        }
        
        let res = fs::set_permissions(file, fs::Permissions::from_mode(mode));
        if let Err(err) = res {
            if self.verbosity != Verbosity::Quiet {
                display_err!(self.stderr, "could not set permissions: {}", err)?;
            }
            if self.verbosity == Verbosity::Verbose {
                display_msg!(
                    self.stdout,
                    "failed to change mode of file '{}' from {:o} ({}) to {:o} ({})",
                    file.display(),
                    fperm,
                    display_permissions_unix(fperm),
                    mode,
                    display_permissions_unix(mode)
                )?;
            }
            Ok(1)
        } else {
            if self.verbosity == Verbosity::Verbose || self.verbosity == Verbosity::Changes {
                display_msg!(
                    self.stdout,
                    "mode of '{}' changed from {:o} ({}) to {:o} ({})",
                    file.display(),
                    fperm,
                    display_permissions_unix(fperm),
                    mode,
                    display_permissions_unix(mode)
                )?;
            }
            Ok(0)
        }
    }
}
