//
// Copyright (c) 2018, The MesaLock Linux Project Contributors
// All rights reserved.
//
// This work is licensed under the terms of the BSD 3-Clause License.
// For a copy, see the LICENSE file.
//

#[macro_use]
extern crate clap;
extern crate failure;
#[macro_use]
extern crate failure_derive;
extern crate libc;

#[cfg(windows)]
extern crate kernel32;
#[cfg(unix)]
extern crate nix;
#[cfg(windows)]
extern crate winapi;

#[cfg(feature = "byteorder")]
extern crate byteorder;
#[cfg(feature = "chrono")]
extern crate chrono;
#[cfg(feature = "crossbeam")]
extern crate crossbeam;
#[cfg(feature = "either")]
extern crate either;
#[cfg(feature = "fnv")]
extern crate fnv;
#[cfg(feature = "globset")]
extern crate globset;
#[cfg(feature = "mio")]
extern crate mio;
#[cfg(feature = "pnet")]
extern crate pnet;
#[cfg(feature = "regex")]
extern crate regex;
#[cfg(feature = "socket2")]
extern crate socket2;
#[cfg(feature = "trust-dns-resolver")]
extern crate trust_dns_resolver;
#[cfg(feature = "uucore")]
extern crate uucore;
#[cfg(feature = "walkdir")]
extern crate walkdir;
#[cfg(feature = "nom")]
#[macro_use]
extern crate nom;
#[cfg(feature = "glob")]
extern crate glob;
#[cfg(feature = "rustyline")]
extern crate rustyline;

use clap::{App, SubCommand};
use libc::EXIT_FAILURE;
use std::env::{self, VarsOs};
use std::ffi::{OsStr, OsString};
use std::io::{self, BufRead, Read, Stderr, Stdin, Stdout, Write};
use std::iter;
use std::path::{Path, PathBuf};
use std::result::Result as StdResult;

pub use error::*;
pub(crate) use platform::*;
pub use setup::*;
#[allow(unused)]
pub(crate) use util::*;

mod error;
#[macro_use]
#[allow(unused_macros)]
mod macros;
mod platform;
mod setup;
#[allow(dead_code)]
mod util;

macro_rules! generate_fns {
    ($($group:ident { $(($util:ident, $feature:expr)),+ }),*) => {
        generate_fns!(import $($group { $(($util, $feature)),+ }),*);
        generate_fns!(dump_cmds $($group { $(($util, $feature)),+ }),*);
        generate_fns!(easy_util $($group { $(($util, $feature)),+ }),*);
        generate_fns!(execute_util $($group { $(($util, $feature)),+ }),*);
        generate_fns!(generate_app $($group { $(($util, $feature)),+ }),*);
    };

    (import $($group:ident { $(($util:ident, $feature:expr)),+ }),*) => {
        $(
            mod $group {
                $(
                    #[cfg(feature = $feature)]
                    pub mod $util;
                )+
            }
        )*
    };

    (dump_cmds $($group:ident { $(($util:ident, $feature:expr)),+ }),*) => {
        pub fn dump_commands<S>(setup: &mut S) -> Result<()>
        where
            S: UtilSetup,
        {
            let stdout = setup.output();
            let mut stdout = stdout.lock_writer()?;

            $($(
                #[cfg(feature = $feature)]
                {
                    writeln!(stdout, stringify!($util))?;
                }
            )+)*

            Ok(())
        }
    };

    (easy_util $($group:ident { $(($util:ident, $feature:expr)),+ }),*) => {
        $($(
            #[cfg(feature = $feature)]
            pub fn $util<T, U, V>(args: T) -> Result<()>
            where
                T: IntoIterator<IntoIter = V, Item = U>,
                U: Into<OsString> + AsRef<OsStr> + Clone,
                V: ArgsIter<ArgItem = U>,
            {
                let mut args = iter::once(OsString::from(stringify!($util))).chain(args.into_iter().map(|s| s.into()));
                EasyUtil::new().execute(&mut args, $group::$util::execute)
            }
        )+)*
    };

    (execute_util $($group:ident { $(($util:ident, $feature:expr)),+ }),*) => {
        fn execute_util<S, T>(setup: &mut S, name: &OsStr, args: T) -> Option<Result<()>>
        where
            S: UtilSetup,
            T: ArgsIter,
        {
            generate_fns!(execute_util_common setup, name, args, $($group { $(($util, $feature)),+ }),*)
        }

        #[cfg(feature = "sh")]
        fn execute_util_sh<S, T>(setup: &mut S, name: &OsStr, args: T) -> Option<Result<()>>
        where
            S: UtilSetup,
            T: ArgsIter,
        {
            if name == "sh" {
                None
            } else {
                generate_fns!(execute_util_common setup, name, args, $($group { $(($util, $feature)),+ }),*)
            }
        }

        #[cfg(feature = "sh")]
        fn util_exists(name: &str) -> bool {
            [
                $($(
                    #[cfg(feature = $feature)]
                    stringify!($util),
                )+)*
            ].contains(&name)
        }
    };

    (execute_util_common $setup:ident, $name:ident, $args:ident, $($group:ident { $(($util:ident, $feature:expr)),+ }),*) => {{
        let mut result = None;
        loop {
            $($(
                #[cfg(feature = $feature)]
                {
                    if $name == stringify!($util) {
                        result = Some($group::$util::execute($setup, $args));
                        break
                    }
                }
            )+)*
            if $name == "dump-cmds" {
                result = Some(dump_commands($setup))
            }
            break;
        }
        result
    }};

    (generate_app $($group:ident { $(($util:ident, $feature:expr)),+ }),*) => {
        // generate a clap::App such that the available utils are set up as subcommands without any
        // arguments (adding all the arguments would slow down startup time)
        fn generate_app() -> App<'static, 'static> {
            let mut app = app_from_crate!();
            $($(
                #[cfg(feature = $feature)]
                {
                    app = app.subcommand(SubCommand::with_name(stringify!($util)).about($group::$util::DESCRIPTION));
                }
            )+)*
            app.subcommand(SubCommand::with_name("dump-cmds").about("Print a list of commands in the binary"))
        }
    };
}

include!("util_list.rs");

pub struct UtilData<'b, 'c, 'd, I, O, E, T>
where
    I: for<'a> UtilRead<'a> + 'b,
    O: for<'a> UtilWrite<'a> + 'c,
    E: for<'a> UtilWrite<'a> + 'd,
    T: Iterator<Item = (OsString, OsString)>,
{
    pub stdin: &'b mut I,
    pub stdout: &'c mut O,
    pub stderr: &'d mut E,
    pub env: T,
    pub current_dir: Option<PathBuf>,
}

impl<'b, 'c, 'd, I, O, E, T> UtilData<'b, 'c, 'd, I, O, E, T>
where
    I: for<'a> UtilRead<'a>,
    O: for<'a> UtilWrite<'a>,
    E: for<'a> UtilWrite<'a>,
    T: Iterator<Item = (OsString, OsString)>,
{
    pub fn new(
        stdin: &'b mut I,
        stdout: &'c mut O,
        stderr: &'d mut E,
        env: T,
        current_dir: Option<PathBuf>,
    ) -> Self {
        Self {
            stdin: stdin,
            stdout: stdout,
            stderr: stderr,
            env: env,
            current_dir: current_dir,
        }
    }
}

pub trait UtilSetup {
    type Input: for<'a> UtilRead<'a>;
    type Output: for<'a> UtilWrite<'a>;
    type Error: for<'a> UtilWrite<'a>;
    type Env: Iterator<Item = (OsString, OsString)>;

    fn input<'a, 'b: 'a>(&'b mut self) -> &'a mut Self::Input;
    fn output<'a, 'b: 'a>(&'b mut self) -> &'a mut Self::Output;
    fn error<'a, 'b: 'a>(&'b mut self) -> &'a mut Self::Error;

    fn stdio<'a, 'b: 'a>(
        &'b mut self,
    ) -> (
        &'a mut Self::Input,
        &'a mut Self::Output,
        &'a mut Self::Error,
    );

    fn env(&mut self) -> &mut Self::Env;

    fn current_dir(&self) -> Option<&Path>;
}

impl<'b, 'c, 'd, I, O, E, T> UtilSetup for UtilData<'b, 'c, 'd, I, O, E, T>
where
    I: for<'a> UtilRead<'a>,
    O: for<'a> UtilWrite<'a>,
    E: for<'a> UtilWrite<'a>,
    T: Iterator<Item = (OsString, OsString)>,
{
    type Input = I;
    type Output = O;
    type Error = E;
    type Env = T;

    fn input<'a, 'e: 'a>(&'e mut self) -> &'a mut Self::Input {
        self.stdin
    }

    fn output<'a, 'e: 'a>(&'e mut self) -> &'a mut Self::Output {
        self.stdout
    }

    fn error<'a, 'e: 'a>(&'e mut self) -> &'a mut Self::Error {
        self.stderr
    }

    fn stdio<'a, 'e: 'a>(
        &'e mut self,
    ) -> (
        &'a mut Self::Input,
        &'a mut Self::Output,
        &'a mut Self::Error,
    ) {
        (self.stdin, self.stdout, self.stderr)
    }

    fn env(&mut self) -> &mut Self::Env {
        &mut self.env
    }

    fn current_dir(&self) -> Option<&Path> {
        self.current_dir.as_ref().map(|p| p.as_path())
    }
}

pub trait LockableRead<'a>: Read + Send + Sync {
    fn lock_reader_dyn<'b: 'a>(&'b mut self) -> StdResult<Box<BufRead + 'a>, LockError>;
}

pub trait LockableWrite<'a>: Write + Send + Sync {
    fn lock_writer_dyn<'b: 'a>(&'b mut self) -> StdResult<Box<Write + 'a>, LockError>;
}

pub trait UtilRead<'a>: LockableRead<'a> {
    type Lock: BufRead + 'a;

    fn lock_reader<'b: 'a>(&'b mut self) -> StdResult<Self::Lock, LockError>;

    fn raw_object(&self) -> Option<RawObject> {
        None
    }
}

pub trait UtilWrite<'a>: LockableWrite<'a> {
    type Lock: Write + 'a;

    fn lock_writer<'b: 'a>(&'b mut self) -> StdResult<Self::Lock, LockError>;

    fn raw_object(&self) -> Option<RawObject> {
        None
    }
}

pub trait ArgsIter: Iterator<Item = <Self as ArgsIter>::ArgItem> {
    type ArgItem: Into<OsString> + AsRef<OsStr> + Clone;
}

impl<'a, T: Into<OsString> + AsRef<OsStr> + Clone, U: Iterator<Item = T>> ArgsIter for &'a mut U {
    type ArgItem = T;
}

pub type Result<T> = StdResult<T, MesaError>;

// used by functions generated in build.rs for each utility (the functions allow a user to call
// utilities like cat(&mut ["file", "anotherfile"].iter()), although this could probably be
// simplified)
struct EasyUtil {
    stdin: Stdin,
    stdout: Stdout,
    stderr: Stderr,
}

impl EasyUtil {
    pub fn new() -> Self {
        Self {
            stdin: io::stdin(),
            stdout: io::stdout(),
            stderr: io::stderr(),
        }
    }

    pub fn execute<'a, T, U, V, F>(&'a mut self, args: T, func: F) -> Result<()>
    where
        T: IntoIterator<IntoIter = V, Item = U>,
        U: Into<OsString> + AsRef<OsStr> + Clone,
        V: ArgsIter<ArgItem = U>,
        F: Fn(&mut UtilData<'a, 'a, 'a, Stdin, Stdout, Stderr, VarsOs>, V) -> Result<()>,
    {
        let mut data = UtilData::new(
            &mut self.stdin,
            &mut self.stdout,
            &mut self.stderr,
            env::vars_os(),
            None,
        );
        func(&mut data, args.into_iter())
    }
}

pub fn execute<S, T, U, V>(setup: &mut S, args: T) -> Result<()>
where
    S: UtilSetup,
    T: IntoIterator<IntoIter = V, Item = U>,
    U: Into<OsString> + AsRef<OsStr> + Clone,
    V: ArgsIter<ArgItem = U>,
{
    let mut args = args.into_iter();

    // assume that we are using symlinks first (i.e. "command args" format).  if not, check for
    // "mesabox command args" format
    let res = start(setup, &mut args)
        .or_else(|| start(setup, &mut args))
        .or_else(|| {
            // no valid util was found, so just display a help menu
            let _ = generate_app().write_help(setup.error());
            let _ = writeln!(setup.error());

            Some(Err(MesaError::new(None, EXIT_FAILURE, None)))
        })
        .unwrap();

    let _ = setup.output().flush();
    let _ = setup.error().flush();

    res
}

fn start<S, T>(setup: &mut S, args: &mut T) -> Option<Result<()>>
where
    S: UtilSetup,
    T: ArgsIter,
{
    if let Some(progname) = args.next() {
        if let Some(filename) = Path::new(&progname.clone().into()).file_name() {
            // we pass along the args in case the util requires non-standard argument
            // parsing (e.g. dd)
            return execute_util(setup, filename, &mut iter::once(progname).chain(args))
                .map(|res| handle_util_result(setup, filename, res));
        }
    }

    None
}

fn handle_util_result<S>(setup: &mut S, filename: &OsStr, res: Result<()>) -> Result<()>
where
    S: UtilSetup,
{
    res.or_else(|mut mesa_err| {
        if let Some(ref e) = mesa_err.err {
            if let Some(clap_err) = e.downcast_ref::<clap::Error>() {
                if clap_err.kind == clap::ErrorKind::HelpDisplayed
                    || clap_err.kind == clap::ErrorKind::VersionDisplayed
                {
                    write!(setup.output(), "{}", clap_err)?;
                    return Ok(());
                }
            }
        }
        // TODO: check for --help and -V/--version probably
        if mesa_err.progname.is_none() {
            mesa_err.progname = Some(filename.to_os_string());
        }
        Err(mesa_err)
    })
}
