use clap::{App, Arg, AppSettings};

use std::borrow::Cow;
use std::ffi::{OsStr, OsString};
use std::fmt;
use std::io::{BufRead, Write};
use std::os::unix::ffi::OsStrExt;

use ::UtilRead;
use super::{UtilSetup, Result};
use super::ast::ExitCode;
use super::command::{ExecData, InProcessCommand};
use super::env::Environment;

#[derive(Clone)]
pub struct Builtin<S: UtilSetup> {
    handler: fn(setup: &mut S, env: &mut Environment<S>, data: ExecData) -> Result<ExitCode>
}

impl<S: UtilSetup> Builtin<S> {
    pub fn new(handler: fn(setup: &mut S, env: &mut Environment<S>, data: ExecData) -> Result<ExitCode>) -> Self {
        Self {
            handler: handler,
        }
    }
}

impl<S: UtilSetup> InProcessCommand<S> for Builtin<S> {
    fn execute(&self, setup: &mut S, env: &mut Environment<S>, data: ExecData) -> ExitCode {
        match (self.handler)(setup, env, data) {
            Ok(m) => m,
            Err(f) => {
                // XXX: do we really want to ignore write errors?
                let _ = writeln!(setup.error(), "{}", f);
                1
            }
        }
    }
}

// FIXME: would be nice if this display the builtin's name
impl<S: UtilSetup> fmt::Debug for Builtin<S> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Builtin<S: UtilSetup> {{ handler: fn(setup: &mut S, env: &mut Environment<S>, args: &[&[Word]]) -> ExitCode }}")
    }
}

// XXX: perhaps the Environment should be passed as an argument so we can add builtins without the
//      intermediate Vec?
pub fn default_builtins<S>() -> Vec<(OsString, Builtin<S>)>
where
    S: UtilSetup,
{
    vec![
        (OsString::from("exit"), Builtin::new(exit_handler)),
        (OsString::from("export"), Builtin::new(export_handler)),
        (OsString::from("read"), Builtin::new(read_handler)),
        (OsString::from("unset"), Builtin::new(unset_handler)),
    ]
}

fn exit_handler<S>(_setup: &mut S, _env: &mut Environment<S>, _data: ExecData) -> Result<ExitCode>
where
    S: UtilSetup,
{
    // TODO: figure out how to exit properly
    unimplemented!()
}

// TODO: needs to support -p option
fn export_handler<S>(_setup: &mut S, env: &mut Environment<S>, data: ExecData) -> Result<ExitCode>
where
    S: UtilSetup,
{
    // TODO: need to split args like VarAssign (we are just assuming names are given atm)
    for arg in data.args {
        env.export_var(Cow::Owned(arg));
    }

    Ok(0)
}

fn unset_handler<S>(_setup: &mut S, env: &mut Environment<S>, data: ExecData) -> Result<ExitCode>
where
    S: UtilSetup,
{
    // TODO: suppress --help/--version (non-POSIX, although they could perhaps serve as an extension)
    let matches = App::new("unset")
        .setting(AppSettings::NoBinaryName)
        .arg(Arg::with_name("function")
            .short("f")
            .overrides_with("variable"))
        .arg(Arg::with_name("variable")
            .short("v"))
        .arg(Arg::with_name("NAMES")
            .index(1)
            .multiple(true))
        .get_matches_from_safe(data.args)?;

    let func = matches.is_present("function");

    // TODO: if variable/whatever is readonly, this function should return >0 and NOT remove that
    //       variable
    if let Some(values) = matches.values_of_os("NAMES") {
        for name in values {
            if func {
                env.remove_func(name);
            } else {
                env.remove_var(name);
            }
        }
    }

    Ok(0)
}

fn read_handler<S>(setup: &mut S, env: &mut Environment<S>, data: ExecData) -> Result<ExitCode>
where
    S: UtilSetup,
{
    let matches = App::new("read")
        .setting(AppSettings::NoBinaryName)
        // if present we treat backslash as a normal character rather than the start of an escape
        // sequence
        .arg(Arg::with_name("backslash")
            .short("r"))
        .arg(Arg::with_name("VARS")
            .index(1)
            .multiple(true)
            .required(true))
        .get_matches_from_safe(data.args)?;

    let mut input = setup.input();
    let mut input = input.lock_reader()?;

    let ignore_backslash = matches.is_present("backslash");

    let check_backslash = |buffer: &mut Vec<u8>| {
        loop {
            let res = match buffer.iter().last() {
                Some(b'\n') => {
                    buffer.pop();
                    continue;
                }
                Some(b'\\') => false,
                _ => true,
            };
            return res;
        }
    };

    let mut buffer = vec![];
    loop {
        // TODO: check for EOF
        input.read_until(b'\n', &mut buffer)?;
        let not_backslash = check_backslash(&mut buffer);
        // TODO: handle heredoc portion?
        if ignore_backslash || not_backslash {
            break;
        }
        // we need to remove the backslash
        buffer.pop();
    }

    let vars = matches.values_of_os("VARS").unwrap();
    let var_count = vars.clone().count();

    let field_iter = {
        // XXX: maybe this should be extracted into a separate function (i feel like this will be used
        //      to split fields normally too)
        let ifs = env.get_var("IFS").map(|v| v.clone()).unwrap_or_else(|| OsString::from(" \t\n"));
        buffer.splitn(var_count, move |byte| {
            ifs.as_bytes().contains(byte)
        })
    };
    for (var, value) in vars.zip(field_iter) {
        env.set_var(Cow::Borrowed(var), OsStr::from_bytes(value).to_owned());
    }

    Ok(0)
}
