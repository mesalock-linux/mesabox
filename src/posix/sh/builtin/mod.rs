use clap::{App, Arg, AppSettings};

use std::borrow::Cow;
use std::ffi::OsString;
use std::fmt;
use std::io::Write;

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
