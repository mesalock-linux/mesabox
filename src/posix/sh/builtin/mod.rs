use std::borrow::Cow;
use std::ffi::OsString;
use std::fmt;

use super::UtilSetup;
use super::ast::{ExitCode, Word};
use super::command::{ExecData, InProcessCommand};
use super::env::Environment;

#[derive(Clone)]
pub struct Builtin<S: UtilSetup> {
    handler: fn(setup: &mut S, env: &mut Environment<S>, data: ExecData) -> ExitCode
}

impl<S: UtilSetup> Builtin<S> {
    pub fn new(handler: fn(setup: &mut S, env: &mut Environment<S>, data: ExecData) -> ExitCode) -> Self {
        Self {
            handler: handler,
        }
    }
}

impl<S: UtilSetup> InProcessCommand<S> for Builtin<S> {
    fn execute(&self, setup: &mut S, env: &mut Environment<S>, data: ExecData) -> ExitCode {
        (self.handler)(setup, env, data)
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
        (OsString::from("export"), Builtin::new(export_handler))
    ]
}

fn exit_handler<S>(_setup: &mut S, _env: &mut Environment<S>, _data: ExecData) -> ExitCode
where
    S: UtilSetup,
{
    // TODO: figure out how to exit properly
    unimplemented!()
}

// TODO: needs to support -p option
fn export_handler<S>(_setup: &mut S, env: &mut Environment<S>, data: ExecData) -> ExitCode
where
    S: UtilSetup,
{
    // TODO: need to split args like VarAssign (we are just assuming names are given atm)
    for arg in data.args {
        env.export_var(Cow::Owned(arg));
    }

    0
}
