use std::borrow::Cow;
use std::collections::HashMap;
use std::ffi::{OsStr, OsString};
use std::marker::PhantomData;
use std::os::unix::process::ExitStatusExt;
use std::process::Command;

use super::{UtilSetup, Result};
use super::ast::ExitCode;
use super::env::Environment;

/// A command executed within the current shell process (e.g. a function or builtin)
pub trait InProcessCommand<S: UtilSetup> {
    fn execute(&self, setup: &mut S, env: &mut Environment<S>, data: ExecData) -> ExitCode;
}

impl<'a, T, S> InProcessCommand<S> for &'a T
where
    T: InProcessCommand<S>,
    S: UtilSetup,
{
    fn execute(&self, setup: &mut S, env: &mut Environment<S>, data: ExecData) -> ExitCode {
        (**self).execute(setup, env, data)
    }
}

pub struct ExecData {
    pub args: Vec<OsString>,
    pub env: HashMap<OsString, OsString>,
}

pub struct ExecEnv<C: InProcessCommand<S>, S: UtilSetup> {
    cmd: C,
    args: Vec<OsString>,
    /// Temporary (i.e. per command) environment variables
    env: HashMap<OsString, OsString>,

    _phantom: PhantomData<S>,
}

impl<C: InProcessCommand<S>, S: UtilSetup> ExecEnv<C, S> {
    pub fn new(cmd: C) -> Self {
        Self {
            cmd: cmd,
            args: vec![],
            env: HashMap::default(),
            _phantom: PhantomData,
        }
    }
}

// TODO: need to add a spawn() equivalent for backgrounded processes
pub trait CommandEnv<S: UtilSetup> {
    fn env(&mut self, key: Cow<OsStr>, val: Cow<OsStr>) -> &mut Self;

    fn arg(&mut self, arg: Cow<OsStr>) -> &mut Self;

    fn status(self, setup: &mut S, env: &mut Environment<S>) -> Result<ExitCode>
    where
        S: UtilSetup;

    fn envs<'a, I>(&mut self, vars: I) -> &mut Self
    where
        I: IntoIterator<Item = (Cow<'a, OsStr>, Cow<'a, OsStr>)>,
    {
        for (k, v) in vars {
            self.env(k, v);
        }
        self
    }

    fn args<'a, I>(&mut self, args: I) -> &mut Self
    where
        I: IntoIterator<Item = Cow<'a, OsStr>>,
    {
        for arg in args {
            self.arg(arg);
        }
        self
    }
}

impl<S: UtilSetup> CommandEnv<S> for Command {
    fn env(&mut self, key: Cow<OsStr>, val: Cow<OsStr>) -> &mut Self {
        (self as &mut Command).env(key, val)
    }

    fn arg(&mut self, arg: Cow<OsStr>) -> &mut Self {
        (self as &mut Command).arg(arg)
    }

    fn status(mut self, _setup: &mut S, _env: &mut Environment<S>) -> Result<ExitCode>
    where
        S: UtilSetup,
    {
        let stat = Command::status(&mut self)?;
        // NOTE: this should be fine as the only way for code() to fail is if a signal terminated
        //       the process
        // XXX: do we need to add 128 (iirc?) to the signal?
        Ok(stat.code().unwrap_or_else(|| stat.signal().unwrap()))
    }
}

impl<C, S> CommandEnv<S> for ExecEnv<C, S>
where
    C: InProcessCommand<S>,
    S: UtilSetup,
{
    fn env(&mut self, key: Cow<OsStr>, val: Cow<OsStr>) -> &mut Self {
        self.env.insert(key.into_owned(), val.into_owned());
        self
    }

    fn arg(&mut self, arg: Cow<OsStr>) -> &mut Self {
        self.args.push(arg.into_owned());
        self
    }

    fn status(self, setup: &mut S, env: &mut Environment<S>) -> Result<ExitCode>
    where
        S: UtilSetup,
    {
        Ok(self.cmd.execute(setup, env, ExecData { args: self.args, env: self.env }))
    }
}
