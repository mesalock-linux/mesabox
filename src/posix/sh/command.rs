use nix::unistd;

use std::borrow::Cow;
use std::collections::HashMap;
use std::ffi::{OsStr, OsString};
use std::fs::File;
use std::io::Write;
use std::marker::PhantomData;
use std::os::unix::io::{FromRawFd, RawFd};
use std::os::unix::process::ExitStatusExt;
use std::process::{Command, Stdio};

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

pub enum CommandIo<'a> {
    Null,
    Inherit,
    Piped(&'a [u8]),
    File(File),
    Fd(RawFd),
    // TODO: child stdin/stdout/stderr
}

impl<'a> CommandIo<'a> {
    pub fn into_stdio(self) -> Result<Stdio> {
        use self::CommandIo::*;

        Ok(match self {
            Null => Stdio::null(),
            Inherit => Stdio::inherit(),
            Piped(_) => Stdio::piped(),
            File(file) => file.into(),
            Fd(fd) => {
                // XXX: make sure this is right with the stdlib and such
                let new_fd = unistd::dup(fd)?;
                unsafe { Stdio::from_raw_fd(new_fd) }
            }
        })
    }
}

pub struct CommandWrapper {
    input: Option<Vec<u8>>,
    output: Option<Vec<u8>>,
    error: Option<Vec<u8>>,
    cmd: Command,
}

impl CommandWrapper {
    pub fn new(mut cmd: Command) -> Self {
        cmd.env_clear();
        Self {
            input: None,
            output: None,
            error: None,
            cmd: cmd,
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

    fn stdin<'a>(&mut self, input: CommandIo<'a>) -> Result<&mut Self>;
    fn stdout<'a>(&mut self, output: CommandIo<'a>) -> Result<&mut Self>;
    fn stderr<'a>(&mut self, error: CommandIo<'a>) -> Result<&mut Self>;

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

impl<S: UtilSetup> CommandEnv<S> for CommandWrapper {
    fn env(&mut self, key: Cow<OsStr>, val: Cow<OsStr>) -> &mut Self {
        self.cmd.env(key, val);
        self
    }

    fn arg(&mut self, arg: Cow<OsStr>) -> &mut Self {
        self.cmd.arg(arg);
        self
    }

    fn stdin<'a>(&mut self, input: CommandIo<'a>) -> Result<&mut Self> {
        if let CommandIo::Piped(input) = input {
            match self.input {
                Some(ref mut buffer) => {
                    buffer.extend(input.iter());
                }
                ref mut val @ None => {
                    *val = Some(input.to_owned());
                }
            }
        }
        self.cmd.stdin(input.into_stdio()?);
        Ok(self)
    }

    fn stdout<'a>(&mut self, output: CommandIo<'a>) -> Result<&mut Self> {
        self.cmd.stdout(output.into_stdio()?);
        Ok(self)
    }

    fn stderr<'a>(&mut self, error: CommandIo<'a>) -> Result<&mut Self> {
        self.cmd.stderr(error.into_stdio()?);
        Ok(self)
    }

    fn status(mut self, _setup: &mut S, _env: &mut Environment<S>) -> Result<ExitCode>
    where
        S: UtilSetup,
    {
        let mut child = self.cmd.spawn()?;

        if let Some(input) = self.input {
            child.stdin.as_mut().unwrap().write_all(&input)?;
        }
        // TODO: output/error
        let stat = child.wait()?;
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

    // TODO: to get stdin/stdout/stderr working, we may need to execute the command (in status)
    //       with a new UtilSetup or something (this needs to be thought through).  doing so would
    //       let the functions work correctly too, but Environment will need to be fixed to not
    //       rely on a specific type of UtilSetup (maybe use dynamic dispatch?)
    fn stdin<'a>(&mut self, input: CommandIo<'a>) -> Result<&mut Self> {
        Ok(self)
    }

    fn stdout<'a>(&mut self, output: CommandIo<'a>) -> Result<&mut Self> {
        Ok(self)
    }

    fn stderr<'a>(&mut self, error: CommandIo<'a>) -> Result<&mut Self> {
        Ok(self)
    }

    fn status(self, setup: &mut S, env: &mut Environment<S>) -> Result<ExitCode>
    where
        S: UtilSetup,
    {
        Ok(self.cmd.execute(setup, env, ExecData { args: self.args, env: self.env }))
    }
}
