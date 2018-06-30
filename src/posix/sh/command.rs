use nix::unistd;

use std::borrow::Cow;
use std::collections::HashMap;
use std::ffi::{OsStr, OsString};
use std::io::{self, Write};
use std::os::unix::io::{AsRawFd, RawFd};
use std::os::unix::process::{CommandExt, ExitStatusExt};
use std::process::{Command, Stdio};

use super::{UtilSetup, Result};
use super::ast::ExitCode;
use super::env::{EnvFd, Environment};

/// A command executed within the current shell process (e.g. a function or builtin)
pub trait InProcessCommand {
    fn execute<S: UtilSetup>(&self, setup: &mut S, env: &mut Environment, data: ExecData) -> ExitCode;
}

impl<'a, T: InProcessCommand> InProcessCommand for &'a T {
    fn execute<S: UtilSetup>(&self, setup: &mut S, env: &mut Environment, data: ExecData) -> ExitCode {
        (**self).execute(setup, env, data)
    }
}

pub struct ExecData {
    pub args: Vec<OsString>,
    pub env: HashMap<OsString, OsString>,
}

pub struct ExecEnv<C: InProcessCommand> {
    cmd: C,
    args: Vec<OsString>,
    /// Temporary (i.e. per command) environment variables
    env: HashMap<OsString, OsString>,
}

impl<C: InProcessCommand> ExecEnv<C> {
    pub fn new(cmd: C) -> Self {
        Self {
            cmd: cmd,
            args: vec![],
            env: HashMap::default(),
        }
    }
}

pub struct CommandWrapper {
    cmd: Command,
}

impl CommandWrapper {
    pub fn new(mut cmd: Command) -> Self {
        cmd.env_clear();
        Self {
            cmd: cmd,
        }
    }
}

// TODO: need to add a spawn() equivalent for backgrounded processes
pub trait CommandEnv {
    fn env(&mut self, key: Cow<OsStr>, val: Cow<OsStr>) -> &mut Self;

    fn arg(&mut self, arg: Cow<OsStr>) -> &mut Self;

    fn status<S>(self, setup: &mut S, env: &mut Environment) -> Result<ExitCode>
    where
        S: UtilSetup;

    // NOTE: keeping this here for the time being until everything here is supported
    //       (the only thing that we don't do atm is return "bad file descriptor," we just explode
    //       on error)
    /*fn fd_alias(&mut self, fd: RawFd, copied: RawFd) -> Result<&mut Self> {
        let cmd_io = match self.get_fd(copied) {
            Some(value) => match value {
                EnvFd::File(file) => {
                    let copy = file.try_clone()?;
                    EnvFd::File(copy)
                }
                EnvFd::Fd(rawfd) => EnvFd::Fd(rawfd.clone()),
                EnvFd::Piped(data) => EnvFd::Piped(data.clone()),
                EnvFd::Null => EnvFd::Null,
                EnvFd::Inherit => EnvFd::Inherit,
            },
            None => return util::string_to_err(Err(format!("bad file descriptor '{}'", copied))),
        };
        self.fd(fd, cmd_io)
    }*/

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

impl CommandEnv for CommandWrapper {
    fn env(&mut self, key: Cow<OsStr>, val: Cow<OsStr>) -> &mut Self {
        self.cmd.env(key, val);
        self
    }

    fn arg(&mut self, arg: Cow<OsStr>) -> &mut Self {
        self.cmd.arg(arg);
        self
    }

    fn status<S>(mut self, setup: &mut S, env: &mut Environment) -> Result<ExitCode>
    where
        S: UtilSetup,
    {
        let mut buffer = Vec::with_capacity(0);

        // FIXME: buffer setup is clearly not right
        // FIXME: cleanup
        // FIXME: what if setup has Vec<u8> instead of Fd?
        let mut fd_iter = env.current_fds();
        {
            let mut convert_stdio = |cmd_io: &mut EnvFd| {
                if let EnvFd::Piped(ref data) = cmd_io {
                    buffer.extend(data.iter());
                    Ok(Stdio::piped())
                } else {
                    cmd_io.try_clone()?.into_stdio()
                }
            };

            self.cmd.stdin(convert_stdio(fd_iter.next().unwrap())?);
            self.cmd.stdout(convert_stdio(fd_iter.next().unwrap())?);
            self.cmd.stderr(convert_stdio(fd_iter.next().unwrap())?);
        }
        let fds: Vec<_> = fd_iter.enumerate().filter_map(|(i, val)| {
            match val {
                EnvFd::Fd(fd) => Some((i + 3, fd.fd)),
                EnvFd::File(file) => Some((i + 3, file.as_raw_fd())),
                EnvFd::Null => None,
                _ => unimplemented!(),
            }
        }).collect();
        self.cmd.before_exec(move || {
            for &(i, old_fd) in fds.iter() {
                unistd::dup2(old_fd, i as RawFd).map_err(|_| io::Error::last_os_error())?;
            }
            Ok(())
        });
        let mut child = self.cmd.spawn()?;

        if !buffer.is_empty() {
            child.stdin.as_mut().unwrap().write_all(&buffer)?;
        }
        // TODO: output/error
        let stat = child.wait()?;
        // NOTE: this should be fine as the only way for code() to fail is if a signal terminated
        //       the process
        // XXX: do we need to add 128 (iirc?) to the signal?
        Ok(stat.code().unwrap_or_else(|| stat.signal().unwrap()))
    }
}

impl<C: InProcessCommand> CommandEnv for ExecEnv<C> {
    fn env(&mut self, key: Cow<OsStr>, val: Cow<OsStr>) -> &mut Self {
        self.env.insert(key.into_owned(), val.into_owned());
        self
    }

    fn arg(&mut self, arg: Cow<OsStr>) -> &mut Self {
        self.args.push(arg.into_owned());
        self
    }

    fn status<S>(self, setup: &mut S, env: &mut Environment) -> Result<ExitCode>
    where
        S: UtilSetup,
    {
        // XXX: maybe we should treat env vars the same way as fds?  it would make things simpler,
        //      but not sure of the effect on execution speed
        Ok(self.cmd.execute(setup, env, ExecData { args: self.args, env: self.env }))
    }
}
