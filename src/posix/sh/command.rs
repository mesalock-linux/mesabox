use nix::unistd;

use std::borrow::Cow;
use std::collections::HashMap;
use std::ffi::{OsStr, OsString};
use std::io::{self, Write};
use std::os::unix::io::{AsRawFd, RawFd};
use std::os::unix::process::{CommandExt, ExitStatusExt};
use std::process::{Command, Stdio};

use super::UtilSetup;
use super::ast::ExitCode;
use super::env::{EnvFd, Environment};
use super::error::{CmdResult, CommandError};
use util::RawFdWrapper;

/// A command executed within the current shell process (e.g. a function or builtin)
pub trait InProcessCommand {
    fn execute<S: UtilSetup>(&self, setup: &mut S, env: &mut Environment, data: ExecData) -> CmdResult<ExitCode>;
}

impl<'a, T: InProcessCommand> InProcessCommand for &'a T {
    fn execute<S: UtilSetup>(&self, setup: &mut S, env: &mut Environment, data: ExecData) -> CmdResult<ExitCode> {
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

    fn status<S>(self, setup: &mut S, env: &mut Environment) -> CmdResult<ExitCode>
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

impl CommandEnv for CommandWrapper {
    fn env(&mut self, key: Cow<OsStr>, val: Cow<OsStr>) -> &mut Self {
        self.cmd.env(key, val);
        self
    }

    fn arg(&mut self, arg: Cow<OsStr>) -> &mut Self {
        self.cmd.arg(arg);
        self
    }

    fn status<S>(mut self, setup: &mut S, env: &mut Environment) -> CmdResult<ExitCode>
    where
        S: UtilSetup,
    {
        let mut stdin_pipe = None;
        let mut stdout_pipe = None;
        let mut stderr_pipe = None;
        // TODO: make a static buffer (memory allocation is not necessary as we can only have FD_COUNT - 3 FDs here)
        let mut other_pipes = Vec::with_capacity(0);

        // FIXME: cleanup
        // FIXME: what if setup has Vec<u8> instead of Fd?
        let mut fd_iter = env.current_fds().enumerate();
        {
            let convert_stdio = |(_, cmd_io): (_, &mut EnvFd), pipe: &mut _| {
                if let EnvFd::Piped(ref data) = cmd_io {
                    *pipe = Some(data.clone());
                    Ok(Stdio::piped())
                } else {
                    cmd_io.try_clone()?.into_stdio()
                }
            };

            self.cmd.stdin(convert_stdio(fd_iter.next().unwrap(), &mut stdin_pipe)?);
            self.cmd.stdout(convert_stdio(fd_iter.next().unwrap(), &mut stdout_pipe)?);
            self.cmd.stderr(convert_stdio(fd_iter.next().unwrap(), &mut stderr_pipe)?);
        }

        let mut fds = Vec::with_capacity(0);
        for (i, val) in fd_iter {
            fds.push(match val {
                EnvFd::Fd(fd) => (i, fd.fd),
                EnvFd::File(file) => (i, file.as_raw_fd()),
                EnvFd::Piped(ref data) => {
                    // we need to manually create the pipe as the stdlib only handles stdio
                    let (read, mut write) = unistd::pipe().map_err(|e| CommandError::Pipe(e))?;

                    // just write now and close the writable end of the pipe as we won't need it
                    // later
                    RawFdWrapper::new(write, false, true).write_all(data).map_err(|e| CommandError::PipeIo(e))?;
                    unistd::close(write).map_err(|e| CommandError::Pipe(e))?;

                    other_pipes.push(read);
                    (i, read)
                }
                EnvFd::Null => continue,
                _ => unimplemented!(),
            });
        }

        self.cmd.before_exec(move || {
            for &(i, old_fd) in fds.iter() {
                unistd::dup2(old_fd, i as RawFd).map_err(|_| io::Error::last_os_error())?;
            }
            Ok(())
        });
        let mut child = self.cmd.spawn().map_err(|e| CommandError::StartRealCommand(e))?;

        if let Some(data) = stdin_pipe {
            child.stdin.as_mut().expect("Could not open stdin pipe").write_all(&data).map_err(|e| CommandError::PipeIo(e))?;
        }
        // TODO: stdout/stderr (and any other pipes that are piped into other commands)

        let stat = child.wait().map_err(|e| CommandError::RealCommandStatus(e))?;

        for pipe in other_pipes {
            // XXX: not sure if we really want to error here (maybe just report the error?)
            unistd::close(pipe).map_err(|e| CommandError::Pipe(e))?;
        }

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

    fn status<S>(self, setup: &mut S, env: &mut Environment) -> CmdResult<ExitCode>
    where
        S: UtilSetup,
    {
        // XXX: maybe we should treat env vars the same way as fds?  it would make things simpler,
        //      but not sure of the effect on execution speed
        self.cmd.execute(setup, env, ExecData { args: self.args, env: self.env })
    }
}
