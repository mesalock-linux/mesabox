use nix::sys::wait::{self, WaitStatus};
use nix::unistd;

use std::borrow::Cow;
use std::collections::HashMap;
use std::ffi::{OsStr, OsString};
use std::io::{self, Write};
use std::os::unix::io::{AsRawFd, RawFd};
use std::os::unix::process::{CommandExt, ExitStatusExt};
use std::process::{self, Child, Command, ExitStatus, Stdio};
use std::rc::Rc;

use super::UtilSetup;
use super::ast::{ExitCode, FunctionBody, RuntimeData};
use super::builtin::Builtin;
use super::env::{EnvFd, Environment, TryClone};
use super::error::{CmdResult, CommandError};
use util::RawFdWrapper;

/// A command executed within the current shell process (e.g. a function or builtin)
pub trait InProcessCommand {
    fn execute<'a: 'b, 'b, S: UtilSetup + 'a>(&self, rt_data: &mut RuntimeData<'a, 'b, S>, data: ExecData) -> CmdResult<ExitCode>;
    fn spawn<'a: 'b, 'b, S: UtilSetup + 'a>(&self, rt_data: &mut RuntimeData<'a, 'b, S>, data: ExecData) -> CmdResult<ShellChild>;
}

impl<'a, T: InProcessCommand> InProcessCommand for &'a T {
    fn execute<'b: 'c, 'c, S: UtilSetup + 'b>(&self, rt_data: &mut RuntimeData<'b, 'c, S>, data: ExecData) -> CmdResult<ExitCode> {
        (**self).execute(rt_data, data)
    }

    fn spawn<'b: 'c, 'c, S: UtilSetup + 'b>(&self, rt_data: &mut RuntimeData<'b, 'c, S>, data: ExecData) -> CmdResult<ShellChild> {
        (**self).spawn(rt_data, data)
    }
}

impl<T: InProcessCommand> InProcessCommand for Rc<T> {
    fn execute<'a: 'b, 'b, S: UtilSetup + 'a>(&self, rt_data: &mut RuntimeData<'a, 'b, S>, data: ExecData) -> CmdResult<ExitCode> {
        (**self).execute(rt_data, data)
    }

    fn spawn<'a: 'b, 'b, S: UtilSetup + 'a>(&self, rt_data: &mut RuntimeData<'a, 'b, S>, data: ExecData) -> CmdResult<ShellChild> {
        (**self).spawn(rt_data, data)
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

    fn setup_child<S>(&mut self, _setup: &mut S, env: &mut Environment) -> CmdResult<ShellChild>
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

        Ok(ShellChild::RealChild(ChildWrapper { child: child, pipes: other_pipes }))
    }
}

// TODO: need to add a spawn() equivalent for backgrounded processes
pub trait CommandEnv {
    fn env(&mut self, key: Cow<OsStr>, val: Cow<OsStr>) -> &mut Self;

    fn arg(&mut self, arg: Cow<OsStr>) -> &mut Self;

    fn status<'a: 'b, 'b, S>(self, data: &mut RuntimeData<'a, 'b, S>) -> CmdResult<ExitCode>
    where
        S: UtilSetup + 'a;

    fn spawn<'a: 'b, 'b, S>(self, data: &mut RuntimeData<'a, 'b, S>) -> CmdResult<ShellChild>
    where
        S: UtilSetup + 'a;

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

    fn status<'a: 'b, 'b, S>(mut self, data: &mut RuntimeData<'a, 'b, S>) -> CmdResult<ExitCode>
    where
        S: UtilSetup + 'a,
    {
        let mut child = self.setup_child(data.setup, data.env)?;
        let stat = child.wait()?;
        Ok(stat.code())
    }

    fn spawn<'a: 'b, 'b, S>(mut self, data: &mut RuntimeData<'a, 'b, S>) -> CmdResult<ShellChild>
    where
        S: UtilSetup + 'a,
    {
        self.setup_child(data.setup, data.env)
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

    fn status<'a: 'b, 'b, S>(self, data: &mut RuntimeData<'a, 'b, S>) -> CmdResult<ExitCode>
    where
        S: UtilSetup + 'a,
    {
        // XXX: maybe we should treat env vars the same way as fds?  it would make things simpler,
        //      but not sure of the effect on execution speed
        self.cmd.execute(data, ExecData { args: self.args, env: self.env })
    }

    fn spawn<'a: 'b, 'b, S>(self, data: &mut RuntimeData<'a, 'b, S>) -> CmdResult<ShellChild>
    where
        S: UtilSetup + 'a,
    {
        // XXX: see if there's an easy way to implement this for all ExecEnvs
        self.cmd.spawn(data, ExecData { args: self.args, env: self.env })
    }
}

// this enum lets us return our commands up the call stack if needed and let's us use closures
// involving the commands
pub enum CommandEnvContainer {
    RealCommand(CommandWrapper),
    Builtin(ExecEnv<Builtin>),
    Function(ExecEnv<Rc<FunctionBody>>),
}

impl CommandEnv for CommandEnvContainer {
    fn env(&mut self, key: Cow<OsStr>, val: Cow<OsStr>) -> &mut Self {
        use self::CommandEnvContainer::*;

        match self {
            RealCommand(cmd) => { cmd.env(key, val); }
            Builtin(builtin) => { builtin.env(key, val); }
            Function(func) => { func.env(key, val); }
        }
        self
    }

    fn arg(&mut self, arg: Cow<OsStr>) -> &mut Self {
        use self::CommandEnvContainer::*;

        match self {
            RealCommand(cmd) => { cmd.arg(arg); }
            Builtin(builtin) => { builtin.arg(arg); }
            Function(func) => { func.arg(arg); }
        }
        self
    }

    fn status<'a: 'b, 'b, S>(self, data: &mut RuntimeData<'a, 'b, S>) -> CmdResult<ExitCode>
    where
        S: UtilSetup + 'a,
    {
        use self::CommandEnvContainer::*;

        match self {
            RealCommand(cmd) => cmd.status(data),
            Builtin(builtin) => builtin.status(data),
            Function(func) => func.status(data),
        }
    }

    fn spawn<'a: 'b, 'b, S>(self, data: &mut RuntimeData<'a, 'b, S>) -> CmdResult<ShellChild>
    where
        S: UtilSetup + 'a,
    {
        use self::CommandEnvContainer::*;

        match self {
            RealCommand(cmd) => cmd.spawn(data),
            Builtin(builtin) => builtin.spawn(data),
            Function(func) => func.spawn(data),
        }
    }
}

pub struct InProcessChild {
    pid: unistd::Pid,
    stdout: EnvFd,
    write_fd: Option<RawFd>,
}

impl InProcessChild {
    pub fn new(handle: unistd::Pid, stdout: EnvFd, write_fd: Option<RawFd>) -> Self {
        Self {
            pid: handle,
            stdout: stdout,
            write_fd: write_fd,
        }
    }

    // FIXME: stop unwrapping
    pub fn spawn<S, F>(data: &mut RuntimeData<S>, func: F) -> CmdResult<Self>
    where
        S: UtilSetup,
        F: FnOnce(&mut RuntimeData<S>) -> CmdResult<ExitCode>,
    {
        let mut read = None;
        let mut write = None;
        let stdout = match data.env.get_fd(1).current_val() {
            EnvFd::Pipeline => {
                let (read_pipe, write_pipe) = unistd::pipe().unwrap();
                read = Some(read_pipe);
                write = Some(write_pipe);
                EnvFd::Fd(RawFdWrapper::new(read_pipe, true, false))
            }
            other => other.try_clone()?
        };
        if let Some(write) = write {
            data.env.set_local_fd(1, EnvFd::Fd(RawFdWrapper::new(write, false, true)));
        }

        match unistd::fork().unwrap() {
            unistd::ForkResult::Child => {
                // FIXME: don't unwrap
                process::exit(func(data).unwrap())
            }
            unistd::ForkResult::Parent { child } => {
                if let Some(write) = write {
                    unistd::close(write).unwrap();
                }
                Ok(InProcessChild::new(child, stdout, read))
            }
        }
    }

    pub fn wait(&mut self) -> CmdResult<ExitCode> {
        match wait::waitpid(self.pid, None).unwrap() {
            WaitStatus::Exited(_, code) => Ok(code),
            _ => unimplemented!(),
        }
    }
}

impl Drop for InProcessChild {
    fn drop(&mut self) {
        if let Some(fd) = self.write_fd {
            unistd::close(fd);
        }
    }
}

pub enum ShellChild {
    RealChild(ChildWrapper),
    InProcess(InProcessChild),
    Empty,
}

impl ShellChild {
    pub fn wait(&mut self) -> CmdResult<ChildExitStatus> {
        use self::ShellChild::*;

        match self {
            RealChild(wrapper) => wrapper.child.wait().map(|stat| ChildExitStatus::RealChild(stat)).map_err(|e| CommandError::RealCommandStatus(e)),
            InProcess(child) => child.wait().map(|code| ChildExitStatus::InProcess(code)),
            Empty => Ok(ChildExitStatus::Empty),
        }
    }

    pub fn try_wait(&mut self) -> CmdResult<Option<ChildExitStatus>> {
        use self::ShellChild::*;

        match self {
            RealChild(wrapper) => wrapper.child.try_wait().map(|stat| stat.map(|stat| ChildExitStatus::RealChild(stat))).map_err(|e| CommandError::RealCommandStatus(e)),
            InProcess(child) => unimplemented!(),
            Empty => Ok(Some(ChildExitStatus::Empty)),
        }
    }

    pub fn output(&self) -> EnvFd {
        use self::ShellChild::*;

        match self {
            RealChild(wrapper) => wrapper.child.stdout.as_ref().map(|out| EnvFd::ChildStdout(RawFdWrapper::new(out.as_raw_fd(), true, false))).unwrap_or(EnvFd::Null),
            InProcess(child) => child.stdout.try_clone().unwrap(),//unimplemented!(),
            Empty => EnvFd::Null,
        }
    }
}

pub enum ChildExitStatus {
    RealChild(ExitStatus),
    InProcess(ExitCode),
    Empty,
}

impl ChildExitStatus {
    pub fn code(&self) -> ExitCode {
        use self::ChildExitStatus::*;

        match self {
            RealChild(stat) => {
                // NOTE: this should be fine as the only way for code() to fail is if a signal terminated
                //       the process
                // XXX: do we need to add 128 (iirc?) to the signal?
                stat.code().unwrap_or_else(|| stat.signal().unwrap())
            }
            InProcess(code) => *code,
            // XXX: can this be anything else???
            Empty => 0,
        }
    }
}

pub struct ChildWrapper {
    child: Child,
    pipes: Vec<RawFd>,
}

impl Drop for ChildWrapper {
    fn drop(&mut self) {
        for &pipe in &self.pipes {
            // XXX: not sure if we really want to error here (maybe just report the error?)
            //unistd::close(pipe).map_err(|e| CommandError::Pipe(e))?;
            unistd::close(pipe);
        }
    }
}
