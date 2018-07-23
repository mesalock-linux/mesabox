use nix::unistd;

use std::process::{Command, Stdio};
use std::os::unix::io::FromRawFd;
use std::os::unix::process::CommandExt;

use super::{BuiltinSetup, UtilSetup, UtilRead, UtilWrite, Environment, ExecData, ExitCode, Result};

#[derive(Clone, Copy)]
pub struct ExecBuiltin;

// XXX: given that this replaces the current process, if we are being used as a library the calling
//      process will be replaced.  this could be an issue when e.g. running our tests
// TODO: because this needs to affect the "current shell execution environment," we need to somehow
//       return the fds to the parent environment
impl BuiltinSetup for ExecBuiltin {
    fn run<S>(&self, setup: &mut S, env: &mut Environment, data: ExecData) -> Result<ExitCode>
    where
        S: UtilSetup,
    {
        let mut args = data.args.into_iter();
        if let Some(name) = args.next() {
            // replace the current process with that started by the given command
            let mut cmd = Command::new(name);
            cmd.args(args)
                .env_clear()
                .envs(env.export_iter())
                .envs(data.env.iter());

            // TODO: figure out what to do if one of the IO interfaces doesn't have a file
            //       descriptor (such as as Vec<u8>).  afaict this is only really an issue with
            //       heredocs and when we are called as a library from a process that most likely
            //       does not actually want to be replaced
            // NOTE: we need to duplicate the fds as from_raw_fd() takes ownership
            // TODO: this needs to duplicate all the fds (3-9 because stdin/stdout/stderr are done
            //       already below) like in command.rs
            if let Some(fd) = setup.input().raw_object() {
                let fd = unistd::dup(fd)?;
                cmd.stdin(unsafe { Stdio::from_raw_fd(fd) });
            }
            if let Some(fd) = setup.output().raw_object() {
                let fd = unistd::dup(fd)?;
                cmd.stdout(unsafe { Stdio::from_raw_fd(fd) });
            }
            if let Some(fd) = setup.error().raw_object() {
                let fd = unistd::dup(fd)?;
                cmd.stderr(unsafe { Stdio::from_raw_fd(fd) });
            }

            // if this actually returns an error the process failed to start
            Err(cmd.exec().into())
        } else {
            Ok(0)
        }
    }
}
