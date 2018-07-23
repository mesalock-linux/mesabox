use std::ffi::{OsStr, OsString};
use std::io::{self, Write};
use std::iter;
use std::result::Result as StdResult;

use ::{UtilData, UtilRead, UtilWrite, ArgsIter};
use util::{ReadableVec, UtilReadDyn, UtilWriteDyn};
use super::UtilSetup;
use super::ast::{ExitCode, RuntimeData};
use super::command::{ExecData, InProcessChild, InProcessCommand, ShellChild};
use super::env::{CheckBreak, EnvFd, Environment, TryClone};
use super::error::{CmdResult, BuiltinError, CommandError};
use super::option::ShellOption;

use self::break_builtin::BreakBuiltin;
use self::cd::CdBuiltin;
use self::colon::ColonBuiltin;
use self::continue_builtin::ContinueBuiltin;
use self::exec::ExecBuiltin;
use self::exit::ExitBuiltin;
use self::export::ExportBuiltin;
use self::read::ReadBuiltin;
use self::shift::ShiftBuiltin;
use self::unset::UnsetBuiltin;

#[path = "break.rs"]
mod break_builtin;
mod cd;
mod colon;
#[path = "continue.rs"]
mod continue_builtin;
mod exec;
mod exit;
mod export;
mod read;
mod shift;
mod unset;

type Result<T> = StdResult<T, BuiltinError>;

#[derive(Clone, Debug)]
pub struct BuiltinSet {
    options: Vec<ShellOption>
}

impl BuiltinSet {
    pub fn new(options: Vec<ShellOption>) -> Self {
        Self {
            options: options,
        }
    }

    // XXX: in the future this should check the list of options to figure out what to do
    pub fn find(&self, name: &OsStr) -> Option<Builtin> {
        let name = name.to_string_lossy();
        loop {
            return Some(match &*name {
                "break" => Builtin::Break(BreakBuiltin),
                "cd" => Builtin::Cd(CdBuiltin),
                ":" => Builtin::Colon(ColonBuiltin),
                "continue" => Builtin::Continue(ContinueBuiltin),
                "exec" => Builtin::Exec(ExecBuiltin),
                "exit" => Builtin::Exit(ExitBuiltin),
                "export" => Builtin::Export(ExportBuiltin),
                "read" => Builtin::Read(ReadBuiltin),
                "shift" => Builtin::Shift(ShiftBuiltin),
                "unset" => Builtin::Unset(UnsetBuiltin),

                // TODO: should prevent certain utils from being run here (e.g. init and sh)
                other if util_exists(other) => break,
                _ => return None
            })
        }
        Some(Builtin::Other(name.into_owned()))
    }
}

#[derive(Clone)]
pub enum Builtin {
    Break(BreakBuiltin),
    Cd(CdBuiltin),
    Colon(ColonBuiltin),
    Continue(ContinueBuiltin),
    Exec(ExecBuiltin),
    Exit(ExitBuiltin),
    Export(ExportBuiltin),
    Read(ReadBuiltin),
    Shift(ShiftBuiltin),
    Unset(UnsetBuiltin),

    Other(String),
}

impl Builtin {
    fn execute_stdin<I>(&self, env: &mut Environment, data: ExecData, input: I) -> CmdResult<ExitCode>
    where
        I: for<'a> UtilRead<'a> + 'static,
    {
        use self::EnvFd::*;

        match env.get_fd(1).current_val().try_clone()? {
            File(file) => self.execute_stdout(env, data, input, file),
            Fd(fd) | ChildStdout(fd) => self.execute_stdout(env, data, input, fd),
            Pipe(pipe) => self.execute_stdout(env, data, input, pipe.raw_object_wrapper()),
            // FIXME: this won't work correctly
            Piped(piped) => self.execute_stdout(env, data, input, piped),
            Null => self.execute_stdout(env, data, input, io::sink()),
            _ => unimplemented!(),
        }
    }

    fn execute_stdout<I, O>(&self, env: &mut Environment, data: ExecData, input: I, output: O) -> CmdResult<ExitCode>
    where
        I: for<'a> UtilRead<'a> + 'static,
        O: for<'a> UtilWrite<'a> + 'static,
    {
        use self::EnvFd::*;

        match env.get_fd(2).current_val().try_clone()? {
            File(file) => self.execute_stderr(env, data, input, output, file),
            Fd(fd) | ChildStdout(fd) => self.execute_stderr(env, data, input, output, fd),
            Pipe(pipe) => self.execute_stderr(env, data, input, output, pipe.raw_object_wrapper()),
            // FIXME: this won't work correctly
            Piped(piped) => self.execute_stderr(env, data, input, output, piped),
            Null => self.execute_stderr(env, data, input, output, io::sink()),
            _ => unimplemented!(),
        }
    }

    fn execute_stderr<I, O, E>(&self, env: &mut Environment, data: ExecData, mut input: I, mut output: O, mut error: E) -> CmdResult<ExitCode>
    where
        I: for<'a> UtilRead<'a> + 'static,
        O: for<'a> UtilWrite<'a> + 'static,
        E: for<'a> UtilWrite<'a> + 'static,
    {
        use self::Builtin::*;

        // TODO: we let the current_dir be empty because that should be set in Environment most likely

        let utilname;
        loop {
            let mut util_setup = UtilData::new(&mut input, &mut output, &mut error, iter::empty(), None);
            let setup = &mut util_setup;

            return match self {
                Break(u) => u.run(setup, env, data),
                Cd(u) => u.run(setup, env, data),
                Colon(u) => u.run(setup, env, data),
                Continue(u) => u.run(setup, env, data),
                Exec(u) => u.run(setup, env, data),
                Exit(u) => u.run(setup, env, data),
                Export(u) => u.run(setup, env, data),
                Read(u) => u.run(setup, env, data),
                Shift(u) => u.run(setup, env, data),
                Unset(u) => u.run(setup, env, data),

                Other(util) => {
                    utilname = util;
                    break;
                }
            }.map_err(|e| CommandError::Builtin(e))
        }

        // FIXME: this needs use dynamic dispatch at the moment to avoid very, very slow build
        //        times and huge binaries (e.g. 8.5 min and 18 MB for release build with maybe 10
        //        utils + sh, like 30 sec and 112 MB for debug build with the same).  ideally, we
        //        would not have to do this (as obviously static dispatch is faster)
        // TODO: add anything else in data to setup
        // TODO: add export_vars to setup
        let input_fd = input.raw_object();
        let output_fd = output.raw_object();
        let error_fd = error.raw_object();

        let mut input = UtilReadDyn::new(Box::new(input), input_fd);
        let mut output = UtilWriteDyn::new(Box::new(output), output_fd);
        let mut error = UtilWriteDyn::new(Box::new(error), error_fd);

        let mut util_setup = UtilData::new(&mut input, &mut output, &mut error, iter::empty(), None);
        let setup = &mut util_setup;
        execute_util(setup, &OsStr::new(utilname), &mut iter::once(OsString::from(utilname)).chain(data.args.into_iter())).map(|_| 0).map_err(|e| {
            env.special_vars().set_last_exitcode(e.exitcode);
            CommandError::Builtin(BuiltinError::Other(e.err.unwrap().compat()))
        })
    }
}

impl InProcessCommand for Builtin {
    fn execute<'a: 'b, 'b, S: UtilSetup + 'a>(&self, rt_data: &mut RuntimeData<'a, 'b, S>, data: ExecData) -> CmdResult<ExitCode> {
        use self::EnvFd::*;

        let res = match rt_data.env.get_fd(0).current_val().try_clone()? {
            File(file) => self.execute_stdin(rt_data.env, data, file),
            Fd(fd) | ChildStdout(fd) => self.execute_stdin(rt_data.env, data, fd),
            Pipe(pipe) => self.execute_stdin(rt_data.env, data, pipe.raw_object_wrapper()),
            Piped(piped) => self.execute_stdin(rt_data.env, data, ReadableVec(piped)),
            Null => self.execute_stdin(rt_data.env, data, io::empty()),
            _ => unimplemented!(),
        };

        Ok(match res {
            Ok(m) => m,
            Err(f) => {
                // XXX: do we really want to ignore write errors?
                // FIXME: should probably not write to setup.error() unless we create a new
                //        UtilData struct each time we call a builtin
                let _ = writeln!(rt_data.setup.error(), "{}", f);
                1
            }
        })
    }

    fn spawn<'a: 'b, 'b, S: UtilSetup + 'a>(&self, rt_data: &mut RuntimeData<'a, 'b, S>, data: ExecData) -> CmdResult<ShellChild> {
        let child = InProcessChild::spawn(rt_data, |rt_data| {
            self.execute(rt_data, data)
        })?;
        Ok(ShellChild::InProcess(child))
    }
}

trait BuiltinSetup {
    fn run<S: UtilSetup>(&self, setup: &mut S, env: &mut Environment, data: ExecData) -> Result<ExitCode>;
}

fn arg_to_usize<F: FnOnce(usize) -> bool>(arg: OsString, validator: F) -> Result<usize> {
    // borrow checker work-around (to avoid an unnecessary allocation)
    let res = if let Some(s) = arg.to_str() {
        Some(s.parse::<usize>())
    } else {
        None
    };
    match res {
        Some(Ok(count)) => {
            if validator(count) {
                Ok(count)
            } else {
                Err(BuiltinError::InvalidNumber(arg))
            }
        }
        Some(Err(f)) => Err(BuiltinError::ParseInt { err: f, string: arg }),
        None => Err(BuiltinError::InvalidUtf8(arg)),
    }
}

fn execute_util<S, T>(setup: &mut S, name: &OsStr, args: T) -> ::Result<()>
where
    S: UtilSetup,
    T: ArgsIter,
{
    ::execute_util_sh(setup, name, args)
        .map(|res| ::handle_util_result(setup, name, res))
        .expect("invalid command name given to execute_util in sh")
}

fn util_exists(name: &str) -> bool {
    ::util_exists(name) && name != "sh"
}
