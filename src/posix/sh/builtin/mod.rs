use clap::{App, Arg, AppSettings};

use std::borrow::Cow;
use std::ffi::{OsStr, OsString};
use std::fmt;
use std::io::{BufRead, Write};
use std::os::unix::ffi::{OsStrExt, OsStringExt};

use ::UtilRead;
use super::{UtilSetup, Result};
use super::ast::ExitCode;
use super::command::{ExecData, InProcessCommand};
use super::env::Environment;

// XXX: in the future, this should probably be located somewhere else
#[derive(Clone, Debug)]
pub enum ShellOption {
    Default,
}

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
        Some(match &*name {
            "exit" => Builtin::Exit(ExitBuiltin),
            "export" => Builtin::Export(ExportBuiltin),
            "read" => Builtin::Read(ReadBuiltin),
            "unset" => Builtin::Unset(UnsetBuiltin),
            _ => return None
        })
    }
}

pub enum Builtin {
    Exit(ExitBuiltin),
    Export(ExportBuiltin),
    Read(ReadBuiltin),
    Unset(UnsetBuiltin),
}

impl InProcessCommand for Builtin {
    fn execute<S: UtilSetup>(&self, setup: &mut S, env: &mut Environment, data: ExecData) -> ExitCode {
        use self::Builtin::*;

        let res = match self {
            Exit(u) => u.run(setup, env, data),
            Export(u) => u.run(setup, env, data),
            Read(u) => u.run(setup, env, data),
            Unset(u) => u.run(setup, env, data),
        };

        match res {
            Ok(m) => m,
            Err(f) => {
                // XXX: do we really want to ignore write errors?
                let _ = writeln!(setup.error(), "{}", f);
                1
            }
        }
    }
}

trait BuiltinSetup {
    fn run<S: UtilSetup>(&self, setup: &mut S, env: &mut Environment, data: ExecData) -> Result<ExitCode>;
}

pub struct ExitBuiltin;

impl BuiltinSetup for ExitBuiltin {
    fn run<S: UtilSetup>(&self, setup: &mut S, env: &mut Environment, data: ExecData) -> Result<ExitCode> {
        // TODO: figure out how to exit properly
        unimplemented!()
    }
}

pub struct ExportBuiltin;

impl BuiltinSetup for ExportBuiltin {
    // TODO: needs to support -p option
    fn run<S>(&self, _setup: &mut S, env: &mut Environment, data: ExecData) -> Result<ExitCode>
    where
        S: UtilSetup,
    {
        // TODO: need to split args like VarAssign (we are just assuming names are given atm)
        for arg in data.args {
            env.export_var(Cow::Owned(arg));
        }

        Ok(0)
    }
}

pub struct UnsetBuiltin;

impl BuiltinSetup for UnsetBuiltin {
    fn run<S>(&self, _setup: &mut S, env: &mut Environment, data: ExecData) -> Result<ExitCode>
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
}

pub struct ReadBuiltin;

impl BuiltinSetup for ReadBuiltin {
    fn run<S>(&self, setup: &mut S, env: &mut Environment, data: ExecData) -> Result<ExitCode>
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

        let input = setup.input();
        let mut input = input.lock_reader()?;

        let ignore_backslash = matches.is_present("backslash");

        let check_backslash = |buffer: &mut Vec<u8>| {
            loop {
                let res = match buffer.iter().last() {
                    Some(b'\n') => {
                        buffer.pop();
                        continue;
                    }
                    Some(b'\\') => {
                        // need to make sure this byte isn't escaped
                        buffer.iter().rev().skip(1).take_while(|&&byte| byte == b'\\').count() % 2 == 1
                    }
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
            let value = if ignore_backslash {
                value.to_owned()
            } else {
                let mut result = Vec::with_capacity(value.len());
                let mut in_escape = false;
                for &byte in value {
                    if in_escape {
                        result.push(byte);
                        in_escape = false;
                    } else {
                        if byte == b'\\' {
                            in_escape = true;
                        } else {
                            result.push(byte);
                        }
                    }
                }
                // it should be impossible for there to be an extra escape
                result
            };
            env.set_var(Cow::Borrowed(var), OsString::from_vec(value));
        }

        Ok(0)
    }
}
