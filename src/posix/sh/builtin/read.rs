use clap::{App, AppSettings, Arg, OsValues};

use std::borrow::Cow;
use std::ffi::OsString;
use std::io::BufRead;
use std::os::unix::ffi::{OsStrExt, OsStringExt};

use super::{BuiltinSetup, UtilSetup, UtilRead, Environment, ExecData, ExitCode, Result};

#[derive(Clone, Copy)]
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
        let mut input = input.lock()?;

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

        setup_vars(env, vars, &buffer, ignore_backslash)
    }
}

fn setup_vars(env: &mut Environment, vars: OsValues, buffer: &[u8], ignore_backslash: bool) -> Result<ExitCode> {
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
