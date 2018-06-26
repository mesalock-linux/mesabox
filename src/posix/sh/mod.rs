use rustyline::{Config, Editor, KeyPress, Cmd, CompletionType};
use rustyline::error::ReadlineError;

use std::borrow::Cow;
use std::ffi::{OsStr, OsString};

use super::{UtilSetup, ArgsIter, Result};

use self::env::Environment;
use self::parser::Parser;

mod ast;
mod builtin;
mod command;
mod env;
mod parser;

pub const NAME: &str = "sh";
pub const DESCRIPTION: &str = "Minimal POSIX shell";

pub fn execute<S, T>(setup: &mut S, mut args: T) -> Result<()>
where
    S: UtilSetup,
    T: ArgsIter,
{
    use std::io::Read;

    start_repl(setup)?;

    let mut input = ::std::fs::File::open("input.sh").unwrap();
    let mut data = Vec::new();
    input.read_to_end(&mut data).unwrap();

    let mut parser = Parser::new();
    let res = complete!(data.as_slice(), call_m!(parser.complete_command));
    //let res = Parser::new().complete_command(data.as_slice());
    match res {
        Ok(m) => {
            println!("{:#?}", m);
            let mut env = setup.env().into();
            setup_default_env(setup, &mut env)?;

            //println!("{:#?}", m);
            //println!();
            println!("status: {}", m.1.execute(setup, &mut env));
        }
        Err(f) => println!("{}", f)
    }

    Ok(())
}

// FIXME: rustyline (and linefeed) seem to only return Strings instead of OsStrings, so they fail
//        on invalid UTF-8
fn start_repl<S>(setup: &mut S) -> Result<()>
where
    S: UtilSetup,
{
    let config = Config::builder().completion_type(CompletionType::List).build();

    let mut rl = Editor::<()>::with_config(config);

    // TODO: load history here

    let mut env = setup.env().into();
    setup_default_env(setup, &mut env)?;

    let mut parser = Parser::new();

    loop {
        let readline = {
            let ps1 = env.get_var("PS1");
            // FIXME: if string contains non-utf8, it won't be displayed
            rl.readline(&ps1.map(|s| s.to_string_lossy()).unwrap_or(Cow::from("")))
        };
        match readline {
            Ok(mut line) => {
                line.push('\n');
                {
                    // FIXME: complete! is not right as we can have functions split across multiple
                    //        lines and such (which is when e.g. PS2 comes in)
                    let res = complete!(line.as_bytes(), call_m!(parser.complete_command));
                    match res {
                        Ok(m) => {
                            println!("status: {}", m.1.execute(setup, &mut env));
                        }
                        Err(f) => {
                            println!("{}", f);
                        }
                    }
                }
                line.pop();
                rl.add_history_entry(line);
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
            }
            Err(err) => {
                println!("err: {:?}", err);
            }
        }
    }

    Ok(())
}

fn setup_default_env<S>(setup: &mut S, env: &mut Environment<S>) -> Result<()>
where
    S: UtilSetup,
{
    // prompt
    env.set_export_var(Cow::Borrowed(OsStr::new("PS1")), OsString::from("$ "));
    env.set_export_var(Cow::Borrowed(OsStr::new("PS2")), OsString::from("> "));
    env.set_export_var(Cow::Borrowed(OsStr::new("PS4")), OsString::from("+ "));

    env.set_export_var(Cow::Borrowed(OsStr::new("LINENO")), OsString::from("1"));

    let cur_dir = match setup.current_dir() {
        Some(p) => p.to_path_buf(),
        None => ::std::env::current_dir()?,
    };
    env.set_export_var(Cow::Borrowed(OsStr::new("PWD")), cur_dir.into());
    // FIXME: what to do about PWD and stuff?  just go based on env or set explicitly?

    // although HOME and PATH and stuff are used, we shouldn't set them explicitly

    env.add_builtins(builtin::default_builtins());

    Ok(())
}
