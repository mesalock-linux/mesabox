use clap::{App, Arg, OsValues};
use rustyline::error::ReadlineError;
use rustyline::{CompletionType, Config, Editor};

use std::borrow::Cow;
use std::ffi::{OsStr, OsString};
use std::fs::File;
use std::io::{Read, Write};
use std::iter;
use std::path::Path;

use util::{self, ExitCode, RawObjectWrapper};
use {ArgsIter, Result, UtilRead, UtilSetup, UtilWrite};

use self::env::{EnvFd, Environment};
use self::parser::{Parser, ParserError};

mod ast;
mod builtin;
mod command;
mod env;
mod error;
pub mod option;
mod parser;
mod types;

pub const NAME: &str = "sh";
pub const DESCRIPTION: &str = "Minimal POSIX shell";

pub fn execute<S, T>(setup: &mut S, args: T) -> Result<ExitCode>
where
    S: UtilSetup,
    T: ArgsIter,
{
    let matches = create_app().get_matches_from_safe(args)?;
    if matches.is_present("command-string") {
        // TODO: read command string stuff
        unimplemented!()
    } else if matches.is_present("command-stdin") || !matches.is_present("ARGUMENTS") {
        // TODO: also need to check if stdin is a terminal if -i was not specified
        if matches.is_present("interactive") {
            start_repl(setup)
        } else {
            // TODO: read commands from stdin and exit
            let mut data = vec![];
            setup.input().read_to_end(&mut data)?;

            match matches.values_of_os("ARGUMENTS") {
                Some(args) => run_data(setup, &data, args),
                None => run_data(setup, &data, iter::empty()),
            }
        }
    } else {
        // we have arguments and nothing was specified, so assume it's a script
        let mut args = matches.values_of_os("ARGUMENTS").unwrap();
        let script = util::actual_path(&setup.current_dir(), args.next().unwrap());

        run_script(setup, &script, args)
    }
}

fn create_app() -> App<'static, 'static> {
    // TODO: support all the options that can be given to `set` as well
    util_app!(NAME)
        .arg(Arg::with_name("command-string")
            .short("c")
            .help("read commands from the given command string")
            .requires("ARGUMENTS"))
        .arg(Arg::with_name("interactive")
            .short("i")
            .help("specify that the shell is interactive"))
        .arg(Arg::with_name("command-stdin")
            .short("s")
            .help("read commands from the standard input"))
        .arg(Arg::with_name("ARGUMENTS")
            .index(1)
            .multiple(true))
}

fn run_script<S>(setup: &mut S, name: &Path, args: OsValues) -> Result<ExitCode>
where
    S: UtilSetup,
{
    // TODO: handle errors using custom error type
    let mut input = File::open(name)?;
    let mut data = vec![];
    input.read_to_end(&mut data)?;

    run_data(setup, &data, args)
}

fn run_data<'a, S, I>(setup: &mut S, data: &[u8], args: I) -> Result<ExitCode>
where
    S: UtilSetup,
    I: Iterator<Item = &'a OsStr>,
{
    let mut parser = Parser::new();

    // FIXME: how to deal with data read from input?  need to somehow convert that data to osstring
    // FIXME: below is ugly hack for unix rn
    let data = {
        use std::os::unix::ffi::OsStrExt;

        OsStr::from_bytes(data)
    };

    let mut input = parser.convert_input(data);
    let mut env = setup.env().into();
    setup_default_env(setup, &mut env)?;

    let mut data = ast::RuntimeData {
        setup: setup,
        env: &mut env,
    };

    let mut code = 0;

    while input.clone().next().is_some() {
        match parser.complete_command(input.clone()) {
            Ok((inp, cmd)) => {
                code = cmd.execute(&mut data);
                input = inp;
            }
            Err(f) => {
                if input.next().is_some() {
                    code = 1;
                    return Err(f)?;
                } else {
                    break;
                }
            }
        }
    }

    Ok(code)
}

// FIXME: rustyline (and linefeed) seem to only return Strings instead of OsStrings, so they fail
//        on invalid UTF-8
fn start_repl<S>(setup: &mut S) -> Result<ExitCode>
where
    S: UtilSetup,
{
    let config = Config::builder()
        .completion_type(CompletionType::List)
        .build();

    let mut rl = Editor::<()>::with_config(config);

    // TODO: load history here

    let mut env = setup.env().into();
    setup_default_env(setup, &mut env)?;

    let mut setup_data = ast::RuntimeData {
        setup: setup,
        env: &mut env,
    };

    let mut parser = Parser::new();

    loop {
        let readline = {
            let ps1 = setup_data.env.get_var("PS1");
            // FIXME: if string contains non-utf8, it won't be displayed
            rl.readline(&ps1.map(|s| s.to_string_lossy()).unwrap_or(Cow::from("")))
        };
        match readline {
            Ok(mut line) => {
                line.push('\n');
                'outer: loop {
                    // FIXME: complete! is not right as we can have functions split across multiple
                    //        lines and such (which is when e.g. PS2 comes in)
                    {
                        let input = parser.convert_input(OsStr::new(&line));
                        let res = parser.complete_command(input);
                        match res {
                            Ok(m) => {
                                println!("status: {}", m.1.execute(&mut setup_data));
                                break;
                            }
                            // FIXME: this is super wasteful (we build up part of the tree and then
                            //        just trash it when it's not complete)
                            Err(f) => {
                                if !f.incomplete() {
                                    writeln!(setup_data.setup.error(), "{}", f)?;
                                }
                            }
                        }
                    }

                    // we use this loop trick here again to escape the borrow checker (the code
                    // should probably be split into functions as it's all very similar)
                    // the input is incomplete, so read more
                    loop {
                        let new_data = {
                            let ps2 = setup_data.env.get_var("PS2");
                            rl.readline(&ps2.map(|s| s.to_string_lossy()).unwrap_or(Cow::from("")))
                        };
                        match new_data {
                            Ok(data) => {
                                line.push_str(&data);
                                line.push('\n');

                                let input = parser.convert_input(OsStr::new(&line));
                                let res = parser.complete_command(input);
                                match res {
                                    Ok(m) => {
                                        println!("status: {}", m.1.execute(&mut setup_data));
                                        break 'outer;
                                    }
                                    Err(f) => {
                                        if !f.incomplete() {
                                            writeln!(setup_data.setup.error(), "{}", f);
                                            break 'outer;
                                        }
                                    }
                                }
                            }
                            _ => unimplemented!(),
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

    Ok(0)
}

fn setup_default_env<S>(setup: &mut S, env: &mut Environment) -> Result<()>
where
    S: UtilSetup,
{
    // prompt
    env.set_export_var(Cow::Borrowed(OsStr::new("PS1")), OsString::from("$ "));
    env.set_export_var(Cow::Borrowed(OsStr::new("PS2")), OsString::from("> "));
    env.set_export_var(Cow::Borrowed(OsStr::new("PS4")), OsString::from("+ "));

    env.set_export_var(Cow::Borrowed(OsStr::new("LINENO")), OsString::from("1"));
    env.set_export_var(Cow::Borrowed(OsStr::new("IFS")), OsString::from(" \t\n"));

    let cur_dir = match setup.current_dir() {
        Some(p) => p.to_path_buf(),
        None => ::std::env::current_dir()?,
    };
    env.set_export_var(Cow::Borrowed(OsStr::new("PWD")), cur_dir.into());
    // FIXME: what to do about PWD and stuff?  just go based on env or set explicitly?

    // although HOME and PATH and stuff are used, we shouldn't set them explicitly

    // FIXME: what to do if can't create fd? (such as in testing framework)
    env.set_fd(
        0,
        EnvFd::Fd(RawObjectWrapper::try_from(
            setup.input().raw_object().unwrap(),
        )?),
    );
    env.set_fd(
        1,
        EnvFd::Fd(RawObjectWrapper::try_from(
            setup.output().raw_object().unwrap(),
        )?),
    );
    env.set_fd(
        2,
        EnvFd::Fd(RawObjectWrapper::try_from(
            setup.error().raw_object().unwrap(),
        )?),
    );

    Ok(())
}
