use clap::{App, Arg, OsValues};
use nom;
use rustyline::error::ReadlineError;
use rustyline::{CompletionType, Config, Editor};

use std::borrow::Cow;
use std::ffi::{OsStr, OsString};
use std::fs::File;
use std::io::{Read, Write};

use util::RawObjectWrapper;
use {ArgsIter, Result, UtilRead, UtilSetup, UtilWrite};

use self::ast::ExitCode;
use self::env::{EnvFd, Environment};
use self::parser::Parser;

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

// XXX: redefine here as nom forces the error type to be u32
macro_rules! complete (
    ($i:expr, $submac:ident!( $($args:tt)* )) => (
        {
            use ::std::result::Result::*;
            use $crate::nom::{Err, ErrorKind};

            let i_ = $i.clone();
            match $submac!(i_, $($args)*) {
                Err(Err::Incomplete(_)) =>  {
                    Err(Err::Error(error_position!($i, ErrorKind::Complete::<parser::ParserError>)))
                },
                rest => rest
            }
        }
    );
    ($i:expr, $f:expr) => (
        complete!($i, call!($f));
    );
);

pub fn execute<S, T>(setup: &mut S, args: T) -> Result<()>
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
            unimplemented!()
        }
    } else {
        // we have arguments and nothing was specified, so assume it's a script
        let mut args = matches.values_of_os("ARGUMENTS").unwrap();
        let script = args.next().unwrap();

        // FIXME: dunno what to do with exitcode (guess do the chmod trick where we set exitcode in
        //        MesaError?  maybe execute() should return Result<i32>)
        let code = run_script(setup, script, args)?;
        Ok(())
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

fn run_script<S>(setup: &mut S, name: &OsStr, args: OsValues) -> Result<ExitCode>
where
    S: UtilSetup,
{
    // TODO: handle errors using custom error type
    let mut input = File::open(name)?;
    let mut data = vec![];
    input.read_to_end(&mut data)?;

    let mut parser = Parser::new();
    let res = complete!(data.as_slice(), call_m!(parser.complete_command));

    // FIXME: clearly gross
    match res {
        Ok(m) => {
            let mut env = setup.env().into();
            setup_default_env(setup, &mut env)?;

            let mut data = ast::RuntimeData {
                setup: setup,
                env: &mut env,
            };

            Ok(m.1.execute(&mut data))
        }
        Err(nom::Err::Failure(ctx)) | Err(nom::Err::Error(ctx)) => {
            match ctx {
                nom::Context::List(ref vec) => {
                    for (_, err) in vec {
                        match err {
                            nom::ErrorKind::Custom(s) => {
                                writeln!(setup.error(), "{}", s)?;
                            }
                            other => {
                                writeln!(setup.error(), "{:#?}", other)?;
                            }
                        }
                    }
                    Ok(1)
                }
                _ => unimplemented!(),
            }
        }
        _ => unreachable!(),
    }
}

// FIXME: rustyline (and linefeed) seem to only return Strings instead of OsStrings, so they fail
//        on invalid UTF-8
fn start_repl<S>(setup: &mut S) -> Result<()>
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
                        let res = complete!(line.as_bytes(), call_m!(parser.complete_command));
                        match res {
                            Ok(m) => {
                                println!("status: {}", m.1.execute(&mut setup_data));
                                break;
                            }
                            // FIXME: this is super wasteful (we build up part of the tree and then
                            //        just trash it when it's not complete)
                            Err(nom::Err::Incomplete(_))
                            | Err(nom::Err::Error(nom::Context::Code(
                                _,
                                nom::ErrorKind::Complete,
                            )))
                            | Err(nom::Err::Failure(nom::Context::Code(
                                _,
                                nom::ErrorKind::Complete,
                            ))) => {}
                            Err(nom::Err::Failure(ctx)) | Err(nom::Err::Error(ctx)) => {
                                //println!("{}", f.into_error_kind())
                                match ctx {
                                    nom::Context::List(ref vec) => {
                                        for (_, err) in vec {
                                            match err {
                                                nom::ErrorKind::Custom(s) => {
                                                    println!("{}", s);
                                                }
                                                other => {
                                                    println!("{:#?}", other);
                                                }
                                            }
                                        }
                                        break;
                                    }
                                    nom::Context::Code(_, err) => match err {
                                        nom::ErrorKind::Custom(s) => {
                                            println!("{}", s);
                                        }
                                        other => {
                                            println!("{:#?}", other);
                                        }
                                    },
                                    _ => unimplemented!(),
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

                                let res =
                                    complete!(line.as_bytes(), call_m!(parser.complete_command));
                                match res {
                                    Ok(m) => {
                                        println!("status: {}", m.1.execute(&mut setup_data));
                                        break 'outer;
                                    }
                                    Err(nom::Err::Incomplete(_))
                                    | Err(nom::Err::Error(nom::Context::Code(
                                        _,
                                        nom::ErrorKind::Complete,
                                    )))
                                    | Err(nom::Err::Failure(nom::Context::Code(
                                        _,
                                        nom::ErrorKind::Complete,
                                    ))) => {}
                                    Err(f) => {
                                        println!("{}", f);
                                        break 'outer;
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

    Ok(())
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
