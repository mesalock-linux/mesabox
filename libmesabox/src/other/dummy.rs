//
// Copyright (c) 2018, The MesaLock Linux Project Contributors
// All rights reserved.
//
// This work is licensed under the terms of the BSD 3-Clause License.
// For a copy, see the LICENSE file.
//

use clap::{App, Arg, ArgMatches};
use std::io::{self, Write};
use {UtilSetup, ArgsIter, Result, UtilRead, UtilWrite};

const NAME: &str = "dummy";
pub const DESCRIPTION: &str = "A dummy utility to demonstrate the framework";

type DummyResult<T> = ::std::result::Result<T, DummyError>;

#[derive(Fail, Debug)]
enum DummyError {
    #[fail(display = "oh no, something wrong")]
    SomethingWrong
}

struct DummyOptions {
    verbose: bool
}

impl DummyOptions {
    fn from_matches(matches: &ArgMatches) -> Self {
        let mut options = Self::default();

        options.verbose = matches.is_present("verbose");

        options
    }
}

impl Default for DummyOptions {
    fn default() -> Self {
        Self {
            verbose: false
        }
    }
}

struct Dummyer<O>
where
    O: Write
{
    output: O
}

impl<O> Dummyer<O>
where
    O: Write
{
    fn new(output: O) -> Self {
        Dummyer { output }
    }

    fn dummy(&mut self, options: &DummyOptions) -> DummyResult<()> {
        if options.verbose {
            writeln!(self.output, "Hello, world! This is a dummy utility. I am very verbose :)");
            return Err(DummyError::SomethingWrong)
        } else {
            writeln!(self.output, "Hello, world!");
        }
        Ok(())
    }
}

fn create_app() -> App<'static, 'static> {
    util_app!(NAME)
        .arg(Arg::with_name("verbose")
                .short("v")
                .long("verbose")
                .help("Say hello in verbose mode"))
}

pub fn execute<S, T>(setup: &mut S, args: T) -> Result<()>
where
    S: UtilSetup,
    T: ArgsIter,
{
    let app = create_app();
    let matches = app.get_matches_from_safe(args)?;
    let options = DummyOptions::from_matches(&matches);

    let output = setup.output();
    let mut output = output.lock()?;

    let mut dummyer = Dummyer::new(output);
    dummyer.dummy(&options)?;
    Ok(())
}
