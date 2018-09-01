//
// Copyright (c) 2018, The MesaLock Linux Project Contributors
// All rights reserved.
//
// This work is licensed under the terms of the BSD 3-Clause License.
// For a copy, see the LICENSE file.
//

use {UtilSetup, ArgsIter, Result, UtilRead, UtilWrite};
use std::io::{self, Write};

pub const DESCRIPTION: &str = "A dummy utility to demonstrate the framework";

#[derive(Fail, Debug)]
enum DummyError {
    #[fail(display = "something wrong")]
    SomethingWrong
}

type DummyResult<T> = ::std::result::Result<T, DummyError>;

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

    fn dummy(&mut self) -> DummyResult<()> {
        writeln!(self.output, "write something to the output");
        Ok(())
    }
}

pub fn execute<S, T>(setup: &mut S, _args: T) -> Result<()>
where
    S: UtilSetup,
    T: ArgsIter,
{
    let output = setup.output();
    let mut output = output.lock()?;
    let mut dummyer = Dummyer::new(output);
    dummyer.dummy()?;
    Ok(())
}
