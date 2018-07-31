//
// Copyright (c) 2018, The MesaLock Linux Project Contributors
// All rights reserved.
//
// This work is licensed under the terms of the BSD 3-Clause License.
// For a copy, see the LICENSE file.
//

use {UtilSetup, ArgsIter, Result, ExitCode, EXIT_FAILURE};

pub const DESCRIPTION: &str = "Returns a value greater than 0";

pub fn execute<S, T>(_setup: &mut S, _args: T) -> Result<ExitCode>
where
    S: UtilSetup,
    T: ArgsIter,
{
    Ok(EXIT_FAILURE)
}
