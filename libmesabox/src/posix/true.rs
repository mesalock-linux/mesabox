//
// Copyright (c) 2018, The MesaLock Linux Project Contributors
// All rights reserved.
//
// This work is licensed under the terms of the BSD 3-Clause License.
// For a copy, see the LICENSE file.
//

use {UtilSetup, ArgsIter, Result};

pub const DESCRIPTION: &str = "Returns 0";

pub fn execute<S, T>(_setup: &mut S, _args: T) -> Result<()>
where
    S: UtilSetup,
    T: ArgsIter,
{
    Ok(())
}
