//
// Copyright (c) 2018, The MesaLock Linux Project Contributors
// All rights reserved.
// 
// This work is licensed under the terms of the BSD 3-Clause License.
// For a copy, see the LICENSE file.
//

use super::{ArgsIter, Result, UtilSetup};
use uucore::encoding::Format;

pub(crate) mod common;

pub const DESCRIPTION: &str = "Encode or decode input data to standard output using Base32";

pub fn execute<S, T>(setup: &mut S, args: T) -> Result<()>
where
    S: UtilSetup,
    T: ArgsIter,
{
    common::execute_base(setup, args, "base32", DESCRIPTION, Format::Base32)
}
