//
// Copyright (c) 2018, The MesaLock Linux Project Contributors
// All rights reserved.
// 
// This work is licensed under the terms of the BSD 3-Clause License.
// For a copy, see the LICENSE file.
//

use super::{ArgsIter, Result, UtilRead, UtilWrite, UtilSetup};
use base32::common;
use uucore::encoding::Format;

pub const DESCRIPTION: &str = "Encode or decode input data to standard output using Base64";

pub fn execute<I, O, E, T>(setup: &mut UtilSetup<I, O, E>, args: T) -> Result<()>
where
    I: for<'a> UtilRead<'a>,
    O: for<'a> UtilWrite<'a>,
    E: for<'a> UtilWrite<'a>,
    T: ArgsIter,
{
    common::execute_base(setup, args, "base64", DESCRIPTION, Format::Base64)
}
