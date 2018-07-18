//
// Copyright (c) 2018, The MesaLock Linux Project Contributors
// All rights reserved.
// 
// This work is licensed under the terms of the BSD 3-Clause License.
// For a copy, see the LICENSE file.
//

extern crate platform_info;

use {UtilSetup, Result, ArgsIter};
use std::io::Write;
use self::platform_info::*;

pub(crate) const NAME: &str = "arch";
pub(crate) const DESCRIPTION: &str = "Print the architecture type";

pub fn execute<S, T>(setup: &mut S, args: T) -> Result<()>
where
    S: UtilSetup,
    T: ArgsIter,
{
    let _ = util_app!(NAME).get_matches_from_safe_borrow(args)?;

    writeln!(setup.output(), "{}", PlatformInfo::new()?.machine().trim())?;

    Ok(())
}
