//
// Copyright (c) 2018, The MesaLock Linux Project Contributors
// All rights reserved.
// 
// This work is licensed under the terms of the BSD 3-Clause License.
// For a copy, see the LICENSE file.
//

extern crate platform_info;

use super::{UtilSetup, Result, /*ArgsIter, */UtilRead, UtilWrite};

use self::platform_info::*;
use std::ffi::OsString;

pub(crate) const NAME: &str = "arch";
pub(crate) const DESCRIPTION: &str = "Print the architecture type";

pub fn execute<I, O, E, T, U>(setup: &mut UtilSetup<I, O, E>, args: T) -> Result<()>
where
    I: for<'a> UtilRead<'a>,
    O: for<'a> UtilWrite<'a>,
    E: for<'a> UtilWrite<'a>,
    T: Iterator<Item = U>,
    U: Into<OsString> + Clone,
{
    let mut app = util_app!("arch");
    let _ = get_matches!(setup, app, args);

    writeln!(setup.stdout, "{}", PlatformInfo::new()?.machine().trim())?;

    Ok(())
}
