use super::{UtilSetup, Result, ArgsIter, UtilWrite};

use clap::Arg;
use std::borrow::Cow;
use std::io::Write;
use std::os::unix::ffi::OsStrExt;

pub(crate) const NAME: &str = "nc";
pub(crate) const DESCRIPTION: &str = "netcat";


pub fn execute<S, T>(setup: &mut S, args: T) -> Result<()>
where
    S: UtilSetup,
    T: ArgsIter,
{
    println!("this is netcat");

    Ok(())
}

