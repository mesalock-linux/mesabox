use std::ffi::OsString;

use super::{BuiltinSetup, BuiltinError, UtilSetup, Environment, ExecData, ExitCode, Result, arg_to_usize};

#[derive(Clone, Copy)]
pub struct ShiftBuiltin;

impl BuiltinSetup for ShiftBuiltin {
    fn run<S>(&self, _setup: &mut S, env: &mut Environment, data: ExecData) -> Result<ExitCode>
    where
        S: UtilSetup,
    {
        let mut args = data.args.into_iter();

        let count = match args.next() {
            Some(arg) => arg_to_usize(arg, |count| {
                count <= env.special_vars().get_positionals().len()
            })?,
            None => if env.special_vars().get_positionals().len() > 0 {
                1
            } else {
                Err(BuiltinError::InvalidNumber(OsString::from("1")))?
            },
        };

        for _ in 0..count {
            env.special_vars().shift_positionals();
        }

        Ok(0)
    }
}
