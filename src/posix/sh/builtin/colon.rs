use super::{BuiltinSetup, UtilSetup, Environment, ExecData, ExitCode, Result};

#[derive(Clone, Copy)]
pub struct ColonBuiltin;

impl BuiltinSetup for ColonBuiltin {
    fn run<S>(&self, _setup: &mut S, _env: &mut Environment, _data: ExecData) -> Result<ExitCode>
    where
        S: UtilSetup,
    {
        Ok(0)
    }
}
