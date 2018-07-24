use super::{BuiltinSetup, Environment, ExecData, ExitCode, Result, UtilSetup};

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
