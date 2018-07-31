use super::{BuiltinSetup, Environment, ExecData, ExitCode, Result, UtilSetup};

#[derive(Clone, Copy)]
pub struct ExitBuiltin;

impl BuiltinSetup for ExitBuiltin {
    fn run<S: UtilSetup>(
        &self,
        _setup: &mut S,
        _env: &mut Environment,
        _data: ExecData,
    ) -> Result<ExitCode> {
        // TODO: figure out how to exit properly
        unimplemented!()
    }
}
