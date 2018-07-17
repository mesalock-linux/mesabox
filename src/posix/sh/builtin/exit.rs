use super::{BuiltinSetup, UtilSetup, Environment, ExecData, ExitCode, Result};

#[derive(Clone, Copy)]
pub struct ExitBuiltin;

impl BuiltinSetup for ExitBuiltin {
    fn run<S: UtilSetup>(&self, _setup: &mut S, _env: &mut Environment, _data: ExecData) -> Result<ExitCode> {
        // TODO: figure out how to exit properly
        unimplemented!()
    }
}
