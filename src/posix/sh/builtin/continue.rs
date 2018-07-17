use super::{BuiltinSetup, UtilSetup, Environment, ExecData, ExitCode, Result, BreakBuiltin, CheckBreak};

#[derive(Clone, Copy)]
pub struct ContinueBuiltin;

impl BuiltinSetup for ContinueBuiltin {
    fn run<S>(&self, setup: &mut S, env: &mut Environment, data: ExecData) -> Result<ExitCode>
    where
        S: UtilSetup,
    {
        BreakBuiltin::run_common(setup, env, data, CheckBreak::Continue)
    }
}
