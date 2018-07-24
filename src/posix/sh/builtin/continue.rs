use super::{BuiltinSetup, UtilSetup, Environment, ExecData, ExitCode, Result, BreakBuiltin, CheckBreak};

#[derive(Clone, Copy)]
pub struct ContinueBuiltin;

impl BuiltinSetup for ContinueBuiltin {
    fn run<S>(&self, _setup: &mut S, env: &mut Environment, data: ExecData) -> Result<ExitCode>
    where
        S: UtilSetup,
    {
        BreakBuiltin::run_common(env, data, CheckBreak::Continue)
    }
}
