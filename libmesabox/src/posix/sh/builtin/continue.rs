use super::{
    BreakBuiltin, BuiltinSetup, CheckBreak, Environment, ExecData, ExitCode, Result, UtilSetup,
};

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
