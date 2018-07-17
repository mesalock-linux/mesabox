use super::{BuiltinSetup, UtilSetup, Environment, ExecData, CheckBreak, ExitCode, Result, arg_to_usize};

#[derive(Clone, Copy)]
pub struct BreakBuiltin;

impl BreakBuiltin {
    pub fn run_common<S>(_setup: &mut S, env: &mut Environment, data: ExecData, kind: CheckBreak) -> Result<ExitCode>
    where
        S: UtilSetup,
    {
        let mut args = data.args.into_iter();

        let count = match args.next() {
            Some(arg) => arg_to_usize(arg, |count| count != 0)?,
            None => 1,
        };

        let count = count.min(env.loop_depth());
        env.set_break_counter(count);
        env.set_break_type(kind);

        Ok(0)
    }
}

impl BuiltinSetup for BreakBuiltin {
    fn run<S>(&self, setup: &mut S, env: &mut Environment, data: ExecData) -> Result<ExitCode>
    where
        S: UtilSetup,
    {
        Self::run_common(setup, env, data, CheckBreak::Break)
    }
}
