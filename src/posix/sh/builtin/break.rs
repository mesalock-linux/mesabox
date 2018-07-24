use super::{
    arg_to_usize, BuiltinSetup, CheckBreak, Environment, ExecData, ExitCode, Result, UtilSetup,
};

#[derive(Clone, Copy)]
pub struct BreakBuiltin;

impl BreakBuiltin {
    pub fn run_common(env: &mut Environment, data: ExecData, kind: CheckBreak) -> Result<ExitCode> {
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
    fn run<S>(&self, _setup: &mut S, env: &mut Environment, data: ExecData) -> Result<ExitCode>
    where
        S: UtilSetup,
    {
        Self::run_common(env, data, CheckBreak::Break)
    }
}
