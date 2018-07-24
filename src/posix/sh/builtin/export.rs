use std::borrow::Cow;

use super::{BuiltinSetup, Environment, ExecData, ExitCode, Result, UtilSetup};

#[derive(Clone, Copy)]
pub struct ExportBuiltin;

impl BuiltinSetup for ExportBuiltin {
    // TODO: needs to support -p option
    fn run<S>(&self, _setup: &mut S, env: &mut Environment, data: ExecData) -> Result<ExitCode>
    where
        S: UtilSetup,
    {
        // TODO: need to split args like VarAssign (we are just assuming names are given atm)
        for arg in data.args {
            env.export_var(Cow::Owned(arg));
        }

        Ok(0)
    }
}
