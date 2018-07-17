use clap::{App, AppSettings, Arg};

use super::{BuiltinSetup, UtilSetup, Environment, ExecData, ExitCode, Result};

#[derive(Clone, Copy)]
pub struct UnsetBuiltin;

impl BuiltinSetup for UnsetBuiltin {
    fn run<S>(&self, _setup: &mut S, env: &mut Environment, data: ExecData) -> Result<ExitCode>
    where
        S: UtilSetup,
    {
        // TODO: suppress --help/--version (non-POSIX, although they could perhaps serve as an extension)
        let matches = App::new("unset")
            .setting(AppSettings::NoBinaryName)
            .arg(Arg::with_name("function")
                .short("f")
                .overrides_with("variable"))
            .arg(Arg::with_name("variable")
                .short("v"))
            .arg(Arg::with_name("NAMES")
                .index(1)
                .multiple(true))
            .get_matches_from_safe(data.args)?;

        let func = matches.is_present("function");

        // TODO: if variable/whatever is readonly, this function should return >0 and NOT remove that
        //       variable
        if let Some(values) = matches.values_of_os("NAMES") {
            for name in values {
                if func {
                    env.remove_func(name);
                } else {
                    env.remove_var(name);
                }
            }
        }

        Ok(0)
    }
}