include!("import.rs");

macro_rules! generate_executors {
    ($($group:ident { $(($util:tt, $feature:expr)),+ }),*) => {
        import_utils!($($group { $(($util, $feature)),+ }),*);

        generate_executors!(@dump_cmds $($group { $(($util, $feature)),+ }),*);
        generate_executors!(@easy_util $($group { $(($util, $feature)),+ }),*);
        generate_executors!(@execute_util $($group { $(($util, $feature)),+ }),*);
        generate_executors!(@generate_app $($group { $(($util, $feature)),+ }),*);
    };

    (@dump_cmds $($group:ident { $(($util:tt, $feature:expr)),+ }),*) => {
        pub fn dump_commands<S>(setup: &mut S) -> Result<ExitCode>
        where
            S: UtilSetup,
        {
            let stdout = setup.output();
            let mut stdout = stdout.lock()?;

            $($(
                #[cfg(feature = $feature)]
                {
                    writeln!(stdout, stringify!($util))?;
                }
            )+)*

            Ok(EXIT_SUCCESS)
        }
    };

    (@easy_util $($group:ident { $(($util:tt, $feature:expr)),+ }),*) => {
        $($(
            generate_executors!(@easy_util_single $group, $util, $feature);
        )+)*
    };

    (@easy_util_single $group:ident, true, $feature:expr) => { };
    (@easy_util_single $group:ident, false, $feature:expr) => { };
    (@easy_util_single $group:ident, $util:ident, $feature:expr) => {
        #[cfg(feature = $feature)]
        pub fn $util<T, U, V>(args: T) -> Result<ExitCode>
        where
            T: IntoIterator<IntoIter = V, Item = U>,
            U: Into<OsString> + AsRef<OsStr> + Clone,
            V: ArgsIter<ArgItem = U>,
        {
            let mut args = iter::once(OsString::from(stringify!($util))).chain(args.into_iter().map(|s| s.into()));
            EasyUtil::new().execute(&mut args, |setup, args| {
                generate_executors!(@execute_single $group, $util, setup, args).map(|res| {
                    let wrapper: util::ExitCodeWrapper = res.into();
                    wrapper.0
                })
            })
        }
    };

    (@execute_single $group:ident, true, $setup:ident, $args:ident) => {
        $group::true_mod::execute($setup, $args)
    };

    (@execute_single $group:ident, false, $setup:ident, $args:ident) => {
        $group::false_mod::execute($setup, $args)
    };

    (@execute_single $group:ident, $util:ident, $setup:ident, $args:ident) => {
        $group::$util::execute($setup, $args)
    };

    (@execute_util $($group:ident { $(($util:tt, $feature:expr)),+ }),*) => {
        fn execute_util<S, T>(setup: &mut S, name: &OsStr, args: T) -> Option<Result<ExitCode>>
        where
            S: UtilSetup,
            T: ArgsIter,
        {
            generate_executors!(@execute_util_common setup, name, args, $($group { $(($util, $feature)),+ }),*)
        }

        #[cfg(feature = "sh")]
        fn execute_util_sh<S, T>(setup: &mut S, name: &OsStr, args: T) -> Option<Result<ExitCode>>
        where
            S: UtilSetup,
            T: ArgsIter,
        {
            if name == "sh" {
                None
            } else {
                generate_executors!(@execute_util_common setup, name, args, $($group { $(($util, $feature)),+ }),*)
            }
        }

        #[cfg(feature = "sh")]
        fn util_exists(name: &str) -> bool {
            [
                $($(
                    #[cfg(feature = $feature)]
                    stringify!($util),
                )+)*
            ].contains(&name)
        }
    };

    (@execute_util_common $setup:ident, $name:ident, $args:ident, $($group:ident { $(($util:tt, $feature:expr)),+ }),*) => {{
        let mut result = None;
        loop {
            $($(
                #[cfg(feature = $feature)]
                {
                    if $name == stringify!($util) {
                        
                        let res = generate_executors!(@execute_single $group, $util, $setup, $args).map(|res| {
                            let wrapper: util::ExitCodeWrapper = res.into();
                            wrapper.0
                        });
                        result = Some(res);
                        break
                    }
                }
            )+)*
            if $name == "dump-cmds" {
                result = Some(dump_commands($setup))
            }
            break;
        }
        result
    }};

    (@generate_app $($group:ident { $(($util:tt, $feature:expr)),+ }),*) => {
        // generate a clap::App such that the available utils are set up as subcommands without any
        // arguments (adding all the arguments would slow down startup time)
        fn generate_app() -> App<'static, 'static> {
            let mut app = app_from_crate!();
            $($(
                #[cfg(feature = $feature)]
                {
                    app = app.subcommand(SubCommand::with_name(stringify!($util)).about(generate_executors!(@util_desc_single $group, $util)));
                }
            )+)*
            app.subcommand(SubCommand::with_name("dump-cmds").about("Print a list of commands in the binary"))
        }
    };

    (@util_desc_single $group:ident, true) => {
        $group::true_mod::DESCRIPTION
    };

    (@util_desc_single $group:ident, false) => {
        $group::false_mod::DESCRIPTION
    };

    (@util_desc_single $group:ident, $util:ident) => {
        $group::$util::DESCRIPTION
    }
}
