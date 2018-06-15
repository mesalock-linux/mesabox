//
// Copyright (c) 2018, The MesaLock Linux Project Contributors
// All rights reserved.
//
// This work is licensed under the terms of the BSD 3-Clause License.
// For a copy, see the LICENSE file.
//

// NOTE: get_matches!() and the last half of util_app!() are temporary until we can specify
//       stdin/stdout/stderr for clap

macro_rules! util_app {
    ($name:expr) => {
        util_app!($name, ())
    };
    ($name:expr, $setup:expr) => {
        util_app!($name, $setup, self::DESCRIPTION)
    };
    ($name:expr, $setup:expr, $desc:expr) => {{
        /*let (input, output, error) = $setup.stdio();
            let stdin = input.lock_reader()?;
            let stdout = output.lock_writer()?;
            let stderr = error.lock_writer()?;*/
        ::clap::App::new($name)
            .version(crate_version!())
            .author(crate_authors!())
            .about($desc)
        /*.setting(::clap::DisableHelpSubcommand)
                .setting(::clap::DisableVersion)
                .arg(::clap::Arg::with_name("version")
                        .short("V")
                        .long("version")
                        .help("Print version information"))
                .arg(::clap::Arg::with_name("help")
                        .long("help")
                        .help("Print help information"))*/
    }};
}

macro_rules! get_matches {
    ($matches:expr) => {};
}

// FIXME: should use name given on the command-line rather than a hard-coded one
macro_rules! display_msg {
    ($stream:expr, $($args:tt)+) => {
        writeln!($stream, "{}: {}", self::NAME, format_args!($($args)+))
    }
}

macro_rules! display_err {
    ($stream:expr, $($args:tt)+) => {
        display_msg!($stream, "error: {}", format_args!($($args)+))
    }
}

macro_rules! display_warn {
    ($stream:expr, $($args:tt)+) => {
        display_msg!($stream, "warning: {}", format_args!($($args)+))
    }
}
