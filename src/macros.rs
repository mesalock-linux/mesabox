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
    ($name:tt) => {
        ::clap::App::new($name)
                        .version(crate_version!())
                        .author(crate_authors!())
                        .about(self::DESCRIPTION)
                        .arg(::clap::Arg::with_name("version")
                                .short("V")
                                .long("version")
                                .help("Print version info and exit"))
                        .arg(::clap::Arg::with_name("help")
                                .long("help")
                                .help("Print help information and exit"))
    };
}

macro_rules! get_matches {
    ($setup:tt, $app:tt, $args:tt) => {{
        let matches = $app.get_matches_from_safe_borrow($args)?;
        if matches.is_present("version") {
            $app.write_version(&mut $setup.stdout)?;
            return Ok(());
        } else if matches.is_present("help") {
            $app.write_help(&mut $setup.stdout)?;
            return Ok(());
        }
        matches
    }}
}

macro_rules! display_msg {
    ($stream:expr, $($args:tt)+) => {
        writeln!($stream, "{}: {}", self::NAME, format_args!($($args)+))
    }
}
