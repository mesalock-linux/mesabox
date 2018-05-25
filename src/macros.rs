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
    ($name:expr, $setup:expr) => {
        util_app!($name, $setup, self::DESCRIPTION)
    };
    ($name:expr, $setup:expr, $desc:expr) => {{
        let stdin = $setup.stdin.lock_reader()?;
        let stdout = $setup.stdout.lock_writer()?;
        let stderr = $setup.stderr.lock_writer()?;
        ::clap::App::with_io($name, stdin, stdout, stderr)
                        .version(crate_version!())
                        .author(crate_authors!())
                        .about($desc)
    }}
}

// FIXME: should use name given on the command-line rather than a hard-coded one
macro_rules! display_msg {
    ($stream:expr, $($args:tt)+) => {
        writeln!($stream, "{}: {}", self::NAME, format_args!($($args)+))
    }
}
