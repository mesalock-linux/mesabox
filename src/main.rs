//
// Copyright (c) 2018, The MesaLock Linux Project Contributors
// All rights reserved.
// 
// This work is licensed under the terms of the BSD 3-Clause License.
// For a copy, see the LICENSE file.
//

extern crate mesatools;

use mesatools::UtilSetup;
use std::env;
use std::io::{self, Write};
use std::process;

// this is just a thin wrapper around the library
fn main() {
    // FIXME: all of these should lock, but this causes issues with clap at the moment
    let stdout = io::stdout();
    let locked_stdout = stdout;//stdout.lock();

    let stdin = io::stdin();
    let locked_stdin = stdin;//stdin.lock();

    let stderr = io::stderr();
    let locked_stderr = stderr;//stderr.lock();

    let mut setup = UtilSetup::new(locked_stdin, locked_stdout, locked_stderr);

    if let Err(f) = mesatools::execute(&mut setup, env::args_os()) {
        if let Some(ref err) = f.err {
            let mut skip = false;
            // XXX: should this be checked in lib.rs?  i feel like it might be useful if people can detect this, so it is being done this way atm
            if let Some(e) = err.downcast_ref::<io::Error>() {
                if e.kind() == io::ErrorKind::BrokenPipe {
                    skip = true;
                }
            }

            if !skip {
                let _ = writeln!(setup.stderr, "{}", err);
                let _ = setup.stderr.flush();
            }
        }
        process::exit(f.exitcode);
    }
}
