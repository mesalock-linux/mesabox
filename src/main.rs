//
// Copyright (c) 2018, The MesaLock Linux Project Contributors
// All rights reserved.
//
// This work is licensed under the terms of the BSD 3-Clause License.
// For a copy, see the LICENSE file.
//

extern crate mesabox;

use mesabox::UtilSetup;
use std::env;
use std::io::{self, Write};
use std::process;

// this is just a thin wrapper around the library
fn main() {
    let stdout = io::stdout();
    let stdin = io::stdin();
    let stderr = io::stderr();

    let mut setup = UtilSetup::new(stdin, stdout, stderr, Box::new(env::vars_os()), None);

    if let Err(f) = mesabox::execute(&mut setup, env::args_os()) {
        if let Some(ref err) = f.err {
            let mut skip = false;
            // XXX: should this be checked in lib.rs?  i feel like it might be useful if people can detect this, so it is being done this way atm
            if let Some(e) = err.downcast_ref::<io::Error>() {
                if e.kind() == io::ErrorKind::BrokenPipe {
                    skip = true;
                }
            }

            if !skip {
                let _ = writeln!(setup.stderr, "{}", f);
                let _ = setup.stderr.flush();
            }
        }
        process::exit(f.exitcode);
    }
}
