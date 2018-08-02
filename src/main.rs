//
// Copyright (c) 2018, The MesaLock Linux Project Contributors
// All rights reserved.
//
// This work is licensed under the terms of the BSD 3-Clause License.
// For a copy, see the LICENSE file.
//

#[cfg(feature = "env_logger")]
extern crate env_logger;
extern crate libmesabox as mesabox;

use mesabox::UtilData;
#[cfg(feature = "system-alloc")]
use std::alloc::System;
use std::env;
use std::io::{self, Write};
use std::process;

#[cfg(feature = "system-alloc")]
#[global_allocator]
static GLOBAL: System = System;

// this is just a thin wrapper around the library
fn main() {
    #[cfg(feature = "env_logger")]
    {
        env_logger::init();
    }

    let stdout = io::stdout();
    let stdin = io::stdin();
    let stderr = io::stderr();

    let (mut input, mut output, mut error) = {
        #[cfg(feature = "full-dynamic")]
        {
            use mesabox::{AsRawObject, UtilReadDyn, UtilWriteDyn};

            let in_obj = stdin.as_raw_object();
            let out_obj = stdout.as_raw_object();
            let err_obj = stderr.as_raw_object();

            let input = UtilReadDyn::new(Box::new(stdin), Some(in_obj));
            let output = UtilWriteDyn::new(Box::new(stdout), Some(out_obj));
            let error = UtilWriteDyn::new(Box::new(stderr), Some(err_obj));
            (input, output, error)
        }
        #[cfg(not(feature = "full-dynamic"))]
        {
            (stdin, stdout, stderr)
        }
    };
    let mut setup = UtilData::new(&mut input, &mut output, &mut error, env::vars_os(), None);

    let code = mesabox::execute(&mut setup, &mut env::args_os()).unwrap_or_else(|f| {
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

        f.exitcode
    });

    process::exit(code);
}
