//
// Copyright (c) 2018, The MesaLock Linux Project Contributors
// All rights reserved.
// 
// This work is licensed under the terms of the BSD 3-Clause License.
// For a copy, see the LICENSE file.
//

extern crate rustc_version;

use std::env;
use std::fs::{self, File};
use std::io::{BufWriter, Write};
use std::path::Path;

fn main() {
    println!("cargo:rerun-if-changed=build.rs");

    let manifest_dir =
        env::var("CARGO_MANIFEST_DIR").expect("Cargo did not set CARGO_MANIFEST_DIR");

    let outdir = env::var("OUT_DIR").expect("Cargo did not set OUT_DIR");
    let output_path = Path::new(&outdir).join("utils.rs");

    let file = File::create(output_path).expect("could not open utils.rs for writing");
    let mut output = BufWriter::new(file);

    let mut utils = vec![];

    // TODO: ideally, stuff like "yes" should be able to be able to go in a "posix" folder or
    //       something similar and the code generation here would still work
    for entry in fs::read_dir("src").expect("could not read src/") {
        let entry = entry.expect("could not process directory entry");
        let path = entry.path();
        if path.is_dir() {
            let filename = path.file_name()
                .expect("directory entry does not have a filename");
            let name = filename.to_string_lossy().into_owned();
            writeln!(output, "#[path = \"{}/src/{}/mod.rs\"]", manifest_dir, name).unwrap();
            writeln!(output, "mod {};", name).unwrap();
            utils.push(name);
        }
    }

    let app_path = Path::new(&outdir).join("generate_app.rs");
    let app_file = File::create(app_path).expect("could not open generate_app.rs for writing");
    let mut app_output = BufWriter::new(app_file);
    write!(app_output, "app_from_crate!()").unwrap();
    for util in &utils {
        write!(app_output, ".subcommand(SubCommand::with_name({:?}).about({0}::DESCRIPTION))", util).unwrap();
    }

    let exec_path = Path::new(&outdir).join("execute_utils.rs");
    let exec_file = File::create(exec_path).expect("could not open execute_utils.rs for writing");
    let mut exec_output = BufWriter::new(exec_file);
    for util in &utils {
        // we can't do the easy "if blah { return Some(x); }" way because the compiler thinks that
        // we are missing else statements and errors
        write!(
            exec_output,
            "if name == {:?} {{ Some({0}::execute(setup, args)) }} else ",
            util
        ).unwrap();
    }
    write!(exec_output, "{{ None }}").unwrap();
}
