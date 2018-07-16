//
// Copyright (c) 2018, The MesaLock Linux Project Contributors
// All rights reserved.
//
// This work is licensed under the terms of the BSD 3-Clause License.
// For a copy, see the LICENSE file.
//

use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::{BufWriter, Write};
use std::path::Path;

fn create_util_map() -> HashMap<&'static str, &'static str> {
    let mut hashmap = HashMap::new();

    hashmap.insert("arch", "gnu");
    hashmap.insert("base32", "gnu");
    hashmap.insert("base64", "gnu");
    hashmap.insert("yes", "gnu");

    hashmap.insert("getty", "loginutils");

    hashmap.insert("tar", "lsb");

    hashmap.insert("ping", "networking");

    hashmap.insert("cat", "posix");
    hashmap.insert("chmod", "posix");
    hashmap.insert("echo", "posix");
    hashmap.insert("head", "posix");
    hashmap.insert("sh", "posix");
    hashmap.insert("sleep", "posix");

    hashmap.insert("init", "sysinit");

    hashmap
}

fn main() {
    println!("cargo:rerun-if-changed=build.rs");

    let util_map = create_util_map();

    let manifest_dir =
        env::var("CARGO_MANIFEST_DIR").expect("Cargo did not set CARGO_MANIFEST_DIR");

    let outdir = env::var("OUT_DIR").expect("Cargo did not set OUT_DIR");
    let output_path = Path::new(&outdir).join("utils.rs");

    let file = File::create(output_path).expect("could not open utils.rs for writing");
    let mut output = BufWriter::new(file);

    let output_path = Path::new(&outdir).join("test_utils.rs");

    let file = File::create(output_path).expect("could not open test_utils.rs for writing");
    let mut test_output = BufWriter::new(file);

    let mut utils = vec![];

    for &util in util_map.keys() {
        let util = if util == "tar" { "tar_util" } else { util };
        if env::var_os(format!("CARGO_FEATURE_{}", util.to_uppercase())).is_some() {
            let util = if util == "tar_util" { "tar" } else { util };
            writeln!(
                output,
                "#[path = \"{}/src/{}/{}/mod.rs\"]",
                manifest_dir,
                util_map.get(util).unwrap(),
                util
            ).unwrap();
            writeln!(output, "mod {};", util).unwrap();

            writeln!(
                test_output,
                "#[path = \"{}/tests/{}/test_{}.rs\"]",
                manifest_dir,
                util_map.get(util).unwrap(),
                util
            ).unwrap();
            writeln!(test_output, "mod test_{};", util).unwrap();

            utils.push(util);
        }
    }

    writeln!(
        output,
        "pub fn dump_commands<S>(setup: &mut S) -> Result<()>
where
    S: UtilSetup,
{{
    let stdout = setup.output();
    let mut stdout = stdout.lock_writer()?;"
    ).unwrap();
    for util in &utils {
        writeln!(output, "writeln!(stdout, {:?})?;", util).unwrap();
    }
    writeln!(output, "Ok(())\n}}").unwrap();

    for util in &utils {
        writeln!(
            output,
            "pub fn {}<T, U, V>(args: T) -> Result<()>
where
    T: IntoIterator<IntoIter = V, Item = U>,
    U: Into<OsString> + AsRef<OsStr> + Clone,
    V: ArgsIter<ArgItem = U>,
{{
    let mut args = iter::once(OsString::from({0:?})).chain(args.into_iter().map(|s| s.into()));
    EasyUtil::new().execute(&mut args, {0}::execute)
}}",
            util
        ).unwrap();
    }

    let app_path = Path::new(&outdir).join("generate_app.rs");
    let app_file = File::create(app_path).expect("could not open generate_app.rs for writing");
    let mut app_output = BufWriter::new(app_file);
    write!(app_output, "app_from_crate!()").unwrap();
    for util in &utils {
        write!(
            app_output,
            ".subcommand(SubCommand::with_name({:?}).about({0}::DESCRIPTION))",
            util
        ).unwrap();
    }
    write!(app_output, ".subcommand(SubCommand::with_name(\"dump-cmds\").about(\"Print a list of commands in the binary\"))").unwrap();

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
    write!(
        exec_output,
        "if name == \"dump-cmds\" {{ Some(dump_commands(setup)) }} else {{ None }}"
    ).unwrap();

    // the following is for executing mesabox utilities in sh
    let exists_path = Path::new(&outdir).join("util_exists.rs");
    let exists_file = File::create(exists_path).expect("could not open util_exists.rs for writing");
    let mut exists_output = BufWriter::new(exists_file);
    write!(exists_output, "&[").unwrap();
    if let Some(util) = utils.iter().next() {
        write!(exists_output, "{:?}", util).unwrap();
    }
    for util in utils.iter().skip(1) {
        write!(exists_output, ", {:?}", util).unwrap();
    }
    write!(exists_output, "]").unwrap();

    let exec_path = Path::new(&outdir).join("execute_utils_sh.rs");
    let exec_file =
        File::create(exec_path).expect("could not open execute_utils_sh.rs for writing");
    let mut exec_output = BufWriter::new(exec_file);
    for &util in &utils {
        // we can't do the easy "if blah { return Some(x); }" way because the compiler thinks that
        // we are missing else statements and errors
        if util != "sh" {
            write!(
                exec_output,
                "if name == {:?} {{ Some(::{0}::execute(setup, args)) }} else ",
                util
            ).unwrap();
        }
    }
    writeln!(exec_output, "{{ None }}").unwrap();
}
