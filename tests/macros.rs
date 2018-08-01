//
// Copyright (c) 2018, The MesaLock Linux Project Contributors
// All rights reserved.
//
// This work is licensed under the terms of the BSD 3-Clause License.
// For a copy, see the LICENSE file.
//

#[macro_export]
macro_rules! util_name {
    () => {
        self::NAME
    };
}

#[macro_export]
macro_rules! new_cmd {
    () => {
        Command::main_binary().unwrap().arg(util_name!())
    };
}

#[macro_export]
macro_rules! fixtures_dir {
    () => {{
        use std::env;
        let mut path = env::current_dir().unwrap();
        path.push("tests/fixtures");
        path.push(util_name!());
        path
    }};
}

#[macro_export]
macro_rules! fixtures_path {
    ($filename:expr) => {{
        let mut fixtures_path = fixtures_dir!();
        fixtures_path.push($filename);
        fixtures_path
    }};
}

#[macro_export]
macro_rules! pred_eq_file {
    ($filename:expr) => {
        predicate::path::eq_file(fixtures_path!($filename).as_path())
    }
}

#[macro_export]
macro_rules! pred_str_contains {
    ($str:expr) => {
        predicate::str::contains($str).from_utf8()
    }
}
