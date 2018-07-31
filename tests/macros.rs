//
// Copyright (c) 2018, The MesaLock Linux Project Contributors
// All rights reserved.
//
// This work is licensed under the terms of the BSD 3-Clause License.
// For a copy, see the LICENSE file.
//
// This file incorporates work covered by the following copyright and
// permission notice:
//
//     Copyright (c) 2013-2018, Jordi Boggiano
//     Copyright (c) 2013-2018, Alex Lyon
//     Copyright (c) 2015,      Joseph Crail
//     Copyright (c) 2015-2016, Nathan Ross
//
//     Permission is hereby granted, free of charge, to any person obtaining a
//     copy of this software and associated documentation files (the
//     "Software"), to deal in the Software without restriction, including
//     without limitation the rights to use, copy, modify, merge, publish,
//     distribute, sublicense, and/or sell copies of the Software, and to
//     permit persons to whom the Software is furnished to do so, subject to
//     the following conditions:
//
//     The above copyright notice and this permission notice shall be included
//     in all copies or substantial portions of the Software.
//
//     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
//     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
//     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
//     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
//     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
//     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
//     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
//

#[macro_export]
macro_rules! assert_empty_stderr(
    ($cond:expr) => (
        if $cond.stderr.len() > 0 {
            panic!(format!("stderr: {}", $cond.stderr))
        }
    );
);

#[macro_export]
macro_rules! assert_empty_stdout(
    ($cond:expr) => (
        if $cond.stdout.len() > 0 {
            panic!(format!("stdout: {}", $cond.stdout))
        }
    );
);

#[macro_export]
macro_rules! assert_no_error(
    ($cond:expr) => (
        assert!($cond.success);
        if $cond.stderr.len() > 0 {
            panic!(format!("stderr: {}", $cond.stderr))
        }
    );
);

#[macro_export]
macro_rules! path_concat {
    ($e:expr, ..$n:expr) => {{
        use std::path::PathBuf;
        let n = $n;
        let mut pb = PathBuf::new();
        for _ in 0..n {
            pb.push($e);
        }
        pb.to_str().unwrap().to_owned()
    }};
    ($($e:expr),*) => {{
        use std::path::PathBuf;
        let mut pb = PathBuf::new();
        $(
            pb.push($e);
        )*
        pb.to_str().unwrap().to_owned()
    }};
}

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
macro_rules! new_ucmd {
    () => {
        TestScenario::new(util_name!()).ucmd()
    };
}

#[macro_export]
macro_rules! at_and_ucmd {
    () => {{
        let ts = TestScenario::new(util_name!());
        (ts.fixtures.clone(), ts.ucmd())
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
