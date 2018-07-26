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

use util::*;
use std::fs::{metadata, OpenOptions, set_permissions};
use std::os::unix::fs::{OpenOptionsExt, PermissionsExt};
use std::sync::Mutex;
use libc::umask;

const NAME: &str = "chmod";

const TEST_FILE: &str = "file";
const REFERENCE_FILE: &str = "reference";
const REFERENCE_PERMS: u32 = 0o247;

lazy_static! {
    static ref UMASK_MUTEX: Mutex<()> = Mutex::new(());
}

struct TestCase {
    args: Vec<&'static str>,
    before: u32,
    after: u32
}

fn mkfile(file: &str, mode: u32) {
    OpenOptions::new().mode(mode).create(true).write(true).open(file).unwrap();
    let mut perms = metadata(file).unwrap().permissions();
    perms.set_mode(mode);
    set_permissions(file, perms).unwrap();
}

fn run_single_test(test: &TestCase, at: AtPath, mut ucmd: UCommand) {
        mkfile(&at.plus_as_string(TEST_FILE), test.before);
        let perms = at.metadata(TEST_FILE).permissions().mode();
        if perms != test.before {
            panic!(format!("{}: expected: {:o} got: {:o}", "setting permissions on test files before actual test run failed", test.after, perms));
        }

        for arg in &test.args {
            ucmd.arg(arg);
        }
        let r = ucmd.run();
        if !r.success {
            println!("{}", r.stderr_str());
            panic!(format!("{:?}: failed", ucmd.command_str()));
        }

        let perms = at.metadata(TEST_FILE).permissions().mode();
        if perms != test.after {
            panic!(format!("{:?}: expected: {:o} got: {:o}", ucmd.command_str(), test.after, perms));
        }
}

fn run_tests(tests: Vec<TestCase>) {
    for test in tests {
        let (at, ucmd) = at_and_ucmd!();
        run_single_test(&test, at, ucmd);
    }
}

#[test]
fn test_chmod_octal() {
    let tests = vec!{
        TestCase{args: vec!{"0700",  TEST_FILE}, before: 0o100000, after: 0o100700},
        TestCase{args: vec!{"0070",  TEST_FILE}, before: 0o100000, after: 0o100070},
        TestCase{args: vec!{"0007",  TEST_FILE}, before: 0o100000, after: 0o100007},
        TestCase{args: vec!{"-0700", TEST_FILE}, before: 0o100700, after: 0o100000},
        TestCase{args: vec!{"-0070", TEST_FILE}, before: 0o100060, after: 0o100000},
        TestCase{args: vec!{"-0007", TEST_FILE}, before: 0o100001, after: 0o100000},
        TestCase{args: vec!{"+0100", TEST_FILE}, before: 0o100600, after: 0o100700},
        TestCase{args: vec!{"+0020", TEST_FILE}, before: 0o100050, after: 0o100070},
        TestCase{args: vec!{"+0004", TEST_FILE}, before: 0o100003, after: 0o100007},
    };
    run_tests(tests);
}

#[test]
fn test_chmod_ugoa() {
    let _guard = UMASK_MUTEX.lock();

    let last = unsafe {
        umask(0)
    };
    let tests = vec!{
        TestCase{args: vec!{"u=rwx", TEST_FILE}, before: 0o100000, after: 0o100700},
        TestCase{args: vec!{"g=rwx", TEST_FILE}, before: 0o100000, after: 0o100070},
        TestCase{args: vec!{"o=rwx", TEST_FILE}, before: 0o100000, after: 0o100007},
        TestCase{args: vec!{"a=rwx", TEST_FILE}, before: 0o100000, after: 0o100777},
        TestCase{args: vec!{"-r", TEST_FILE}, before: 0o100777, after: 0o100333},
        TestCase{args: vec!{"-w", TEST_FILE}, before: 0o100777, after: 0o100555},
        TestCase{args: vec!{"-x", TEST_FILE}, before: 0o100777, after: 0o100666},
    };
    run_tests(tests);

    unsafe {
        umask(0o022);
    }
    let tests = vec!{
        TestCase{args: vec!{"u=rwx", TEST_FILE}, before: 0o100000, after: 0o100700},
        TestCase{args: vec!{"g=rwx", TEST_FILE}, before: 0o100000, after: 0o100070},
        TestCase{args: vec!{"o=rwx", TEST_FILE}, before: 0o100000, after: 0o100007},
        TestCase{args: vec!{"a=rwx", TEST_FILE}, before: 0o100000, after: 0o100777},
        TestCase{args: vec!{"+rw", TEST_FILE}, before: 0o100000, after: 0o100644},
        TestCase{args: vec!{"=rwx", TEST_FILE}, before: 0o100000, after: 0o100755},
        TestCase{args: vec!{"-w", TEST_FILE}, before: 0o100777, after: 0o100577},
        TestCase{args: vec!{"-x", TEST_FILE}, before: 0o100777, after: 0o100666},
    };
    run_tests(tests);
    unsafe {
        umask(last);
    }
}

#[test]
fn test_chmod_ugo_copy() {
    let tests = vec!{
        TestCase{args: vec!{"u=g", TEST_FILE}, before: 0o100070, after: 0o100770},
        TestCase{args: vec!{"g=o", TEST_FILE}, before: 0o100005, after: 0o100055},
        TestCase{args: vec!{"o=u", TEST_FILE}, before: 0o100200, after: 0o100202},
        TestCase{args: vec!{"u-g", TEST_FILE}, before: 0o100710, after: 0o100610},
        TestCase{args: vec!{"u+g", TEST_FILE}, before: 0o100250, after: 0o100750},
    };
    run_tests(tests);
}

#[test]
fn test_chmod_many_options() {
    let _guard = UMASK_MUTEX.lock();

    let original_umask = unsafe {
        umask(0)
    };
    let tests = vec!{
        TestCase{args: vec!{"-r,a+w", TEST_FILE}, before: 0o100444, after: 0o100222},
    };
    run_tests(tests);
    unsafe {
        umask(original_umask);
    }
}

#[test]
#[ignore]
fn test_chmod_reference_file() {
    let tests = vec!{
        TestCase{args: vec!{"--reference", REFERENCE_FILE, TEST_FILE}, before: 0o100070, after: 0o100247},
        TestCase{args: vec!{"a-w", "--reference", REFERENCE_FILE, TEST_FILE}, before: 0o100070, after: 0o100247},
    };
    let (at, ucmd) = at_and_ucmd!();
    mkfile(&at.plus_as_string(REFERENCE_FILE), REFERENCE_PERMS);
    run_single_test(&tests[0], at, ucmd);
}
