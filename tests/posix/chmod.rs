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

use assert_cmd::prelude::*;
use assert_fs;
use assert_fs::prelude::*;
use std::fs::{metadata, set_permissions};
use std::os::unix::fs::PermissionsExt;
use std::process::Command;
use std::sync::Mutex;
use libc::umask;

const NAME: &str = "chmod";

lazy_static! {
    static ref UMASK_MUTEX: Mutex<()> = Mutex::new(());
}

fn run_test(before: u32, mode: &str, expected: u32) {
    let temp_dir = assert_fs::TempDir::new().unwrap();
    let temp_file = temp_dir.child("TEST_FILE");
    temp_file.touch().unwrap();
    let temp_filepath = temp_file.path();
    let mut perms = metadata(temp_filepath).unwrap().permissions();
    perms.set_mode(before);
    set_permissions(temp_filepath, perms).unwrap();

    new_cmd!()
        .args(&[mode, temp_filepath.to_str().unwrap()])
        .assert()
        .success();

    let perms_after = metadata(temp_filepath).unwrap().permissions().mode();
    if perms_after != expected {
        panic!(format!("expected: {:o} got: {:o}", expected, perms_after));
    }
}

#[test]
fn test_chmod_octal() {
    let tests = vec![
        (0o100000, "0700",  0o100700),
        (0o100000, "0070",  0o100070),
        (0o100000, "0007",  0o100007),
        (0o100700, "-0700", 0o100000),
        (0o100060, "-0070", 0o100000),
        (0o100001, "-0007", 0o100000),
        (0o100600, "+0100", 0o100700),
        (0o100050, "+0020", 0o100070),
        (0o100003, "+0004", 0o100007),
    ];
    for t in tests {
        let (before, mode, expected) = t;
        run_test(before, mode, expected);
    }
}

#[test]
fn test_chmod_ugoa() {
    let _guard = UMASK_MUTEX.lock();

    let last = unsafe {
        umask(0)
    };
    let tests = vec![
        (0o100000, "u=rwx", 0o100700),
        (0o100000, "g=rwx", 0o100070),
        (0o100000, "o=rwx", 0o100007),
        (0o100000, "a=rwx", 0o100777),
        (0o100777, "-r"   , 0o100333),
        (0o100777, "-w"   , 0o100555),
        (0o100777, "-x"   , 0o100666),
    ];
    for t in tests {
        let (before, mode, expected) = t;
        run_test(before, mode, expected);
    }

    unsafe {
        umask(0o022);
    }

    let tests = vec![
        (0o100000, "u=rwx", 0o100700),
        (0o100000, "g=rwx", 0o100070),
        (0o100000, "o=rwx", 0o100007),
        (0o100000, "a=rwx", 0o100777),
        (0o100000, "+rw"  , 0o100644),
        (0o100000, "=rwx" , 0o100755),
        (0o100777, "-w"   , 0o100577),
        (0o100777, "-x"   , 0o100666),
    ];
    for t in tests {
        let (before, mode, expected) = t;
        run_test(before, mode, expected);
    }

    unsafe {
        umask(last);
    }
}

#[test]
fn test_chmod_ugo_copy() {
    let tests = vec![
        (0o100070, "u=g", 0o100770),
        (0o100005, "g=o", 0o100055),
        (0o100200, "o=u", 0o100202),
        (0o100710, "u-g", 0o100610),
        (0o100250, "u+g", 0o100750),
    ];

    for t in tests {
        let (before, mode, expected) = t;
        run_test(before, mode, expected);
    }
}

#[test]
fn test_chmod_many_options() {
    let _guard = UMASK_MUTEX.lock();

    let original_umask = unsafe {
        umask(0)
    };

    let tests = vec![
        (0o100444, "-r,a+w", 0o100222),
    ];

    for t in tests {
        let (before, mode, expected) = t;
        run_test(before, mode, expected);
    }

    unsafe {
        umask(original_umask);
    }
}
