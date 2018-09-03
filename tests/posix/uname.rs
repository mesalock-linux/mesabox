//
// Copyright (c) 2018, The MesaLock Linux Project Contributors
// All rights reserved.
//
// This work is licensed under the terms of the BSD 3-Clause License.
// For a copy, see the LICENSE file.
//

use assert_cmd::prelude::*;
use predicates::prelude::*;
use std::process::Command;

const NAME: &str = "uname";

#[test]
fn test_default() {
    let output = Command::new("uname").output().unwrap();
    new_cmd!()
        .assert()
        .success()
        .stdout(predicate::eq(&output.stdout as &[u8]))
        .stderr(predicate::eq(&output.stderr as &[u8]));
}

#[test]
fn test_all() {
    // In the POSIX standard `-a` means `-mnrsvo`, since `-p` and `-i` are not portable.
    // http://pubs.opengroup.org/onlinepubs/9699919799/utilities/uname.html
    let output = Command::new("uname").arg("-mnrsvo").output().unwrap();
    new_cmd!()
        .arg("-a")
        .assert()
        .success()
        .stdout(predicate::eq(&output.stdout as &[u8]))
        .stderr(predicate::eq(&output.stderr as &[u8]));
}
