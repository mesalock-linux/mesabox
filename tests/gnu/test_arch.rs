//
// Copyright (c) 2018, The MesaLock Linux Project Contributors
// All rights reserved.
//
// This work is licensed under the terms of the BSD 3-Clause License.
// For a copy, see the LICENSE file.
//

use std::process::Command;
use assert_cmd::prelude::*;

#[test]
#[cfg(target_arch = "x86_64")]
fn test_x86_64() {
    new_cmd!()
        .assert()
        .success()
        .stdout("x86_64\n")
        .stderr("");
}

#[test]
#[cfg(target_arch = "arm")]
fn test_arm() {
    new_cmd!()
        .assert()
        .success()
        .stdout("arm\n")
        .stderr("");
}

#[test]
#[cfg(target_arch = "aarch64")]
fn test_aarch64() {
    new_cmd!()
        .assert()
        .success()
        .stdout("aarch64\n")
        .stderr("");
}
