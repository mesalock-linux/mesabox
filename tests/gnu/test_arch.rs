//
// Copyright (c) 2018, The MesaLock Linux Project Contributors
// All rights reserved.
//
// This work is licensed under the terms of the BSD 3-Clause License.
// For a copy, see the LICENSE file.
//

use assert_cli;

#[test]
#[cfg(target_arch = "x86_64")]
fn test_x86_64() {
    new_cli!()
        .succeeds()
        .and()
        .stdout().is("x86_64\n")
        .unwrap();
}

#[test]
#[cfg(target_arch = "arm")]
fn test_arm() {
    new_cli!()
        .succeeds()
        .and()
        .stdout().is("arm\n")
        .unwrap();
}

#[test]
#[cfg(target_arch = "aarch64")]
fn test_aarch64() {
    new_cli!()
        .succeeds()
        .and()
        .stdout().is("aarch64\n")
        .unwrap();
}
