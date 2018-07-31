//
// Copyright (c) 2018, The MesaLock Linux Project Contributors
// All rights reserved.
//
// This work is licensed under the terms of the BSD 3-Clause License.
// For a copy, see the LICENSE file.
//

use assert_cmd::prelude::*;
use std::process::Command;

const NAME: &str = "true";

#[test]
fn test_true() {
    new_cmd!().assert().success();
}
