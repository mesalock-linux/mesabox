//
// Copyright (c) 2018, The MesaLock Linux Project Contributors
// All rights reserved.
//
// This work is licensed under the terms of the BSD 3-Clause License.
// For a copy, see the LICENSE file.
//

extern crate mesabox;
#[macro_use]
extern crate lazy_static;
extern crate libc;
extern crate tempfile;
extern crate assert_cmd;
extern crate assert_fs;
extern crate predicates;

mod util;
#[macro_use]
mod macros;

// contains all the "mod"s needed to test the utils
include!(concat!(env!("OUT_DIR"), "/test_utils.rs"));
