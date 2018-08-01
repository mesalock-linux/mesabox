//
// Copyright (c) 2018, The MesaLock Linux Project Contributors
// All rights reserved.
//
// This work is licensed under the terms of the BSD 3-Clause License.
// For a copy, see the LICENSE file.
//

extern crate libmesabox as mesabox;
#[macro_use]
extern crate lazy_static;
extern crate libc;
extern crate tempfile;
extern crate assert_cmd;
extern crate assert_fs;
extern crate predicates;
extern crate timebomb;

#[macro_use]
mod macros;

use std::path::PathBuf;
use assert_cmd::cargo;

lazy_static! {
    pub static ref BIN_PATH: PathBuf = cargo::main_binary_path().unwrap();
}
include!("../libmesabox/src/util/build/import.rs");

macro_rules! generate_fns {
    ($($group:ident { $(($util:tt, $feature:expr)),+ }),*) => {
        import_utils!($($group { $(($util, $feature)),+ }),*);
    }
}

// calls generate_fns!()
include!("../libmesabox/src/util_list.rs");
