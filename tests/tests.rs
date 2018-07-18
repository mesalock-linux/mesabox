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

mod util;
#[macro_use]
mod macros;

macro_rules! generate_fns {
    ($($group:ident { $(($util:ident, $feature:expr)),+ }),*) => {
        $(
            mod $group {
                $(
                    #[cfg(feature = $feature)]
                    mod $util;
                )+
            }
        )*
    }
}

// calls generate_fns!()
include!("../src/util_list.rs");
