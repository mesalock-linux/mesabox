extern crate mesabox;
#[macro_use]
extern crate lazy_static;
extern crate libc;
extern crate tempfile;

mod util;
#[macro_use]
mod macros;

// contains all the "mod"s needed to test the utils
include!(concat!(env!("OUT_DIR"), "/test_utils.rs"));
