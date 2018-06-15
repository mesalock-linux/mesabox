//
// Copyright (c) 2017-2018, The MesaLock Linux Project Contributors
// All rights reserved.
// 
// This work is licensed under the terms of the BSD 3-Clause License.
// For a copy, see the LICENSE file.
//

use super::{ArgsIter, UtilRead, UtilWrite, UtilSetup, Result};

use libc;
use nix::fcntl::{self, FcntlArg, OFlag};
use nix::unistd;
use std::ffi::CString;
use nix::sys::stat;
use std::os::unix::io;

const NAME: &str = "getty";
pub const DESCRIPTION: &str = "Open, initialize, and take control of a terminal";

fn open_tty() {
    unistd::close(0).expect("close(0) failed");
    let _ = fcntl::open("/dev/tty1", OFlag::O_RDWR | OFlag::O_NONBLOCK, stat::Mode::empty()).expect("open failed");
    unsafe {
        libc::fchown(0, 0, 0);
        libc::fchmod(0, 0620);
    }

    if unistd::isatty(0).expect("isatty failed") == false {
        panic!("isatty failed");
    }
}

fn ndelay_off(fd: io::RawFd) {
    let original_flags = OFlag::from_bits(fcntl::fcntl(fd, FcntlArg::F_GETFL).expect("fcntl failed")).expect("from_bits failed");
    fcntl::fcntl(fd, FcntlArg::F_SETFL(!OFlag::O_NONBLOCK & original_flags)).expect("fcntl failed");
}

pub(crate) fn execute<I, O, E, T>(setup: &mut UtilSetup<I, O, E>, args: T) -> Result<()>
where
    I: for<'a> UtilRead<'a>,
    O: for<'a> UtilWrite<'a>,
    E: for<'a> UtilWrite<'a>,
    T: ArgsIter,
{
    let _matches = {
        let app = util_app!(NAME, setup);
        app.get_matches_from_safe(args)?
    };

    let pid = unistd::getpid();
    unistd::setsid().expect("setsid failed");
    open_tty();
    ndelay_off(0);

    if unistd::dup2(0, 1).expect("dup2 1 failed") != 1 {
        panic!("dup2 failed");
    }
    if unistd::dup2(0, 2).expect("dup2 2 failed") != 2 {
        panic!("dup2 failed");
    }

    unsafe { libc::ioctl(0, libc::TIOCSCTTY, 1); }

    unistd::tcsetpgrp(0, pid).expect("tcsetpgrp failed");

    unistd::execv(&(CString::new("/bin/ion").unwrap()), &[CString::new("ion").unwrap()]).expect("execv failed");

    Ok(())
}
