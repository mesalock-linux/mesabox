//TODO: find dates of commits by other authors

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
//     Copyright (c) Joao Oliveira
//     Copyright (c) Jian Zeng
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

use clap::Arg;
use platform_info::*;

use std::io::Write;

use {UtilSetup, ArgsIter, Result};

pub const NAME: &str = "uname";
pub const DESCRIPTION: &str = "Print basic system information";

const OPT_ALL: &str = "all";
const OPT_KERNELNAME: &str = "kernel-name";
const OPT_NODENAME: &str = "nodename";
const OPT_KERNELVERSION: &str = "kernel-version";
const OPT_KERNELRELEASE: &str = "kernel-release";
const OPT_MACHINE: &str = "machine";

//FIXME: unimplemented options
//const OPT_PROCESSOR: &'str = "processor";
//const OPT_HWPLATFORM: &'str = "hardware-platform";
const OPT_OS: &str = "operating-system";

fn host_os() -> &'static str {
    if cfg!(target_os = "linux") {
        "GNU/Linux"
    } else if cfg!(target_os = "windows") {
        "Windows NT"
    } else if cfg!(target_os = "freebsd") {
        "FreeBSD"
    } else if cfg!(target_os = "openbsd") {
        "OpenBSD"
    } else if cfg!(target_os = "macos") {
        "Darwin"
    } else if cfg!(target_os = "fuchsia") {
        "Fuchsia"
    } else if cfg!(target_os = "redox") {
        "Redox"
    } else {
        "Unknown"
    }
}

pub fn execute<S, T>(setup: &mut S, args: T) -> Result<()>
where
    S: UtilSetup,
    T: ArgsIter,
{
    let matches = util_app!(NAME)
        .arg(Arg::with_name(OPT_ALL)
            .short("a")
            .long(OPT_ALL)
            .help("Behave as though all of the options -mnrsv were specified."))
        .arg(Arg::with_name(OPT_KERNELNAME)
            .short("s")
            .long(OPT_KERNELNAME)
            .alias("sysname") // Obsolescent option in GNU uname
            .help("print the operating system name."))
        .arg(Arg::with_name(OPT_NODENAME)
            .short("n")
            .long(OPT_NODENAME)
            .help("print the nodename (the nodename may be a name that the system is known by to a communications network)."))
        .arg(Arg::with_name(OPT_KERNELRELEASE)
            .short("r")
            .long(OPT_KERNELRELEASE)
            .alias("release") // Obsolescent option in GNU uname
            .help("print the operating system release."))
        .arg(Arg::with_name(OPT_KERNELVERSION)
            .short("v")
            .long(OPT_KERNELVERSION)
            .help("print the operating system version."))
        //FIXME: unimplemented options
        // .arg(Arg::with_name(OPT_PROCESSOR)
        //     .short("p")
        //     .long(OPT_PROCESSOR)
        //     .help("print the processor type (non-portable)"))
        // .arg(Arg::with_name(OPT_HWPLATFORM)
        //     .short("i")
        //     .long(OPT_HWPLATFORM)
        //     .help("print the hardware platform (non-portable)"))
        .arg(Arg::with_name(OPT_MACHINE)
            .short("m")
            .long(OPT_MACHINE)
            .help("print the machine hardware name."))
        .get_matches_from_safe(args)?;

    let uname = PlatformInfo::new()?;
    let mut output = String::new();

    if matches.is_present(OPT_KERNELNAME) || matches.is_present(OPT_ALL) {
        output.push_str(&uname.sysname());
        output.push_str(" ");
    }
    if matches.is_present(OPT_NODENAME) || matches.is_present(OPT_ALL) {
        output.push_str(&uname.nodename());
        output.push_str(" ");
    }
    if matches.is_present(OPT_KERNELRELEASE) || matches.is_present(OPT_ALL) {
        output.push_str(&uname.release());
        output.push_str(" ");
    }
    if matches.is_present(OPT_KERNELVERSION) || matches.is_present(OPT_ALL) {
        output.push_str(&uname.version());
        output.push_str(" ");
    }
    if matches.is_present(OPT_MACHINE) || matches.is_present(OPT_ALL) {
        output.push_str(&uname.machine());
        output.push_str(" ");
    }
    if matches.is_present(OPT_OS) || matches.is_present(OPT_ALL) {
        output.push_str(host_os());
        output.push_str(" ");
    }

    if output.len() == 0 {
        output.push_str(&uname.sysname());
    }

    writeln!(setup.output(), "{}", output.trim_right())?;

    Ok(())
}
