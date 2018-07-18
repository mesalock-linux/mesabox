//
// Copyright (c) 2017-2018, The MesaLock Linux Contributors
// All rights reserved.
// 
// This work is licensed under the terms of the BSD 3-Clause License.
// For a copy, see the LICENSE file.
//

use {ArgsIter, Result, MesaError, UtilSetup, UtilRead, UtilWrite};

use fnv::FnvHashMap as HashMap;
use nix;
use nix::unistd::{self, Pid};
use nix::mount::{self, MsFlags};
use nix::sys::reboot::{self, RebootMode};
use nix::sys::signal::{self, Signal, SigSet, SigmaskHow};
use nix::sys::wait::{self, WaitPidFlag, WaitStatus};
use libc;
use std::cmp::Ordering;
use std::collections::BinaryHeap;
use std::env;
use std::fs::File;
use std::io::{self, BufRead, BufReader, Write};
use std::os::unix::ffi::OsStrExt;
use std::os::unix::process::CommandExt;
use std::process::{Child, Command};
use std::ffi::OsStr;
use std::result::Result as StdResult;

const NAME: &str = "init";
pub(crate) const DESCRIPTION: &str = "initialize the system on startup";

const SIGNALS: &[(Signal, fn(&mut HashMap<Pid, InittabEntry>) -> StdResult<(), InitError>)] = &[
    (Signal::SIGUSR1, halt_handler),
    (Signal::SIGUSR2, poweroff_handler),
    (Signal::SIGTERM, reset_handler),
    (Signal::SIGCHLD, reap_handler),
];

#[derive(Debug, Fail)]
enum InitError {
    #[fail(display = "{}", _0)]
    Nix(#[cause] nix::Error),

    #[fail(display = "{}", _0)]
    Io(#[cause] io::Error),

    #[fail(display = "cannot execute the entry's process as it has no name")]
    NamelessCommand,
}

// NOTE: the ordering is significant (the greater the value the earlier it will appear in the
//       generated heap)
type InittabAction = u8;

const NOTHING: InittabAction = 0;
const OFF: InittabAction = 1;
const POWERWAIT: InittabAction = 2;
const POWEROKWAIT: InittabAction = 3;
const POWERFAIL: InittabAction = 4;
const POWERFAILNOW: InittabAction = 5;
const CTRLALTDEL: InittabAction = 6;
const KBREQUEST: InittabAction = 7;
const ONDEMAND: InittabAction = 8;
const RESPAWN: InittabAction = 9;
const WAIT: InittabAction = 10;
const ONCE: InittabAction = 11;
const BOOTWAIT: InittabAction = 12;
const BOOT: InittabAction = 13;
const SYSINIT: InittabAction = 14;
const INITDEFAULT: InittabAction = 15;

#[derive(Debug)]
struct InittabEntry {
    id: Vec<u8>,
    runlevels: Vec<u8>,
    action: InittabAction,
    process: Vec<u8>,
}

impl Ord for InittabEntry {
    fn cmp(&self, other: &Self) -> Ordering {
        let order = self.action.cmp(&other.action);
        if order == Ordering::Equal {
            other.id.cmp(&self.id)
        } else {
            order
        }
    }
}

impl PartialOrd for InittabEntry {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

// XXX: might want to check the id too
impl PartialEq for InittabEntry {
    fn eq(&self, other: &Self) -> bool {
        self.action == other.action
    }
}

impl Eq for InittabEntry {}

impl InittabEntry {
    pub fn start(self, runlevel: u8, respawn: &mut HashMap<Pid, InittabEntry>) -> StdResult<Option<Self>, (Vec<u8>, InitError)> {
        Ok(match self.action {
            BOOT | SYSINIT | BOOTWAIT => {
                self.spawn_child(&[SYSINIT, BOOTWAIT]).map_err(|e| (self.id, e))?;
                None
            }
            ONCE | WAIT => {
                if self.runlevels.contains(&runlevel) {
                    self.spawn_child(&[WAIT]).map_err(|e| (self.id, e))?;
                }
                None
            }
            RESPAWN => {
                let child = self.spawn_child(&[]).map_err(|e| (self.id.clone(), e))?;
                respawn.insert(Pid::from_raw(child.id() as _), self);
                None
            }
            INITDEFAULT => {
                // TODO: print error saying only one initdefault is allowed (and that any after the first will be ignored)
                None
            }
            _ => {
                // TODO: power-related stuff and kbrequest
                unimplemented!()
            }
        })
    }

    fn spawn_child(&self, waiters: &[u8]) -> StdResult<Child, InitError> {
        let mut child = run(&self.process)?;
        if waiters.contains(&self.action) {
            child.wait().map_err(|e| InitError::Io(e))?;
        }
        Ok(child)
    }
}

fn parse_entry(input: &[u8], line: usize) -> Result<Option<InittabEntry>> {
    use util::string_to_err;

    if let Some(entry) = try_parse_entry(input) {
        if entry.action != OFF {
            return Ok(Some(entry));
        }
    } else {
        for &byte in input {
            match byte {
                b'#' | b'\n' => break,
                byte if byte != b' ' => {
                    return string_to_err(Err(format!("invalid entry on line {}", line)));
                }
                _ => {}
            }
        }
    }

    Ok(None)
}

fn try_parse_entry(input: &[u8]) -> Option<InittabEntry> {
    let mut entry = input.splitn(4, |&ch| ch == b':');

    let id = entry.next()?;
    let runlevels = entry.next()?;
    let action = entry.next()?;
    let mut process = entry.next()?;
    if process.ends_with(&[b'\n']) {
        process = &process[..process.len() - 1];
    }

    let action = match action {
        b"respawn" => RESPAWN,
        b"wait" => WAIT,
        b"once" => ONCE,
        b"boot" => BOOT,
        b"bootwait" => BOOTWAIT,
        b"off" => OFF,
        b"ondemand" => ONDEMAND,
        b"initdefault" => INITDEFAULT,
        b"sysinit" => SYSINIT,
        b"powerwait" => POWERWAIT,
        b"powerfail" => POWERFAIL,
        b"powerokwait" => POWEROKWAIT,
        b"powerfailnow" => POWERFAILNOW,
        b"ctrlaltdel" => CTRLALTDEL,
        b"kbrequest" => KBREQUEST,
        b"" => NOTHING,
        _ => {
            return None;
        }
    };

    // TODO: validate process/runlevel based on action type

    Some(InittabEntry {
        id: id.to_owned(),
        runlevels: runlevels.to_owned(),
        action: action,
        process: process.to_owned(),
    })
}

fn parse_inittab<R: BufRead>(mut reader: R) -> Result<BinaryHeap<InittabEntry>> {
    let mut result = BinaryHeap::new();

    let mut line = vec![];
    let mut count = 1;
    while reader.read_until(b'\n', &mut line)? > 0 {
        if let Some(entry) = parse_entry(&line, count)? {
            result.push(entry);
        }
        count += 1;
        line.clear();
    }

    Ok(result)
}

fn parse_system_inittab() -> Result<BinaryHeap<InittabEntry>> {
    let file = File::open("/etc/inittab")?;
    let reader = BufReader::new(file);

    parse_inittab(reader)
}

// TODO: handle + prefix
fn run(cmd: &[u8]) -> StdResult<Child, InitError> {
    println!("[+] init: run {}", String::from_utf8_lossy(cmd));
    let mut args = cmd.split(|&v| v == b' ');

    if let Some(cmdname) = args.next() {
        let mut command = Command::new(OsStr::from_bytes(&cmdname));
        for arg in args {
            if arg.len() != 0 {
                command.arg(OsStr::from_bytes(&arg));
            }
        }
        command.before_exec(|| {
            // the only way this can fail is if we provide an invalid signal number
            let _ = setup_default_signals();

            // TODO: open the new terminal device
            Ok(())
        });
        command.spawn().map_err(|e| InitError::Io(e))
    } else {
        Err(InitError::NamelessCommand)
    }
}

fn halt_handler(_respawn: &mut HashMap<Pid, InittabEntry>) -> StdResult<(), InitError> {
    try_reboot(RebootMode::RB_HALT_SYSTEM)
}

fn poweroff_handler(_respawn: &mut HashMap<Pid, InittabEntry>) -> StdResult<(), InitError> {
    try_reboot(RebootMode::RB_POWER_OFF)
}

fn reset_handler(_respawn: &mut HashMap<Pid, InittabEntry>) -> StdResult<(), InitError> {
    try_reboot(RebootMode::RB_AUTOBOOT)
}

fn reap_handler(respawn: &mut HashMap<Pid, InittabEntry>) -> StdResult<(), InitError> {
    use self::WaitStatus::*;

    loop {
        match wait::waitpid(None, Some(WaitPidFlag::WNOHANG)) {
            Err(_) | Ok(StillAlive) => break,
            // XXX: other conditions?
            Ok(Exited(pid, _)) | Ok(Signaled(pid, _, _)) => {
                if let Some(entry) = respawn.remove(&pid) {
                    // XXX: if spawning fails what should happen?  keep trying?
                    let child = run(&entry.process)?;
                    respawn.insert(Pid::from_raw(child.id() as _), entry);
                }
            }
            _ => {}
        }
    }
    Ok(())
}

fn try_reboot(kind: RebootMode) -> StdResult<(), InitError> {
    reboot::reboot(kind).map_err(|e| InitError::Nix(e))?;
    Ok(())
}

fn setup_signals() -> nix::Result<SigSet> {
    let set = SigSet::all();

    signal::sigprocmask(SigmaskHow::SIG_BLOCK, Some(&set), None)?;

    Ok(set)
}

fn setup_default_signals() -> nix::Result<SigSet> {
    let set = SigSet::all();

    signal::sigprocmask(SigmaskHow::SIG_UNBLOCK, Some(&set), None)?;

    Ok(set)
}

#[allow(dead_code)]
fn reset_signals(set: &SigSet) -> nix::Result<()> {
    signal::sigprocmask(SigmaskHow::SIG_UNBLOCK, Some(set), None)
}

pub fn execute<S, T>(setup: &mut S, _args: T) -> Result<()>
where
    S: UtilSetup,
    T: ArgsIter,
{
    if unistd::getpid() != Pid::from_raw(1) {
        display_msg!(setup.error(), "already running")?;
        return Err(MesaError {
            progname: None,
            exitcode: 1,
            err: None,
        });
    }

    let (stdin, stdout, stderr) = setup.stdio();
    let mut stdin = stdin.lock_reader()?;
    let mut stdout = stdout.lock_writer()?;
    let mut stderr = stderr.lock_writer()?;

    display_msg!(stdout, "starting")?;

    env::set_current_dir("/")?;
    unistd::setsid()?;
    // NOTE: we are using libc here because it should avoid allocations/memory leaks
    unsafe {
        libc::putenv(b"HOME=/\0".as_ptr() as *mut _);
        libc::putenv(b"PATH=/sbin:/bin:/usr/sbin:/usr/bin\0".as_ptr() as *mut _);
        libc::putenv(b"SHELL=/bin/sh\0".as_ptr() as *mut _);
    }

    let mut inittab = parse_system_inittab()?;

    // block signals to prevent init from dying
    let set = setup_signals()?;

    mount_fs()?;

    let runlevel = determine_runlevel(&mut inittab, &mut stdin, &mut stderr);
    let (mut respawn, _inittab) = start_entries(&mut inittab, runlevel, &mut stderr)?;

    // at this point, we should never return
    loop {
        if let Ok(sig) = set.wait() {
            for &(valid_sig, handler) in SIGNALS {
                if sig == valid_sig {
                    if let Err(f) = handler(&mut respawn) {
                        let _ = display_err!(stderr, "handler for {:?} failed with {}", sig, f);
                    }
                    break;
                }
            }
        }
    }
}

fn mount_fs() -> nix::Result<()> {
    // XXX: this should be done something started in /etc/inittab, but we have no mount binary
    //      currently and thus must be done in init
    // mount -n -t proc proc /proc
    let proc_mount_flags = MsFlags::MS_NOSUID | MsFlags::MS_NODEV | MsFlags::MS_NOEXEC | MsFlags::MS_RELATIME;
    let _ = mount::mount(Some("proc"), "/proc", Some("proc"), proc_mount_flags, Some("mode=0555"))?;

    // mount -n -t devtmpfs devtmpfs /dev
    let dev_mount_flags = MsFlags::MS_NOSUID | MsFlags::MS_RELATIME;
    let _ = mount::mount(Some("dev"), "/dev", Some("devtmpfs"), dev_mount_flags, Some("mode=0755"))?;

    // mount -n -t sysfs sysfs /sys
    let sys_mount_flags = MsFlags::MS_NOSUID | MsFlags::MS_NODEV | MsFlags::MS_NOEXEC | MsFlags::MS_RELATIME;
    mount::mount(Some("sysfs"), "/sys", Some("sysfs"), sys_mount_flags, Some("mode=0555"))
}

fn start_entries<E>(inittab: &mut BinaryHeap<InittabEntry>, runlevel: u8, stderr: &mut E) -> io::Result<(HashMap<Pid, InittabEntry>, BinaryHeap<InittabEntry>)>
where
    E: Write,
{
    let mut respawn = HashMap::default();
    let mut new_heap = BinaryHeap::new();
    while let Some(entry) = inittab.pop() {
        match entry.start(runlevel, &mut respawn) {
            Ok(Some(entry)) => {
                new_heap.push(entry);
            }
            Ok(None) => {}
            Err((id, f)) => {
                // FIXME: these should use display_err!
                write!(stderr, "failed on entry '")?;
                stderr.write_all(&id)?;
                writeln!(stderr, "': {}", f)?;
            }
        }
    }
    // XXX: is new_heap needed?  prob for ondemand
    Ok((respawn, new_heap))
}

fn determine_runlevel<I, E>(inittab: &mut BinaryHeap<InittabEntry>, stdin: &mut I, stderr: &mut E) -> u8
where
    I: BufRead,
    E: Write,
{
    inittab.pop().and_then(|entry| {
        if entry.action == INITDEFAULT {
            if entry.runlevels.len() == 1 {
                Some(entry.runlevels[0])
            } else {
                let _ = display_err!(stderr, "only one runlevel may be given to initdefault (found {})", entry.runlevels.len());
                None
            }
        } else {
            inittab.push(entry);
            None
        }
    }).unwrap_or_else(|| {
        // ask for runlevel because there was no default
        let mut input = Vec::new();
        loop {
            match stdin.read_until(b'\n', &mut input) {
                Ok(_) => {
                    if (input.len() == 1 && input[0] != b'\n') || (input.len() == 2 && input[1] == b'\n') {
                        return input[0];
                    } else {
                        let _ = display_err!(stderr, "need exactly one runlevel");
                    }
                }
                Err(f) => {
                    let _ = display_err!(stderr, "{}", f);
                }
            }
            input.clear();
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn determine_runlevel_default() {
        let stdin = b"23\n2\n";
        let mut stderr = vec![];

        let mut inittab = BinaryHeap::new();
        inittab.push(InittabEntry {
            id: b"id".to_vec(),
            runlevels: vec![b'1'],
            action: INITDEFAULT,
            process: vec![],
        });
        inittab.push(InittabEntry {
            id: vec![b'2'],
            runlevels: vec![b'2'],
            action: RESPAWN,
            process: b"/bin/mgetty".to_vec(),
        });
        inittab.push(InittabEntry {
            id: b"si".to_vec(),
            runlevels: vec![],
            action: SYSINIT,
            process: b"/etc/init.sh".to_vec(),
        });

        let runlevel = determine_runlevel(&mut inittab, &mut &stdin[..], &mut stderr);
        assert_eq!(runlevel, b'1');
        assert!(stderr.is_empty());
    }

    #[test]
    fn determine_runlevel_stdin() {
        let stdin = b"23\n2\n";
        let mut stderr = vec![];

        let mut inittab = BinaryHeap::new();
        inittab.push(InittabEntry {
            id: b"id".to_vec(),
            runlevels: b"12".to_vec(),
            action: INITDEFAULT,
            process: vec![],
        });
        inittab.push(InittabEntry {
            id: vec![b'2'],
            runlevels: vec![b'2'],
            action: RESPAWN,
            process: b"/bin/mgetty".to_vec(),
        });
        inittab.push(InittabEntry {
            id: b"si".to_vec(),
            runlevels: vec![],
            action: SYSINIT,
            process: b"/etc/init.sh".to_vec(),
        });

        let runlevel = determine_runlevel(&mut inittab, &mut &stdin[..], &mut stderr);
        assert!(stderr.len() > 0);
        assert_eq!(stderr.into_iter().filter(|&byte| byte == b'\n').count(), 2);
        assert_eq!(runlevel, b'2');
    }

    #[test]
    fn parse_inittab_valid_simple() {
        let data = include_bytes!("test_assets/valid_simple.txt");

        let correct = &[
            (&b"id"[..], &b"2"[..], INITDEFAULT, &[][..]),
            (&b"si"[..], &[], SYSINIT, &b"/etc/init.sh"[..]),
            (&b"2"[..], &b"2"[..], RESPAWN, &b"/bin/mgetty"[..]),
        ];

        let result = parse_inittab(&mut &data[..]);
        compare_inittab(correct, result);
    }

    #[test]
    fn parse_inittab_valid_complex() {
        let data = include_bytes!("test_assets/valid_complex.txt");

        let correct = &[
            (&b"id"[..], &b"2"[..], INITDEFAULT, &[][..]),
            (&b"si"[..], &[], SYSINIT, &b"/etc/init.d/rcS"[..]),
            (&b"l0"[..], &b"0"[..], WAIT, &b"/etc/init.d/rc 0"[..]),
            (&b"l1"[..], &b"1"[..], WAIT, &b"/etc/init.d/rc 1"[..]),
            (&b"l2"[..], &b"2"[..], WAIT, &b"/etc/init.d/rc 2"[..]),
            (&b"l3"[..], &b"3"[..], WAIT, &b"/etc/init.d/rc 3"[..]),
            (&b"l4"[..], &b"4"[..], WAIT, &b"/etc/init.d/rc 4"[..]),
            (&b"l5"[..], &b"5"[..], WAIT, &b"/etc/init.d/rc 5"[..]),
            (&b"l6"[..], &b"6"[..], WAIT, &b"/etc/init.d/rc 6"[..]),
            (&b"~"[..], &b"S"[..], WAIT, &b"/sbin/sulogin"[..]),
            (&b"1"[..], &b"23"[..], RESPAWN, &b"/sbin/getty tty1 VC linux"[..]),
            (&b"2"[..], &b"23"[..], RESPAWN, &b"/sbin/getty tty2 VC linux"[..]),
            (&b"3"[..], &b"23"[..], RESPAWN, &b"/sbin/getty tty3 VC linux"[..]),
            (&b"4"[..], &b"23"[..], RESPAWN, &b"/sbin/getty tty4 VC linux"[..]),
            (&b"S0"[..], &b"3"[..], RESPAWN, &b"/sbin/getty -L 9600 ttyS0 vt320"[..]),
            (&b"S1"[..], &b"3"[..], RESPAWN, &b"/sbin/mgetty -x0 -D ttyS1"[..]),
            (&b"ca"[..], &[], CTRLALTDEL, &b"/sbin/shutdown -t1 -h now"[..]),
        ];

        let result = parse_inittab(&mut &data[..]);
        compare_inittab(correct, result);
    }

    fn compare_inittab(correct: &[(&[u8], &[u8], InittabAction, &[u8])], result: Result<BinaryHeap<InittabEntry>>) {
        assert!(result.is_ok());
        let mut result = result.unwrap();
        
        for entry in correct {
            let found = result.pop().unwrap();
            assert_eq!(entry.0, &found.id[..]);
            assert_eq!(entry.1, &found.runlevels[..]);
            assert_eq!(entry.2, found.action);
            assert_eq!(entry.3, &found.process[..]);
        }

        assert!(result.is_empty());
    }
}
