// Copyright (c) 2018, The MesaLock Linux Project Contributors
// All rights reserved.
// 
// This work is licensed under the terms of the BSD 3-Clause License.
// For a copy, see the LICENSE file.

use super::{ArgsIter, UtilSetup, UtilWrite, Result};

use clap::Arg;
use chrono::Local;
use crossbeam;
use failure;
use std::ffi::{OsStr, OsString};
use std::io::{self, Write};
use libc::{self, AF_INET};
use nix;
use nix::errno::Errno;
use nix::unistd;
use nix::sys::signal::{self, Signal, SigSet};
use nix::sys::socket;
use nix::sys::uio::IoVec;
use std::net::{SocketAddr, ToSocketAddrs};
use std::os::unix::io::RawFd;
use std::str::FromStr;
use std::f64;
use std::u64;
use std::result::Result as StdResult;
use std::thread;
use std::time::Duration;
use std::mem;
use std::slice;
use std::sync::atomic::{AtomicBool, Ordering};

pub(crate) const NAME: &str = "ping";
pub(crate) const DESCRIPTION: &str = "Send ICMP ECHO_REQUEST packets to hosts on the network";

struct Stats {
    sent: u64,
    received: u64,
    // XXX: f32 is probably good enough tbh
    roundtrips: Vec<f64>,
}

impl Stats {
    pub fn packet_loss(&self) -> f32 {
        100.0 - (self.received as f32 / self.sent as f32 * 100.0)
    }
}

#[repr(u8)]
#[derive(Copy, Clone)]
enum IcmpKind {
    EchoRequestIpv4 = 8,
    EchoRequestIpv6 = 128,
    EchoReplyIpv4 = 0,
    EchoReplyIpv6 = 129,
}

// FIXME: probably need to change byte order or something when sending

#[repr(packed)]
struct IcmpPacket {
    kind: IcmpKind,
    code: u8,
    checksum: u16,
    id: u16,
    seq_num: u16,
    pub timestamp: i64,
    pub nanos: u32,
}

impl IcmpPacket {
    pub fn new(kind: IcmpKind, id: u16, seq_num: u16) -> Self {
        //let checksum = !((!(((kind as u16) << 8) | 0)).overflowing_add(!(0)).0.overflowing_add(!(id)).0.overflowing_add(!(seq_num)).0);
        /*let mut checksum = 0u16;
        for &value in &[(kind as u16) << 8, 0, id, seq_num] {
            checksum = checksum.overflowing_add(value).0;
            if value >= checksum {
                checksum += 1;
            }
        }*/
        
        //let checksum = ((kind as u16) << 8)
        let time = Local::now();
        let mut result = Self {
            kind: kind,
            code: 0,
            checksum: 0,
            id: id,
            seq_num: seq_num,
            timestamp: time.timestamp(),
            nanos: time.timestamp_subsec_nanos(),
        };

        result.checksum = result.calculate_checksum();

        result
    }

    pub fn calculate_checksum(&self) -> u16 {
        match self.kind {
            IcmpKind::EchoRequestIpv4 | IcmpKind::EchoReplyIpv4 => self.calculate_checksum_ipv4(),
            _ => self.calculate_checksum_ipv6(),
        }
    }

    fn calculate_checksum_ipv4(&self) -> u16 {
        let ptr = self as *const Self as *const u16;
        let buf = unsafe { slice::from_raw_parts(ptr, mem::size_of::<IcmpPacket>()) };

        let mut count = mem::size_of::<IcmpPacket>();
        let mut sum = 0u32;
        let mut i = 0;
        while count > 1 {
            sum = sum.overflowing_add(buf[i] as u32).0;
            i += 1;
            count -= 2;
        }
        if count == 1 {
            // XXX: this won't happen atm
        }
        sum = (sum >> 16) + (sum & 0xFFFF);
        sum += sum >> 16;
        !sum as u16
    }

    fn calculate_checksum_ipv6(&self) -> u16 {
        unimplemented!()
    }

    pub fn validate_checksum(&self) -> bool {
        self.calculate_checksum() == 0
    }

    pub fn as_bytes(&self) -> &[u8] {
        unsafe { slice::from_raw_parts(self as *const Self as *const u8, mem::size_of::<IcmpPacket>()) }
    }
}

enum SocketType {
    Raw,
    Dgram,
}

struct IcmpSocket {
    fd: RawFd,
    kind: SocketType,
}

impl IcmpSocket {
    pub fn new() -> Result<Self> {
        let (kind, socktype) = if unistd::geteuid().is_root() {
            (SocketType::Raw, libc::SOCK_RAW)
        } else {
            (SocketType::Dgram, libc::SOCK_DGRAM)
        };
        let sock = unsafe { libc::socket(AF_INET, socktype, libc::IPPROTO_ICMP) };
        if sock < 0 {
            Err(io::Error::last_os_error().into())
        } else {
            Ok(Self {
                fd: sock as RawFd,
                kind: kind,
            })
        }
    }

    // XXX: OsString?
    pub fn resolve_hostname(hostname: &str) -> io::Result<SocketAddr> {
        let mut addrs = (hostname, /*8080*/0).to_socket_addrs()?;

        // XXX: which address?
        while let Some(addr) = addrs.next() {
            //if addr.is_ipv4() {
                return Ok(addr);
            //}
        }
        // FIXME: this stuff is wrong but need to test
        Err(io::Error::last_os_error())
        //Ok(addrs.next().unwrap())

//        Ok("".to_string())
    }

    pub fn send(&self, addr: &socket::SockAddr, packet: &IcmpPacket) -> Result<usize> {
        let bytes = packet.as_bytes();
        let size = bytes.len();
        match self.kind {
            SocketType::Dgram => {
                //socket::send(sock.fd, &request.as_bytes(), socket::MsgFlags::empty())?;//, socket::MsgFlags::MSG_DONTWAIT)?;
                //socket::sendto(self.fd, &bytes, &addr, socket::MsgFlags::empty())?;//, socket::MsgFlags::MSG_DONTWAIT)?;
            }
            SocketType::Raw => {
                // TODO: need to add IP header info to size and stuff like that
                //socket::
            }
        }
        let iov = IoVec::from_slice(&bytes);
        let iov_slice = &[iov];
        let res = socket::sendmsg(self.fd, iov_slice, &[], socket::MsgFlags::empty(), Some(&addr))?;
        // FIXME: not complete
        if res < size {
            println!("partial write");
        }
        Ok(res)
    }
}

impl Drop for IcmpSocket {
    fn drop(&mut self) {
        // we can't handle any errors so just ignore them
        let _ = socket::shutdown(self.fd, socket::Shutdown::Both);
    }
}

struct Options {
    pub count: Option<u64>,
    pub recv_wait: libc::c_int,
    pub between_wait: f64,
    pub sock_addr: SocketAddr,
    pub expected_size: usize,
}

// TODO: this needs to catch SIGINT and dump out the stats when it does
pub fn execute<S, T>(setup: &mut S, args: T) -> Result<()>
where
    S: UtilSetup,
    T: ArgsIter,
{
    let matches = {
        let app = util_app!(NAME)
                    .arg(Arg::with_name("count")
                                .short("c")
                                .takes_value(true)
                                .value_name("COUNT")
                                .validator_os(is_number)
                                .help("stop after sending and receiving COUNT ECHO_RESPONSE \
                                        packets"))
                     .arg(Arg::with_name("waittime")
                                .short("W")
                                .takes_value(true)
                                .value_name("WAIT_TIME")
                                .validator_os(is_number)
                                .help("wait WAIT_TIME milliseconds for a reply after a packet is \
                                         sent.  If a reply arrives later, it will not be printed \
                                         but will be counted as received"))
                     .arg(Arg::with_name("packet_wait")
                                .short("i")
                                .takes_value(true)
                                .value_name("WAIT_TIME")
                                .validator_os(is_valid_wait_time)
                                .help("wait WAIT_TIME seconds between sending each packet (the \
                                        default is 1 second)"))
                     .arg(Arg::with_name("HOST")
                                .index(1)
                                .required(true));
    
        app.get_matches_from_safe(args)?
    };
    
    let count = if matches.is_present("count") {
        // this is fine because of the validator
        Some(u64::from_str(matches.value_of("count").unwrap()).unwrap())
    } else {
        None
    };

    let wait_time = matches.value_of("waittime")
                           .and_then(|v| libc::c_int::from_str(v).ok())
                           .unwrap_or(1000);
    
    let between_packet_wait = matches.value_of("packet_wait")
                                     .and_then(|v| f64::from_str(v).ok())
                                     .unwrap_or(1.0);

    let hostname_os = matches.value_of_os("HOST").unwrap();
    let hostname = match hostname_os.to_str() {
        Some(s) => s,
        None => Err(failure::err_msg(format!("invalid hostname: {}", hostname_os.to_string_lossy())).compat())?,
    };
    let resolved_ip = IcmpSocket::resolve_hostname(hostname)?;

    let options = Options {
        count: count,
        recv_wait: wait_time,
        between_wait: between_packet_wait,
        sock_addr: resolved_ip,
        expected_size: 20 + mem::size_of::<IcmpPacket>(),  // "20" is the size of the IP header
    };

    let sock = IcmpSocket::new()?;

    let mut stats = Stats { sent: 0, received: 0, roundtrips: vec![] };

    writeln!(setup.output(), "PING {} ({}): {} data bytes", hostname, resolved_ip.ip(), options.expected_size)?;

    let should_stop = AtomicBool::new(false);
    {
        let mut stdout_ref = setup.output();
        crossbeam::scope(|scope| {
            let should_stop_ref = &should_stop;
            let stdout_ref = &mut *stdout_ref;
            let stats_ref = &mut stats;

            let child = scope.spawn(move || {
                let res = ping_socket(sock, stats_ref, stdout_ref, should_stop_ref, options);
                if !should_stop_ref.load(Ordering::Acquire) {
                    // FIXME: pretty sure there's a race condition where user causes a SIGINT right after the above check, so two SIGINTs will occur and thus the program will die
                    // send a SIGINT to trigger the sigwait() below (XXX: there is probably a cleaner way to do this)
                    signal::kill(unistd::getpid(), Signal::SIGINT)?;
                }
                res
            });

            let mut set = signal::SigSet::empty();
            set.add(Signal::SIGINT);
            set.wait()?;

            should_stop.store(true, Ordering::Release);
        
            child.join()
        })?;
    }

    print_stats(setup, &hostname, &stats)
}

fn ping_socket<O>(sock: IcmpSocket, stats: &mut Stats, stdout: &mut O, should_stop: &AtomicBool, mut options: Options) -> Result<()>
where
    O: for<'a> UtilWrite<'a>,
{
    let pid: libc::pid_t = unistd::getpid().into();
    // there are probably better ways to do this, but the ident needs to be unique per call
    let ident = (pid as u16).overflowing_add(Local::now().timestamp_millis() as u16).0;
    let mut seq_num = 0;

    let millis = Duration::from_millis(((options.between_wait - (options.between_wait as u64 as f64)) * 1000.0) as u64);
    let duration = Duration::from_secs(options.between_wait as u64) + millis;

    let icmp_kind = if options.sock_addr.is_ipv4() {
        IcmpKind::EchoRequestIpv4
    } else {
        IcmpKind::EchoRequestIpv6
    };

    let addr = socket::SockAddr::new_inet(socket::InetAddr::from_std(&options.sock_addr));

    let mut stdout = stdout.lock_writer()?;
    while options.count.unwrap_or(1) > 0 && !should_stop.load(Ordering::Acquire) {
        let request = IcmpPacket::new(icmp_kind, ident, seq_num);
        // FIXME: should print error rather than return
        sock.send(&addr, &request)?;
        stats.sent += 1;

        // TODO: wait some time (1 second? or whatever wait time is specified)
        //thread::sleep(Duration::from_secs(1));
        // XXX: need to use specified time rather than 1
        nix::poll::poll(&mut [nix::poll::PollFd::new(sock.fd, nix::poll::EventFlags::POLLIN)], options.recv_wait)?;

        // try to receive the ICMP reply
        let mut data = [0; 56];
        let iov = IoVec::from_mut_slice(&mut data);
        let iov_slice = &[iov];
        // FIXME: should not use fixed size data
        loop {
            // TODO: add timeout or something
            match socket::recvmsg::<()>(sock.fd, iov_slice, None, socket::MsgFlags::empty()) {
                Ok(msg) => {
                    // FIXME: not right so
                    if msg.bytes == options.expected_size {
                        // FIXME: need to check length and stuff
                        let packet = unsafe { &*(iov_slice[0].as_slice()[20..].as_ptr() as *const IcmpPacket) };
                        if packet.validate_checksum() {
                            let current_time = Local::now();
                            // TODO: if this returns None the packet has been tampered with and should probably be considered invalid
                            let large_num = current_time.timestamp().checked_sub(packet.timestamp).unwrap();
                            let small_num = current_time.timestamp_subsec_nanos().checked_sub(packet.nanos).unwrap();
                            let time = (large_num * 1000) as f64 + small_num as f64 / 1_000_000.0;
                            // FIXME: "?" could probably just be the sent to address
                            // FIXME: ttl
                            let ip = if let Some(socket::SockAddr::Inet(addr)) = msg.address {
                                format!("{}", addr.ip())
                            } else {
                                "?".to_owned()
                            };
                            let seq_num = packet.seq_num;
                            writeln!(stdout, "{} bytes from {}: icmp_seq={} ttl={} time={:.3} ms", msg.bytes, ip, seq_num, 0, time)?;
                            stats.roundtrips.push(time);
                            stats.received += 1;
                            break;
                        } else {
                            // TODO: checksum is invalid
                            //writeln!(stdout, ")
                        }
                    } else {
                        // TODO: msg is wrong
                    }
                }
                Err(nix::Error::Sys(Errno::EAGAIN)) => break,
                Err(f) => {
                    Err(f)?
                }
            }
        }

        if let Some(val) = options.count {
            options.count = Some(val - 1);
        }

        seq_num += 1;

        if !should_stop.load(Ordering::Acquire) {
            // TODO: all the sleeps/polls/whatever need to be interrupted on SIGINT
            // TODO: do not sleep if flooding
            thread::sleep(duration.clone());
        }
    }

    Ok(())
}

fn is_number(val: &OsStr) -> StdResult<(), OsString> {
    if val.to_str().and_then(|s| libc::c_int::from_str(s).ok()).is_some() {
        Ok(())
    } else {
        Err(OsString::from(format!("'{}' is not a number", val.to_string_lossy())))
    }
}

fn is_valid_wait_time(val: &OsStr) -> StdResult<(), OsString> {
    if let Some(num) = val.to_str().and_then(|s| f64::from_str(s).ok()) {
        let (uid, euid) = (unistd::getuid(), unistd::geteuid());
        if uid == euid && num < 0.1 {
            Err(OsString::from("only the super-user can use wait values < 0.1 s"))
        } else {
            Ok(())
        }
    } else {
        Err(OsString::from(format!("'{}' is not a number", val.to_string_lossy())))
    }
}

fn print_stats<S>(setup: &mut S, hostname: &str, stats: &Stats) -> Result<()>
where
    S: UtilSetup,
{
    // ignore SIGINT completely while printing stats
    let old_set = SigSet::thread_get_mask()?;
    let mut set = old_set.clone();
    set.add(Signal::SIGINT);
    set.thread_set_mask()?;

    let mut stdout = setup.output();
    let mut stdout = stdout.lock_writer()?;

    writeln!(stdout, "\n--- {} ping statistics ---", hostname)?;
    writeln!(stdout, "{} packets transmitted, {} packets received, {}% packet loss", stats.sent, stats.received, stats.packet_loss())?;

    if stats.received > 0 {
        let mut min = f64::MAX;
        let mut max = f64::MIN;
        let mut avg = 0.0;
        for &time in &stats.roundtrips {
            min = min.min(time);
            max = max.max(time);
            avg += time / stats.roundtrips.len() as f64;
        }
        let mut variance = 0.0;
        for &time in &stats.roundtrips {
            variance += ((time - avg) * (time - avg)) / stats.roundtrips.len() as f64;
        }
        let stddev = variance.sqrt();
        writeln!(stdout, "round-trip min/avg/max/stddev = {:.3}/{:.3}/{:.3}/{:.3} ms", min, avg, max, stddev)?;
    }

    // restore the old signal mask for this thread
    old_set.thread_set_mask()?;

    Ok(())
}
