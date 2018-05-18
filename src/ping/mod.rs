// Copyright (c) 2018, The MesaLock Linux Project Contributors
// All rights reserved.
// 
// This work is licensed under the terms of the BSD 3-Clause License.
// For a copy, see the LICENSE file.

use super::{ArgsIter, UtilSetup, UtilRead, UtilWrite, Result};

use clap::Arg;
use std::ffi::{OsStr, OsString};
use std::io::{self, Read, Write};
use libc::{self, AF_INET, c_int};
use nix;
use nix::errno::Errno;
use nix::unistd;
use nix::sys::socket;
use std::net::{SocketAddr, ToSocketAddrs};
use std::os::unix::io::RawFd;
use std::str::FromStr;
use std::f64;
use std::u64;
use std::result::Result as StdResult;
use std::process;
use std::thread;
use std::time::Duration;

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

#[repr(packed)]
struct IcmpPacket {
    kind: u8,
    code: u8,
    checksum: u16,
    id: u16,
    seq_num: u16,
}

impl IcmpPacket {
    pub const ECHO_REQUEST_IPV4: u8 = 8;
    pub const ECHO_REQUEST_IPV6: u8 = 128;
    pub const ECHO_REPLY_IPV4: u8 = 0;
    pub const ECHO_REPLY_IPV6: u8 = 129;

    pub fn new(kind: u8, id: u16, seq_num: u16) -> Self {
        //let checksum = !((!(((kind as u16) << 8) | 0)).overflowing_add(!(0)).0.overflowing_add(!(id)).0.overflowing_add(!(seq_num)).0);
        let mut checksum = 0u16;
        for &value in &[(kind as u16) << 8, 0, id, seq_num] {
            checksum = checksum.overflowing_add(value).0;
            if value >= checksum {
                checksum += 1;
            }
        }
        //let checksum = ((kind as u16) << 8) 
        Self {
            kind: kind,
            code: 0,
            checksum: checksum,
            id: id,
            seq_num: seq_num,
        }
    }

    pub fn as_bytes(&self) -> [u8; 8] {
        [
            self.kind,
            self.code,
            /*(self.checksum.to_be() >> 8) as u8,
            self.checksum.to_be() as u8,
            (self.id.to_be() >> 8) as u8,
            self.id.to_be() as u8,
            (self.seq_num.to_be() >> 8) as u8,
            self.seq_num.to_be() as u8,*/
            (self.checksum >> 8) as u8,
            self.checksum as u8,
            (self.id >> 8) as u8,
            self.id as u8,
            (self.seq_num >> 8) as u8,
            self.seq_num as u8,
        ]
    }
}

struct IcmpSocket {
    fd: RawFd
}

impl IcmpSocket {
    pub fn new() -> Result<Self> {
        let socktype = if unistd::geteuid().is_root() {
            libc::SOCK_RAW
        } else {
            libc::SOCK_DGRAM
        };
        let sock = unsafe { libc::socket(AF_INET, socktype, libc::IPPROTO_ICMP) };
        if sock < 0 {
            Err(io::Error::last_os_error().into())
        } else {
            Ok(Self {
                fd: sock as RawFd
            })
        }
    }

    /*pub fn resolve_hostname(&mut self, hostname: &str) -> io::Result<()> {
        // XXX: i do not believe the port matters, but i have not checked the code for
        //      to_socket_addrs() yet
        let addrs = (hostname, 8080).to_socket_addrs()?;

        // TODO: loop over addrs iter and bind socket

        Ok(())
    }*/

    // XXX: OsString?
    pub fn resolve_hostname(hostname: &str) -> io::Result<SocketAddr> {
        let mut addrs = (hostname, /*8080*/80).to_socket_addrs()?;

        // XXX: which address?
        Ok(addrs.next().unwrap())

//        Ok("".to_string())
    }
}

impl Drop for IcmpSocket {
    fn drop(&mut self) {
        // we can't handle any errors so just ignore them
        let _ = socket::shutdown(self.fd, socket::Shutdown::Both);
    }
}

// TODO: this needs to catch SIGINT and dump out the stats when it does
pub fn execute<I, O, E, T, U>(setup: &mut UtilSetup<I, O, E>, args: ArgsIter<T, U>) -> super::Result<()>
where
    I: UtilRead,
    O: UtilWrite,
    E: UtilWrite,
    T: Iterator<Item = U>,
    U: Into<OsString> + Clone,
{
    let mut app = util_app!("ping")
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
    
    let matches = get_matches!(setup, app, args);
    
    let mut count = if matches.is_present("count") {
        // this is fine because of the validator
        Some(u64::from_str(matches.value_of("count").unwrap()).unwrap())
    } else {
        None
    };

    let wait_time = matches.value_of("waittime")
                           .and_then(|v| u64::from_str(v).ok())
                           .unwrap_or(1000);
    
    let between_packet_wait = matches.value_of("packet_wait")
                                     .and_then(|v| f64::from_str(v).ok())
                                     .unwrap_or(1.0);

    let hostname_os = matches.value_of_os("HOST").unwrap();
    let hostname = match hostname_os.to_str() {
        Some(s) => s,
        None => {
            eprintln!("invalid hostname: {}", hostname_os.to_string_lossy());
            process::exit(1);
        }
    };
    let resolved_ip = IcmpSocket::resolve_hostname(hostname)?;

    let millis = Duration::from_millis(((between_packet_wait - (between_packet_wait as u64 as f64)) * 1000.0) as u64);
    let duration = Duration::from_secs(between_packet_wait as u64) + millis;

    // TODO: check for errors
    let mut sock = IcmpSocket::new()?;
    let addr = socket::SockAddr::new_inet(socket::InetAddr::from_std(&resolved_ip));
    //socket::connect(sock.fd, &socket::SockAddr::new_inet(socket::InetAddr::from_std(&resolved_ip)))?;

    let mut stats = Stats { sent: 0, received: 0, roundtrips: vec![] };

    // FIXME: should be unique per call or something
    let mut ident = 0;
    let mut seq_num = 0;
    // TODO: data bytes
    println!("PING {} ({}): {} data bytes", hostname, resolved_ip.ip(), 0);
    while count.unwrap_or(1) > 0 {
        // TODO: send packet
        //sock.send()
        let request = IcmpPacket::new(IcmpPacket::ECHO_REQUEST_IPV4, ident, seq_num);
        // FIXME: should print error (should not return)
        //socket::send(sock.fd, &request.as_bytes(), socket::MsgFlags::empty())?;//, socket::MsgFlags::MSG_DONTWAIT)?;
        let res = socket::sendto(sock.fd, &request.as_bytes(), &addr, socket::MsgFlags::empty())?;//, socket::MsgFlags::MSG_DONTWAIT)?;
        if res < 8 {
            println!("partial write");
        }
        stats.sent += 1;

        // TODO: wait some time (1 second? or whatever wait time is specified)
        //thread::sleep(Duration::from_secs(1));

        // FIXME: prob not right
        nix::poll::poll(&mut [nix::poll::PollFd::new(sock.fd, nix::poll::EventFlags::POLLIN)], 1)?;

        // TODO: try to receive the packet

        println!("testing");
        let mut data = [0; 56];
        // FIXME: should use recv() and not use fixed size data
        loop {
            match socket::recvfrom(sock.fd, &mut data) {//, socket::MsgFlags::MSG_DONTWAIT) {      //socket::recv(sock.fd, &mut data, socket::MsgFlags::empty()) {//, socket::MsgFlags::MSG_DONTWAIT) {
                Ok(n) => {
                    // FIXME: not right so
                    //if n.0 == 8 {
                        println!("test");
                        stats.received += 1;
                        break;
                        // TODO: add to roundtrips
                    //}
                }
                Err(nix::Error::Sys(Errno::EAGAIN)) => break,
                Err(f) => {
                    Err(f)?
                }
            }
        }

        if let Some(val) = count {
            count = Some(val - 1);
        }

        seq_num += 1;

        // TODO: do not sleep if flooding
        thread::sleep(duration.clone());
    }

    print_stats(&hostname, &stats);

    Ok(())
}

fn is_number(val: &OsStr) -> StdResult<(), OsString> {
    if val.to_str().and_then(|s| u64::from_str(s).ok()).is_some() {
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

fn print_stats(hostname: &str, stats: &Stats) {
    println!("--- {} ping statistics ---", hostname);
    println!("{} packets transmitted, {} packets received, {}% packet loss", stats.sent, stats.received, stats.packet_loss());

    if stats.received > 0 {
        let mut min = f64::MAX;
        let mut max = f64::MIN;
        let mut avg = 0.0;
        let mut stddev = 0.0;
        for &time in &stats.roundtrips {
            min = min.min(time);
            max = max.max(time);
            avg += time / stats.roundtrips.len() as f64;
            // TODO: stddev
        }
        println!("round-trip min/avg/max/stddev = {}/{}/{}/{} ms", min, max, avg, stddev);
    }
}
