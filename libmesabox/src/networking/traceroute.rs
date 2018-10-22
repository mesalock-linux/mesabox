//
// Copyright (c) 2018, The MesaLock Linux Project Contributors
// All rights reserved.
//
// This work is licensed under the terms of the BSD 3-Clause License.
// For a copy, see the LICENSE file.
//

use {ArgsIter, Result, UtilSetup, UtilWrite};

use std::borrow::Cow;
use std::ffi::{OsStr, OsString};
use std::io::{self, ErrorKind, Write};
use std::net::{IpAddr, Ipv4Addr, SocketAddr};
use std::result::Result as StdResult;
use std::time::{Duration, Instant};

use byteorder::{NetworkEndian, ReadBytesExt};
use clap::{App, Arg, ArgMatches};
use pnet_packet::icmp::echo_reply::EchoReplyPacket;
use pnet_packet::icmp::echo_request::MutableEchoRequestPacket;
use pnet_packet::icmp::time_exceeded::TimeExceededPacket;
use pnet_packet::icmp::{self, IcmpPacket, IcmpType, IcmpTypes, MutableIcmpPacket};
use pnet_packet::ipv4::{self, Ipv4Packet};
use socket2::{Domain, Protocol, SockAddr, Socket, Type};
use trust_dns_resolver::Resolver;

pub const DESCRIPTION: &str = "Traces the route taken to reach a host";

const DEFAULT_PORT: u16 = 33434;
const DEFAULT_FHOP: u8 = 1;
const DEFAULT_MHOP: u8 = 64;
const DEFAULT_WAIT: u64 = 3;
const DEFAULT_TRIES: u64 = 3;

struct Options<'a> {
    port: u16,
    first_hop: u8,
    max_hop: u8,
    resolve_hostnames: bool,
    wait_time: Duration,
    tries: u64,
    method: TraceMethod,
    host: Cow<'a, str>,
}

// TODO: add support for TCP
#[derive(Clone, Copy)]
enum TraceMethod {
    Udp,
    Icmp,
}

struct TraceSocket {
    method: TraceMethod,
    domain: Domain,
    port: u16,
    ttl: u8,
    socket: Socket,
    send_time: Instant,
}

impl TraceSocket {
    pub fn new(method: TraceMethod, port: u16, domain: Domain) -> io::Result<Self> {
        let (kind, protocol) = match method {
            TraceMethod::Udp => {
                let kind = Type::dgram();
                let protocol = Protocol::udp();
                (kind, protocol)
            }
            TraceMethod::Icmp => {
                let kind = Type::raw();
                let protocol = if i32::from(domain) == i32::from(Domain::ipv4()) {
                    Protocol::icmpv4()
                } else {
                    Protocol::icmpv6()
                };
                (kind, protocol)
            }
        };
        let socket = Socket::new(domain, kind, Some(protocol))?;

        Ok(Self {
            method,
            domain,
            port,
            ttl: 128,
            socket,
            send_time: Instant::now(),
        })
    }

    pub fn recv_socket(&mut self) -> io::Result<(RecvSocket, u16)> {
        let sock = RecvSocket::new(self.method, self.port, self.domain)?;

        let cur_port = self.port;
        self.port += 1;

        Ok((sock, cur_port))
    }

    pub fn send_trace(&mut self, addr: &SockAddr) -> io::Result<usize> {
        self.send_time = Instant::now();

        match self.method {
            TraceMethod::Udp => self.socket.send_to(&[], addr),
            TraceMethod::Icmp => {
                // TODO: find the actual number needed
                let mut icmp_buffer = [0; 512];

                // FIXME: don't unwrap
                let buffer = self.construct_icmp_packet(&mut icmp_buffer).unwrap();

                self.socket.send_to(buffer, addr)
            }
        }
    }

    pub fn set_ttl(&mut self, ttl: u8) -> io::Result<()> {
        // NOTE: i am unsure why but this accepts 32-bit values even though the
        //       TTL is limited to 8 bits
        self.ttl = ttl;
        self.socket.set_ttl(ttl as _)
    }

    fn construct_icmp_packet<'a>(&self, buffer: &'a mut [u8]) -> Option<&'a [u8]> {
        {
            let mut icmp_packet = MutableEchoRequestPacket::new(buffer)?;
            icmp_packet.set_sequence_number(self.port - 1);
            icmp_packet.set_icmp_type(IcmpTypes::EchoRequest);
        }

        {
            let mut icmp_packet = MutableIcmpPacket::new(
                &mut buffer[..MutableEchoRequestPacket::minimum_packet_size()],
            )?;
            let checksum = icmp::checksum(&icmp_packet.to_immutable());
            icmp_packet.set_checksum(checksum);
        }

        Some(&buffer[..MutableEchoRequestPacket::minimum_packet_size()])
    }
}

struct RecvSocket {
    method: TraceMethod,
    socket: Socket,

    // timeout-related
    last_update: Instant,
    timeout: Option<Duration>,

    recv_time: Instant,
}

impl RecvSocket {
    pub fn new(method: TraceMethod, port: u16, domain: Domain) -> io::Result<Self> {
        let (kind, protocol) = match method {
            TraceMethod::Udp | TraceMethod::Icmp => {
                // NOTE: because this requires raw sockets, this utility must be executed
                //       as root (or with CAP_NET_RAW on Linux)
                let kind = Type::raw();
                let protocol = if i32::from(domain) == i32::from(Domain::ipv4()) {
                    Protocol::icmpv4()
                } else {
                    Protocol::icmpv6()
                };
                (kind, protocol)
            }
        };

        let socket = Socket::new(domain, kind, Some(protocol))?;
        socket.bind(&SocketAddr::from((Ipv4Addr::new(0, 0, 0, 0), port)).into())?;

        let cur_time = Instant::now();

        Ok(RecvSocket {
            method,
            socket,
            last_update: cur_time,
            timeout: None,
            recv_time: cur_time,
        })
    }

    pub fn recv_trace(&mut self, buf: &mut [u8]) -> io::Result<TraceData> {
        let (size, addr) = self.socket.recv_from(buf)?;
        self.recv_time = Instant::now();

        let ip_addr = if let Some(addr_v4) = addr.as_inet() {
            addr_v4.ip().clone().into()
        } else if let Some(addr_v6) = addr.as_inet6() {
            addr_v6.ip().clone().into()
        } else {
            unimplemented!()
        };

        // FIXME: obviously don't unwrap
        let (kind, port) = self.find_target_port(&buf[..size]).unwrap();
        // XXX: does this ever return EchoReply?
        let found_dest = kind == IcmpTypes::DestinationUnreachable || kind == IcmpTypes::EchoReply;

        Ok(TraceData {
            ip_addr,
            port,
            found_dest,
            recv_time: self.recv_time,
        })
    }

    // TODO: add IPv6 support
    fn find_target_port(&self, buffer: &[u8]) -> Option<(IcmpType, u16)> {
        let start = match self.method {
            TraceMethod::Udp => 2,
            TraceMethod::Icmp => 6,
        };

        let icmp_buffer = self.extract_ipv4_payload(buffer)?;

        let packet = IcmpPacket::new(icmp_buffer)?;

        let kind = packet.get_icmp_type();

        if icmp::checksum(&packet) == packet.get_checksum() {
            if kind == IcmpTypes::TimeExceeded || kind == IcmpTypes::DestinationUnreachable {
                // NOTE: for some reason, there is no way to get the payload from a
                //       TimeExceeded packet, so we just manually find the data we need
                let old_ip_header = self.extract_ipv4_payload(
                    &icmp_buffer[TimeExceededPacket::minimum_packet_size()..],
                )?;

                if old_ip_header.len() >= 8 {
                    // NOTE: unwrapping is fine as we verify the length of the packet
                    //       is alright above
                    // XXX: for some reason this differs from the udp method?
                    let recv_port = (&old_ip_header[start..])
                        .read_u16::<NetworkEndian>()
                        .unwrap();

                    return Some((kind, recv_port));
                }
            } else if kind == IcmpTypes::EchoReply {
                return Some((
                    kind,
                    EchoReplyPacket::new(icmp_buffer)?.get_sequence_number(),
                ));
            }
        }

        None
    }

    fn extract_ipv4_payload<'a>(&self, buffer: &'a [u8]) -> Option<&'a [u8]> {
        let ip_packet = Ipv4Packet::new(buffer)?;

        if ip_packet.get_checksum() == ipv4::checksum(&ip_packet) {
            let header_len = ip_packet.get_header_length() as usize * 4;
            if header_len < buffer.len() {
                return Some(&buffer[header_len..]);
            }
        }

        None
    }

    pub fn update_timeout(&mut self) -> io::Result<bool> {
        if let Some(ref mut dur) = self.timeout {
            let diff = self.recv_time - self.last_update;
            if diff >= *dur {
                return Ok(false);
            } else {
                *dur -= diff;
            }
        }

        self.last_update = self.recv_time;

        self.socket.set_read_timeout(self.timeout).map(|_| true)
    }

    pub fn reset_timeout(&mut self, dur: Option<Duration>) -> io::Result<()> {
        self.timeout = dur;
        self.last_update = Instant::now();

        self.socket.set_read_timeout(dur)
    }
}

struct TraceData {
    ip_addr: IpAddr,
    port: u16,
    found_dest: bool,
    recv_time: Instant,
}

pub fn execute<S, T>(setup: &mut S, args: T) -> Result<()>
where
    S: UtilSetup,
    T: ArgsIter,
{
    let app = setup_clap();

    let matches = app.get_matches_from_safe(args)?;

    let options = determine_options(&matches);

    let resolver = Resolver::from_system_conf()?;
    let response = resolver.lookup_ip(&options.host)?;

    let addr = match response.iter().next() {
        Some(addr) => addr,
        None => {
            // TODO: print out "unknown host" or something and return an error
            unimplemented!()
        }
    };

    let domain = if addr.is_ipv4() {
        Domain::ipv4()
    } else {
        Domain::ipv6()
    };

    let mut socket = TraceSocket::new(options.method, options.port, domain)?;

    let mut stdout = setup.output().lock()?;

    let mut buffer = [0; 512];

    writeln!(
        stdout,
        "traceroute to {} ({}), {} hops max",
        options.host, addr, options.max_hop
    )?;

    // XXX: what should all of these errors do?  should they exit (like they
    //      currently do) or just continue with the next hop?
    for hops in options.first_hop..=options.max_hop {
        socket.set_ttl(hops)?;

        let (mut recv_sock, cur_port) = socket.recv_socket()?;

        let mut printed_addr = false;
        let mut found_dest = false;

        write!(stdout, "{: >3} ", hops - options.first_hop + 1)?;
        'outer: for _ in 0..options.tries {
            recv_sock.reset_timeout(Some(options.wait_time))?;

            let sockaddr = SocketAddr::from((addr, cur_port));
            let send_now = Instant::now();

            // send a packet to the host
            socket.send_trace(&sockaddr.into())?;

            // this loop skips any old messages that timed-out
            let data = loop {
                let timed_out = match recv_sock.recv_trace(&mut buffer) {
                    Ok(ref res) if res.port != cur_port => {
                        // modify the timeout so we don't accidentally wait
                        // longer than WAIT secs
                        !recv_sock.update_timeout()?
                    }
                    Ok(res) => break res,
                    // XXX: this might need to check for both WouldBlock and
                    //      TimedOut (not sure what it returns on Windows)
                    Err(ref e) if e.kind() == ErrorKind::WouldBlock => true,
                    // XXX: return?
                    Err(e) => return Err(e.into()),
                };

                if timed_out {
                    write!(stdout, "  *")?;
                    stdout.flush()?;
                    continue 'outer;
                }
            };

            // FIXME: how should this print when the first connection attempt times out by the second succeeds?
            //        the windows version seems to just print each separate connection even if it's on the same line
            //        like if the first packet goes to one host but the second goes to another, it prints the first host and the time
            //        and then the second host and the time
            if !printed_addr {
                write!(stdout, "  {}", data.ip_addr)?;
                if options.resolve_hostnames {
                    let mut success = false;
                    if let Ok(host) = resolver.reverse_lookup(data.ip_addr) {
                        if let Some(hostname) = host.iter().next() {
                            success = true;
                            write!(stdout, " ({})", hostname)?;
                        }
                    }
                    if !success {
                        write!(stdout, " ({})", data.ip_addr)?;
                    }
                }
                printed_addr = true;
            }
            let duration = data.recv_time - send_now;
            let (time, suffix) = if duration.as_secs() > 0 {
                let time = duration.as_secs() as f32 + duration.subsec_millis() as f32 / 1_000.0;
                (time, "s")
            } else {
                let time = duration.subsec_nanos() as f32 / 1_000_000.0;
                (time, "ms")
            };
            write!(stdout, "  {:.3}{}", time, suffix)?;
            stdout.flush()?;

            found_dest = data.found_dest;
        }
        writeln!(stdout)?;

        if found_dest {
            break;
        }
    }

    Ok(())
}

fn setup_clap() -> App<'static, 'static> {
    util_app!("traceroute")
        .arg(
            Arg::with_name("first-hop")
                .short("f")
                .long("first-hop")
                .takes_value(true)
                .validator_os(is_valid_hop)
                .help("Set the first hop (default is 1)"),
        ).arg(
            Arg::with_name("max-hop")
                .short("m")
                .long("max-hop")
                .takes_value(true)
                .validator_os(is_valid_hop)
                .help("Set the max hop (default is 64)"),
        ).arg(
            Arg::with_name("resolve")
                .long("resolve-hostnames")
                .help("Try to resolve hostnames of IP addresses"),
        ).arg(
            Arg::with_name("port")
                .short("p")
                .long("port")
                .takes_value(true)
                .value_name("PORT")
                .validator_os(is_valid_u16)
                .help("Set the destination port of the target to PORT"),
        ).arg(
            Arg::with_name("wait")
                .short("w")
                .long("wait")
                .takes_value(true)
                .value_name("TIME")
                .validator_os(is_valid_u64)
                .help("Set the timeout for receiving packets (default is 3)"),
        ).arg(
            Arg::with_name("tries")
                .short("q")
                .long("tries")
                .takes_value(true)
                .value_name("NUM")
                .validator_os(is_valid_u64)
                .help("Set the number of probes per hop"),
        ).arg(
            Arg::with_name("method")
                .short("M")
                .long("type")
                .takes_value(true)
                .value_name("METHOD")
                .possible_values(&["udp", "icmp"])
                .default_value("udp"),
        ).arg(Arg::with_name("HOST").index(1).required(true))
}

fn determine_options<'a>(matches: &'a ArgMatches) -> Options<'a> {
    let port = matches
        .value_of("port")
        .map(|s| s.parse::<u16>().unwrap())
        .unwrap_or(DEFAULT_PORT);

    let first_hop = matches
        .value_of("first-hop")
        .map(|s| s.parse::<u8>().unwrap())
        .unwrap_or(DEFAULT_FHOP);

    let max_hop = matches
        .value_of("max-hop")
        .map(|s| s.parse::<u8>().unwrap())
        .unwrap_or(DEFAULT_MHOP);

    let wait = matches
        .value_of("wait")
        .map(|s| s.parse::<u64>().unwrap())
        .unwrap_or(DEFAULT_WAIT);

    let tries = matches
        .value_of("tries")
        .map(|s| s.parse::<u64>().unwrap())
        .unwrap_or(DEFAULT_TRIES);

    let method = matches
        .value_of("method")
        .map(|s| match s {
            "udp" => TraceMethod::Udp,
            "icmp" => TraceMethod::Icmp,
            _ => unreachable!(),
        }).unwrap();

    let resolve_hostnames = matches.is_present("resolve");

    // XXX: is lossy okay for this?
    let host = matches.value_of_lossy("HOST").unwrap();

    Options {
        port,
        first_hop,
        max_hop,
        wait_time: Duration::from_secs(wait),
        tries,
        resolve_hostnames,
        method,
        host,
    }
}

fn is_valid_num<T, F: Fn(&str) -> Option<T>>(val: &OsStr, func: F) -> StdResult<(), OsString> {
    let res = val.to_str().and_then(func);
    if res.is_some() {
        Ok(())
    } else {
        Err(OsString::from(format!(
            "'{}' is either not a number or out of range",
            val.to_string_lossy()
        )))
    }
}

fn is_valid_hop(val: &OsStr) -> StdResult<(), OsString> {
    is_valid_num(val, |s| {
        s.parse::<u8>()
            .ok()
            .and_then(|num| if num > 0 { Some(num) } else { None })
    })
}

fn is_valid_u16(val: &OsStr) -> StdResult<(), OsString> {
    is_valid_num(val, |s| s.parse::<u16>().ok())
}

fn is_valid_u64(val: &OsStr) -> StdResult<(), OsString> {
    is_valid_num(val, |s| s.parse::<u64>().ok())
}
