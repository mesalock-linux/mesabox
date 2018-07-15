extern crate mio;
extern crate clap;
extern crate libc;
extern crate socket2;

use std;
use failure;
use tempfile::NamedTempFile;
use clap::{Arg, ArgMatches};
use mio::{Events, Event, Poll, Ready, PollOpt, Token};
use libc::{AF_UNSPEC, AF_INET, AF_INET6, AF_UNIX};
use std::io;
use std::net::{SocketAddr};
use mio::unix::EventedFd;
use std::io::{Read,Write, ErrorKind};
use mio::unix::UnixReady;
use std::thread::sleep;
use std::fmt::Debug;
use std::os::unix::io::AsRawFd;
use std::os::unix::io::RawFd;
use std::time::Duration;
use std::net::{ToSocketAddrs};
use socket2::{Socket, Domain};
use super::{UtilSetup, ArgsIter};
use super::{MesaError};


pub(crate) const NAME: &str = "nc";
pub(crate) const DESCRIPTION: &str = "netcat";

const BUFSIZE: usize = 16384;
const PRINT_DEBUG_INFO: bool = false;

#[derive(Debug)]
struct NcOptions {
    dflag: bool,
    iflag: bool,
    interval: Option<Duration>,
    lflag: bool,
    host: String,
    portlist: Vec<u16>,
    unixflag: bool,
    uflag: bool,
    family: i32,
    kflag: bool,
    s_addr: Option<String>,
    pflag: bool,
    vflag: bool,
    zflag: bool,
    timeout: Option<Duration>,
    unix_dg_tmp_socket: String,
}

fn mesaerr_result<T>(err_msg: &str) -> Result<T, MesaError> {
    Err(MesaError::from(
        failure::err_msg(format!("{}", err_msg)).compat()
        ))
}

fn build_ports(ports: &str) -> Result<Vec<u16>, MesaError>{
    // TODO: suport XX-XX
    let port_list = match ports.parse::<u16>() {
        Ok(port) => port,
        Err(_) => {
            return mesaerr_result(&format!("invalid port[s] {}", ports));
        }
    };

    Ok(vec!(port_list))
}

fn warn(msg: &str) {
    eprint!("{}", msg);
}

fn debug_info(msg: &str) {
    if PRINT_DEBUG_INFO {
        eprint!("{}", msg);
    }
}

impl NcOptions {
    pub fn parse(matches: ArgMatches, msg: &str) -> Result<NcOptions, MesaError> {
        let mut portlist = vec!();
        let lflag = matches.is_present("l");
        let mut host = String::from("127.0.0.1");
        let uport:String;
        let mut interval = None;
        let mut timeout = None;
        let s_addr = match matches.value_of("s") {
            Some(addr) => Some(String::from(addr)),
            None => None
        };
        let uflag = matches.is_present("u");
        let pflag = matches.is_present("p");

        let zflag = matches.is_present("z");
        let kflag = matches.is_present("k");

        /* Cruft to make sure options are clean, and used properly. */
        let positionals: Vec<&str> = if matches.is_present("positionals") {
            matches.values_of("positionals").unwrap().collect()
        } else {
            vec!()
        };

        let family = if matches.is_present("U") {
            AF_UNIX
        } else if matches.is_present("6") {
            AF_INET6
        } else if matches.is_present("4") {
            AF_INET
        } else {
            AF_UNSPEC
        };

        if positionals.len() == 1 {
            if family == AF_UNIX {
                host = String::from(positionals[0]);
                uport = String::new();
            } else {
                if !lflag {
                    return mesaerr_result(msg);
                }
                uport = String::from(positionals[0]);
            }
        } else if positionals.len() >= 2 {
            host = String::from(positionals[0]);
            uport = String::from(positionals[1]);
        } else {
            return mesaerr_result(msg);
        }

        if lflag && s_addr.is_some() {
            return mesaerr_result("cannot use -s and -l");
        }
        if lflag && pflag {
            return mesaerr_result("cannot use -p and -l");
        }
        if lflag && zflag {
            return mesaerr_result("cannot use -z and -l");
        }
        if !lflag && kflag {
            return mesaerr_result("must use -l with -k");
        }

        if !uport.is_empty() {
            portlist = build_ports(&uport)?;
        }

        if matches.is_present("i") {
            let sec = matches.value_of("i").unwrap().parse::<u64>().unwrap();
            interval = Some(Duration::new(sec, 0));
        }

        if matches.is_present("w") {
            let sec = matches.value_of("w").unwrap().parse::<u64>().unwrap();
            timeout = Some(Duration::new(sec, 0));
        }

        let mut unix_dg_tmp_socket = String::new();

        /* Get name of temporary socket for unix datagram client */
        if family == AF_UNIX && uflag && !lflag {
            unix_dg_tmp_socket = if s_addr.is_some() {
                s_addr.clone().unwrap()
            } else {
                let nf = NamedTempFile::new()?;
                let path = String::from(nf.path().to_str().unwrap());
                path
            };
        }

        let ret = NcOptions {
            dflag: matches.is_present("d"),
            iflag: matches.is_present("i"),
            interval: interval,
            lflag: matches.is_present("l"),
            unixflag: matches.is_present("U"),
            uflag: uflag,
            host: host,
            portlist: portlist,
            family: family,
            kflag: matches.is_present("k"),
            // sflag: sflag,
            s_addr: s_addr,
            pflag: pflag,
            vflag: matches.is_present("v"),
            timeout: timeout,
            unix_dg_tmp_socket: unix_dg_tmp_socket,
            zflag: zflag,
        };

        return Ok(ret);
    }
}

fn remove_item<T: Eq+Debug>(v: &mut Vec<T>, item: T) {
    debug_info(&format!("remove_item {:?}", item));
    match v.iter().position(|t| *t == item) {
        Some(i) => v.remove(i),
        None => return
    };
}

struct NcCore<'a> {
    sock: &'a mut Socket,
    opts: &'a NcOptions,
    poll: Poll,
    net_interest: Ready,
    event_stdin: EventedFd<'a>,
    event_net: EventedFd<'a>,
    event_stdout: EventedFd<'a>,
    stdinbuf: [u8; BUFSIZE],
    netinbuf: [u8; BUFSIZE],
    stdinbuf_bac: [u8; BUFSIZE],
    netinbuf_bac: [u8; BUFSIZE],
    netinbufpos: usize,
    stdinbufpos: usize,
    open_ends: Vec<i32>,
}

impl <'a> NcCore<'a> {
    const STDIN_POLL: i32 = 1;
    const STDOUT_POLL: i32 = 2;
    const NETIN_POLL: i32 = 4;
    const NETOUT_POLL: i32 = 8;

    const TK_STDIN: Token = Token(0);
    const TK_STDOUT: Token = Token(1);
    const TK_NET: Token = Token(2);

    fn new(sock: &'a mut Socket, opts: &'a NcOptions, net_fd: &'a RawFd) -> Result<NcCore<'a>, MesaError> {
        let mut ret = NcCore {
            sock: sock,
            opts: opts,

            poll: Poll::new()?,
            net_interest: Ready::readable(),
            event_stdin: EventedFd(&0),
            event_net: EventedFd(net_fd),
            event_stdout: EventedFd(&1),
            stdinbuf: [0; BUFSIZE],
            netinbuf: [0; BUFSIZE],
            stdinbuf_bac: [0; BUFSIZE],
            netinbuf_bac: [0; BUFSIZE],
            netinbufpos: 0,
            stdinbufpos: 0,
            open_ends: vec![NcCore::STDIN_POLL, NcCore::STDOUT_POLL, NcCore::NETIN_POLL, NcCore::NETOUT_POLL],
        };

        ret.poll.register(&ret.event_stdin, NcCore::TK_STDIN, Ready::readable(),
          PollOpt::empty())?;
        ret.poll.register(&ret.event_net, NcCore::TK_NET, ret.net_interest,
            PollOpt::empty())?;
        ret.poll.register(&ret.event_stdout, NcCore::TK_STDOUT, Ready::empty(),
          PollOpt::empty())?;

        if ret.opts.dflag {
            ret.remove_stdin()?;
        }

        Ok(ret)
    }

    /*
     * readwrite()
     * Loop that polls on the network file descriptor and stdin.
     */
    fn readwrite(&mut self) -> Result<(), MesaError> {
        let mut events = Events::with_capacity(1024);

        let mut last_ready_end = -1;

        loop {
            /* both inputs are gone, buffers are empty, we are done */
            if self.stdin_gone() && self.netin_gone() &&
                self.stdinbuf_empty() && self.netinbuf_empty() {
                self.sock.shutdown(std::net::Shutdown::Both)?;
                return Ok(());
            }

            /* both outputs are gone, we can't continue */
            if self.stdout_gone() && self.netout_gone() {
                self.sock.shutdown(std::net::Shutdown::Both)?;
                return Ok(());
            }

            /* listen and net in gone, queues empty, done */
            if self.opts.lflag && self.netin_gone() &&
                self.stdinbuf_empty() && self.netinbuf_empty() {
                self.sock.shutdown(std::net::Shutdown::Both)?;
                return Ok(());
            }

            /* help says -i is for "wait between lines sent". We read and
             * write arbitrary amounts of data, and we don't want to start
             * scanning for newlines, so this is as good as it gets */
             if self.opts.iflag {
                sleep(self.opts.interval.unwrap());
            }

            if let Err(_) = self.poll.poll(&mut events, None) {
                return mesaerr_result("polling error");
            }

            /* timeout happened */
            if events.is_empty() {
                return Ok(());
            }

            for event in &events {
                // if any error happend, stop watching for the corresponding fd
                if event.readiness().is_error() {
                    self.handle_error_event(&event)?;
                }

                NcCore::debug_print_ready_end(&event, &mut last_ready_end);

                if event.readiness().contains(UnixReady::hup()) {
                    self.handle_hup_event(&event)?;
                }

                // if no net out, finish watching stdin
                if self.netout_gone() {
                    self.remove_stdin()?;
                }

                // if no stdout, stop watching net in
                if self.stdout_gone() {
                    if !self.netin_gone() {
                        self.sock.shutdown(std::net::Shutdown::Read)?;
                    }
                    self.remove_stdin()?;
                }

                // if stdin readable and buf not full, try to read stdin
                //     error or eof, remove and deregister stdin
                //     if buf not emtpy, reregister writable for netout
                //     if buf full, reregister with empty
                if event.token() == NcCore::TK_STDIN && event.readiness().is_readable() && !self.stdinbuf_full() {
                    self.read_stdin()?;

                    if !self.stdinbuf_empty() {
                        self.enable_netout()?;
                    } else if self.stdinbuf_full() {
                        self.disable_stdin()?;
                    }
                }                

                // if net writable and buf not empty, try to write to net
                //      error, stop watching for netout
                //      memmove if needed
                //      if stdinbuf empty, reregister to remove netout writable
                //      if buf not full, reregister to add stdin readable
                if event.token() == NcCore::TK_NET && event.readiness().is_writable() && !self.stdinbuf_empty() {
                    self.write_netout()?;
                    if self.stdinbuf_empty() {
                        self.disable_netout()?;
                    } else if !self.stdinbuf_full() {
                        self.enable_stdin()?;
                    }
                }

                // if net readable and buf not full, try to read net
                //     error or eof, remove and deregister netin
                //     if buf not emtpy, reregister writable for stdout
                //     if buf full, reregister to remove netin
                if event.token() == NcCore::TK_NET && event.readiness().is_readable() && !self.netinbuf_full() {
                    self.read_netin()?;
                    if !self.netinbuf_empty() {
                        self.enable_stdout()?;
                    } else if self.netinbuf_full() {
                        self.disable_netin()?;
                    }
                }

                // if stdout writable and buf not empty, try to write to stdout
                //      error, stop watching for stdout
                //      memmove if needed
                //      if netinbuf empty, reregister to remove stdout writable
                //      if buf not full, reregister to add netin readable
                if event.token() == NcCore::TK_STDOUT && event.readiness().is_writable() && !self.netinbuf_empty() {
                    self.write_stdout()?;
                    if self.netinbuf_empty() {
                        self.disable_stdout()?;
                    } else if !self.netinbuf_full() {
                        self.enable_netin()?;
                    }
                }

                // if stdin gone and stdinbuf empty, remove netout
                if self.stdin_gone() && self.stdinbuf_empty() {
                    if !self.netout_gone() {
                        // TODO: check && opts.Nflag {
                        self.sock.shutdown(std::net::Shutdown::Write)?;
                    }
                    self.remove_netout()?;
                }

                // if netin gone and netinbuf empty, remove stdout
                if self.netin_gone() && self.netinbuf_empty() {
                    self.remove_stdout()?;
                }
            }

        }
    }

    fn run(sock: &mut Socket, opts: &NcOptions) -> Result<(), MesaError> {
        let net_fd = sock.as_raw_fd();
        let mut nc = NcCore::new(sock, &opts, &net_fd)?;
        nc.readwrite()
    }

    fn stdin_gone(&self) -> bool {
        !self.open_ends.contains(&NcCore::STDIN_POLL)
    }

    fn stdout_gone(&self) -> bool {
        !self.open_ends.contains(&NcCore::STDOUT_POLL)
    }

    fn netin_gone(&self) -> bool {
        !self.open_ends.contains(&NcCore::NETIN_POLL)
    }

    fn netout_gone(&self) -> bool {
        !self.open_ends.contains(&NcCore::NETOUT_POLL)
    }

    fn stdinbuf_empty(&self)  -> bool {
        self.stdinbufpos == 0
    }

    fn stdinbuf_full(&self) -> bool {
        self.stdinbufpos >= BUFSIZE
    }

    fn netinbuf_empty(&self)  -> bool {
        self.netinbufpos == 0
    }

    fn netinbuf_full(&self) -> bool {
        self.netinbufpos >= BUFSIZE
    }

    fn remove_stdin(&mut self) -> std::io::Result<()> {
        remove_item(&mut self.open_ends, NcCore::STDIN_POLL);
        self.poll.deregister(&self.event_stdin)
    }

    fn remove_stdout(&mut self) -> std::io::Result<()> {
        debug_info("remove_stdout");
        remove_item(&mut self.open_ends, NcCore::STDOUT_POLL);
        self.poll.deregister(&self.event_stdout)
    }

    fn remove_netin(&mut self) -> std::io::Result<()> {
        remove_item(&mut self.open_ends, NcCore::NETIN_POLL);
        self.net_interest.remove(Ready::readable());
        self.reregister_net()
    }

    fn remove_netout(&mut self) -> std::io::Result<()> {
        remove_item(&mut self.open_ends, NcCore::NETOUT_POLL);
        self.net_interest.remove(Ready::writable());
        return self.reregister_net();
    }

    fn reregister_net(&mut self) -> std::io::Result<()> {
        self.poll.reregister(&self.event_net, NcCore::TK_NET, self.net_interest,
            PollOpt::empty())
    }

    fn enable_netin(&mut self) -> std::io::Result<()>{
        self.net_interest |= Ready::readable();
        self.reregister_net()
    }

    fn disable_netin(&mut self) -> std::io::Result<()>{
        self.net_interest.remove(Ready::readable());
        self.reregister_net()     
    }

    fn enable_netout(&mut self) -> std::io::Result<()> {
        self.net_interest |= Ready::writable();
        self.reregister_net()        
    }

    fn disable_netout(&mut self) -> std::io::Result<()> {
        self.net_interest.remove(Ready::writable());
        self.reregister_net()       
    }

    fn enable_stdin(&mut self) -> std::io::Result<()> {
        self.poll.reregister(&self.event_stdin, NcCore::TK_STDIN, Ready::readable(),
                        PollOpt::empty())
    }

    fn disable_stdin(&mut self) -> std::io::Result<()> {
        self.poll.reregister(&self.event_stdin, NcCore::TK_STDIN, Ready::empty(),
                        PollOpt::empty())
    }

    fn enable_stdout(&mut self) -> std::io::Result<()> {
        self.poll.reregister(&self.event_stdout, NcCore::TK_STDOUT, Ready::writable(),
                        PollOpt::empty())
    }

    fn disable_stdout(&mut self) -> std::io::Result<()> {
        self.poll.reregister(&self.event_stdout, NcCore::TK_STDOUT, Ready::empty(),
                        PollOpt::empty())
    }

    fn remove_net(&mut self) -> std::io::Result<()> {
        remove_item(&mut self.open_ends, NcCore::NETIN_POLL);
        remove_item(&mut self.open_ends, NcCore::NETOUT_POLL);
        self.poll.deregister(&self.event_net)
    }

    fn read_stdin(&mut self) -> std::io::Result<()> {
        let mut remove = false;
        match io::stdin().read(&mut self.stdinbuf[self.stdinbufpos..]) {
            Ok(len) => {
                remove = len == 0;
                self.stdinbufpos += len;
            },
            Err(e) => {
                match e.kind() {
                    ErrorKind::Interrupted => {},
                    _ => remove = true
                }
            },
        }
        if remove {
            return self.remove_netin();
        }
        Ok(())
    }

    fn write_netout(&mut self) -> std::io::Result<()> {
        let mut remove = false;
        match self.sock.write(&mut self.stdinbuf[0..self.stdinbufpos]) {
            Ok(len) => {
                debug_info(&format!("write ok len={}", len));
                // remove = len == 0;
                if len > 0 {
                    self.stdinbufpos -= len;
                    if self.stdinbufpos > 0 {
                        self.stdinbuf_bac.copy_from_slice(&self.stdinbuf[len..len+self.stdinbufpos]);
                        self.stdinbuf.copy_from_slice(&self.stdinbuf_bac);
                    }
                }
            },
            Err(e) => {
                debug_info(&format!("write error {:?}", e));
                match e.kind() {
                    ErrorKind::Interrupted => {},
                    _ => remove = true
                }
            },
        }
        if remove {
            return self.remove_netout();
        }
        Ok(())
    }

    fn read_netin(&mut self) -> std::io::Result<()> {
        let mut remove = false;
        match self.sock.read(&mut self.netinbuf[self.netinbufpos..]) {
            Ok(len) => {
                remove = len == 0;
                self.netinbufpos += len;
            },
            Err(e) => {
                match e.kind() {
                    ErrorKind::Interrupted => {},
                    _ => remove = true
                }
            },
        }
        if remove {
            return self.remove_netin();
        }
        Ok(())
    }

    fn write_stdout(&mut self) -> std::io::Result<()> {
        let mut remove = false;
        match io::stdout().write(&mut self.netinbuf[0..self.netinbufpos]) {
            Ok(len) => {
                // remove = len == 0;
                if len > 0 {
                    self.netinbufpos -= len;
                    if self.netinbufpos > 0 {
                        self.netinbuf_bac.copy_from_slice(&self.netinbuf[len..len+self.netinbufpos]);
                        self.netinbuf.copy_from_slice(&self.netinbuf_bac);
                    }
                }
            },
            Err(e) => {
                match e.kind() {
                    ErrorKind::Interrupted => {},
                    _ => remove = true
                }
            },
        }  
        if remove {
            return self.remove_stdout();
        }
        Ok(())           
    }

    fn handle_error_event(&mut self, event: &Event) -> std::io::Result<()> {
        match event.token() {
            NcCore::TK_STDIN => self.remove_stdin(),
            NcCore::TK_STDOUT => self.remove_stdout(),
            NcCore::TK_NET => self.remove_net(),
            _ => unreachable!()
        }
    }

    fn handle_hup_event(&mut self, event: &Event) -> std::io::Result<()> {
        if !self.stdin_gone() && event.token() == NcCore::TK_STDIN &&
            !event.readiness().is_readable() {
            self.remove_stdin()?
        }

        if !self.netin_gone() && event.token() == NcCore::TK_NET &&
            !event.readiness().is_readable() {
            self.remove_netin()?
        }

        if event.token() == NcCore::TK_NET {
            debug_info("STDOUT HUP");
            // TODO: check Nflag
            self.sock.shutdown(std::net::Shutdown::Write)?;
            self.remove_netout()?
        }

        Ok(())
    }

    fn debug_print_ready_end(event :&Event, last_ready_end: &mut i32) {
        let new_ready_end = match event.token() {
            NcCore::TK_STDIN => NcCore::STDIN_POLL,
            NcCore::TK_STDOUT => NcCore::STDOUT_POLL,
            NcCore::TK_NET => {
                let mut val = 0;
                if event.readiness().is_readable() {
                    val |= NcCore::NETIN_POLL;
                }
                if event.readiness().is_writable() {
                    val |= NcCore::NETOUT_POLL;
                }
                val
            }
            _ => -1
        };
        if *last_ready_end != new_ready_end {
            debug_info(&format!("new_ready_end {:?}", new_ready_end));
        } else {
            if *last_ready_end & (NcCore::STDIN_POLL | NcCore::NETIN_POLL) != 0 {
                debug_info(&format!("new_ready_end {:?}", new_ready_end));
            }
        }
        *last_ready_end = new_ready_end;    
    }
}

fn local_listen(opts: &NcOptions) -> Result<Socket, MesaError> {
    debug_info("local_listen");

    let addrs_iter = (&opts.host as &str, opts.portlist[0]).to_socket_addrs()?;

    for addr in addrs_iter {
        let family = match addr {
            SocketAddr::V4(_) => socket2::Domain::ipv4(),
            SocketAddr::V6(_) => socket2::Domain::ipv6(),
        };

        let sock_type = if opts.uflag {
            socket2::Type::dgram()
        } else {
            socket2::Type::stream()
        };

        let sock = Socket::new(family, sock_type, None)?;

        debug_info("local_listen binding");
        match sock.bind(&socket2::SockAddr::from(addr)) {
                Ok(_) => {
                    if !opts.uflag {
                        sock.listen(128)?;
                        debug_info(&format!("local_listen returning sock.as_raw_fd() = {}", sock.as_raw_fd()));
                    }
                    debug_info("local_listen bind finish");
                    return Ok(sock);
                }
                Err(_) => {
                    debug_info("local_listen err, continue");
                    continue
                }
        };
    }

    return mesaerr_result("local_listen failed");
}

fn remote_connect(opts: &NcOptions, port: u16) -> Result<Socket, MesaError>{
    let addrs_iter = (&opts.host as &str, port).to_socket_addrs()?;

    for addr in addrs_iter{
        let sock_domain = match addr {
            SocketAddr::V4(_) => socket2::Domain::ipv4(),
            SocketAddr::V6(_) => socket2::Domain::ipv6(),
        };

        let sock_type = if opts.uflag {
            socket2::Type::dgram()
        } else {
            socket2::Type::stream()
        };

        let sock = Socket::new(sock_domain, sock_type, None)?;

        if opts.s_addr.is_some() || opts.pflag {
            // TODO: implement
        }

        // TODO: maybe sometimes no timeout
        match sock.connect_timeout(&socket2::SockAddr::from(addr), Duration::new(1, 0)) {
            Ok(_) => return Ok(sock),
            Err(_) => {
                if opts.vflag {
                    let connection_type = if opts.uflag {
                        "udp"
                    } else {
                        "tcp"
                    };
                    warn(&format!("connect to {} port {} ({}) failed", opts.host, port, connection_type));
                }
            }
        }
    }
    mesaerr_result("local_listen failed")
}

/*
 * unix_bind()
 * Returns a unix socket bound to the given path
 */
fn unix_bind(path: &str, opts: &NcOptions) -> Result<Socket, MesaError> {
    let sock_type = if opts.uflag {
        socket2::Type::dgram()
    } else {
        socket2::Type::stream()
    };

    let sock = Socket::new(Domain::unix(), sock_type, None)?;
    sock.bind(&socket2::SockAddr::unix(path)?)?;
    Ok(sock)
}

/*
 * unix_listen()
 * Create a unix domain socket, and listen on it.
 */
fn unix_listen(path: &str) -> Result<Socket, MesaError> {
    let sock = Socket::new(Domain::unix(), socket2::Type::stream(), None)?;
    sock.bind(&socket2::SockAddr::unix(path)?)?;
    sock.listen(5)?;
    Ok(sock)
}

fn server(opts: &NcOptions) -> Result<(), MesaError> {
    let mut sock :Socket =  Socket::new(socket2::Domain::ipv4(), socket2::Type::stream(), None)?;
    if opts.family == AF_UNIX {
        sock = if opts.uflag {
            unix_bind(&opts.host, &opts)?
        } else {
            unix_listen(&opts.host)?
        }
    }

    loop {
        if opts.family != AF_UNIX {
            sock = local_listen(opts)?;
        }
        // TODO: implement
        // /*
        //  * For UDP and -k, don't connect the socket, let it
        //  * receive datagrams from multiple socket pairs.
        //  */
        // if opts.uflag && opts.kflag {
        //     readwrite(sock.as_raw_fd(), opts);
        // }
        // /*
        //  * For UDP and not -k, we will use recvfrom() initially
        //  * to wait for a caller, then use the regular functions
        //  * to talk to the caller.
        //  */
        // else if opts.uflag && !opts.kflag {

        if opts.uflag {
            let mut netinbuf: [u8; BUFSIZE] = [0; BUFSIZE];
            let (_, sockaddr) = sock.peek_from(&mut netinbuf)?;
            sock.connect(&sockaddr)?;

            if opts.vflag {
                eprintln!("Connection from {:?} received!", sockaddr);
            }

            NcCore::run(&mut sock, opts)?;
        } else {
            debug_info(&format!("sock = {:?}", sock));
            let (mut sock_conn, sockaddr) = sock.accept()?;
            if opts.vflag {
                eprintln!("Connection from {:?} received!", sockaddr);
            }

            NcCore::run(&mut sock_conn, opts)?;
            // TODO: sock_conn.shutdown(std::net::Shutdown::Both);
        }

        // TODO: implement
        // if opts.family != AF_UNIX {

        // }

        if !opts.kflag {
            break;
        }
    }


    return Ok(());
}

/*
 * unix_connect()
 * Returns a socket connected to a local unix socket. Returns -1 on failure.
 */
fn unix_connect(path: &str, opts: &NcOptions) -> Result<Socket, MesaError> {
    let sock = if opts.uflag {
        unix_bind(&opts.unix_dg_tmp_socket, opts)?
    } else {
        Socket::new(Domain::unix(), socket2::Type::stream(), None)?
    };

    sock.connect(&socket2::SockAddr::unix(path)?)?;

    Ok(sock)
}

fn unix_client(opts: &NcOptions) -> Result<(), MesaError> {
    debug_info("unix_client");

    let mut sock = unix_connect(&opts.host, opts)?;

    if !opts.zflag {
        // readwrite(&mut sock, &opts);
        NcCore::run(&mut sock, opts)?;
    } else {
        return mesaerr_result("TODO: unknown error");
    }

    if opts.uflag {
        std::fs::remove_file(&opts.unix_dg_tmp_socket)?;
    }

    Ok(())
}

fn nonunix_client(opts: &NcOptions) -> Result<(), MesaError> {
    for port in &opts.portlist {
        let mut sock = match remote_connect(opts, port.clone()) {
            Ok(expr) => expr,
            Err(_) => continue,
        };

        // if opts.vflag || opts.zflag {
        //     // TODO: implement
        // }

        // TODO: Fflag && !zflag
        NcCore::run(&mut sock, opts)?;
    }
    Ok(())
}

pub fn execute<S, T>(_setup: &mut S, args: T) -> Result<(), MesaError>
where
    S: UtilSetup,
    T: ArgsIter,
{
    let mut help_msg: Vec<u8> = Vec::new();
    let app = util_app!(NAME)
        .arg(Arg::with_name("l")
            .short("l"))
        .arg(Arg::with_name("i")
            .short("i")
            .value_name("interval")
            .takes_value(true))
        .arg(Arg::with_name("s")
            .short("s")
            .value_name("source_ip_address") 
            .takes_value(true))        
        .arg(Arg::with_name("d")
            .short("d"))
        .arg(Arg::with_name("U")
            .short("U"))
        .arg(Arg::with_name("u")
            .short("u"))
        .arg(Arg::with_name("v")
            .short("v"))
        .arg(Arg::with_name("k")
            .short("k"))
        .arg(Arg::with_name("z")
            .short("z"))
        .arg(Arg::with_name("positionals")
            .multiple(true));

    app.write_help(&mut help_msg)?;
    let help_msg = String::from_utf8(help_msg)?;
    let matches = app.get_matches_from_safe(args)?;

    // println!("matches = {:?}", matches);
    let opts = NcOptions::parse(matches, &help_msg)?;

    if opts.lflag {
        return server(&opts);
    } else {
        if opts.family == AF_UNIX {
            unix_client(&opts)?;
        } else {
            nonunix_client(&opts)?;
        }
    }

    Ok(())
}

