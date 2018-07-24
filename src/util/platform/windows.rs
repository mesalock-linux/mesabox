use kernel32;
use winapi::shared::{minwindef, ntdef};
use winapi::um::minwinbase::SECURITY_ATTRIBUTES;
use winapi::um::winnt::DUPLICATE_SAME_ACCESS;
use winapi::um::{consoleapi, fileapi, handleapi, namedpipeapi};

use std::ffi::OsStr;
use std::fs::File;
use std::io::{self, Read, Write};
use std::mem;
use std::net::TcpStream;
use std::os::windows::io::{AsRawHandle, AsRawSocket, RawHandle, RawSocket};
use std::ptr;

use super::{AsRawObject, PipeKind};

impl super::OsStrExt for OsStr {
    fn try_as_bytes(&self) -> Result<&[u8], super::Utf8Error> {
        match self.to_str() {
            Some(s) => Ok(s.as_bytes()),
            None => Err(super::Utf8Error(self.to_owned())),
        }
    }
}

// The system native representation of files, stdin/stdout/stderr, etc.
#[derive(Copy, Clone, Debug)]
pub enum RawObject {
    Handle(RawHandle),
    Socket(RawSocket),
}

impl RawObject {
    pub fn new(handle: RawHandle) -> Self {
        RawObject::Handle(handle)
    }

    pub fn raw_handle(&self) -> Option<RawHandle> {
        match self {
            RawObject::Handle(handle) => Some(*handle),
            _ => None,
        }
    }

    pub fn raw_socket(&self) -> Option<RawSocket> {
        match self {
            RawObject::Socket(socket) => Some(*socket),
            _ => None,
        }
    }
}

impl From<RawHandle> for RawObject {
    fn from(handle: RawHandle) -> Self {
        RawObject::Handle(handle)
    }
}

impl From<RawSocket> for RawObject {
    fn from(socket: RawSocket) -> Self {
        RawObject::Socket(socket)
    }
}

unsafe impl Send for RawObject {}
unsafe impl Sync for RawObject {}

/// A wrapper around a raw handle that enables reading and writing using the standard traits.  Take
/// note that this wrapper does *NOT* close the file descriptor when dropping.
#[derive(Clone, Debug)]
pub struct RawObjectWrapper {
    pub object: RawObject,
    pub readable: bool,
    pub writable: bool,
}
// TODO: add way to determine if fd is readable and/or writable

impl RawObjectWrapper {
    pub fn new(handle: RawObject, readable: bool, writable: bool) -> Self {
        Self {
            object: handle,
            readable: readable,
            writable: writable,
        }
    }

    pub fn inner_object(&self) -> RawObject {
        self.object
    }

    // implement here until TryFrom trait is stable
    pub fn try_from(fd: RawObject) -> io::Result<Self> {
        // FIXME: dunno how to do this atm
        unimplemented!()
    }

    /// Duplicate a file descriptor such that its new value is `val`.  This is equivalent to `dup2`.
    pub fn dup_as(&self, val: RawObject) -> io::Result<RawObject> {
        unimplemented!()
    }

    /// Duplicate a file descriptor such that it is greater than or equal to `min`.
    pub fn dup_above(&self, min: RawObject) -> io::Result<RawObject> {
        // FIXME: dunno how to do this atm
        unimplemented!()
    }

    /// Duplicate a file descriptor such that it is greater than or equal to 10.
    #[cfg(feature = "sh")]
    pub fn dup_sh(&self) -> io::Result<RawObject> {
        unimplemented!()
        //self.dup_above(::posix::sh::option::FD_COUNT as RawObject + 1)
    }

    pub fn dup(&self) -> io::Result<RawObject> {
        match self.object {
            RawObject::Handle(handle) => {
                let mut duplicate = ptr::null_mut();
                let res = unsafe {
                    kernel32::DuplicateHandle(
                        kernel32::GetCurrentProcess(),
                        handle,
                        kernel32::GetCurrentProcess(),
                        &mut duplicate,
                        0,
                        minwindef::FALSE,
                        DUPLICATE_SAME_ACCESS,
                    )
                };
                // == 0 is correct
                if res == 0 {
                    Err(io::Error::last_os_error())
                } else {
                    Ok(RawObject::Handle(duplicate))
                }
            }
            RawObject::Socket(socket) => {
                // TODO: call WSADuplicateSocketW() and use the returned info to create a new
                //       socket using WSASocket()
                unimplemented!()
            }
        }
    }
}

impl Read for RawObjectWrapper {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        // according to the docs, a socket handle can be used with ReadFile()
        let handle = match self.object {
            RawObject::Handle(handle) => handle,
            RawObject::Socket(socket) => socket as _,
        };

        let mut bytes_read = 0;
        let res = unsafe {
            fileapi::ReadFile(
                handle,
                buf.as_mut_ptr() as _,
                buf.len() as _,
                &mut bytes_read,
                ntdef::NULL as _,
            )
        };
        if res == 0 {
            Err(io::Error::last_os_error())
        } else {
            Ok(bytes_read as _)
        }
    }
}

impl Write for RawObjectWrapper {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        // according to the docs, a socket handle can be used with WriteFile()
        let handle = match self.object {
            RawObject::Handle(handle) => handle,
            RawObject::Socket(socket) => socket as _,
        };

        let mut bytes_written = 0;
        let res = unsafe {
            fileapi::WriteFile(
                handle,
                buf.as_ptr() as _,
                buf.len() as _,
                &mut bytes_written,
                ntdef::NULL as _,
            )
        };
        if res == 0 {
            Err(io::Error::last_os_error())
        } else {
            Ok(bytes_written as _)
        }
    }

    fn flush(&mut self) -> io::Result<()> {
        // XXX: may want to try fsync() or something and ignore failures due to invalid fd type
        Ok(())
    }
}

/// An platform-independent abstraction for pipes that automatically closes the pipe on drop.
#[derive(Debug)]
pub struct Pipe {
    handle: RawHandle,
    kind: PipeKind,
}

impl Pipe {
    pub fn create() -> io::Result<(Self, Self)> {
        let mut read = ptr::null_mut();
        let mut write = ptr::null_mut();
        let mut attributes = SECURITY_ATTRIBUTES {
            nLength: mem::size_of::<SECURITY_ATTRIBUTES>() as _,
            lpSecurityDescriptor: ptr::null_mut(),
            bInheritHandle: minwindef::TRUE,
        };
        let res = unsafe { namedpipeapi::CreatePipe(&mut read, &mut write, &mut attributes, 0) };
        if res == 0 {
            Err(io::Error::last_os_error())
        } else {
            Ok((Pipe::new_read(read), Pipe::new_write(write)))
        }
    }

    pub fn readable(&self) -> bool {
        self.kind == PipeKind::Read
    }

    pub fn writable(&self) -> bool {
        self.kind == PipeKind::Write
    }

    // XXX: might be better to only allow conversion to RawObjectWrapper
    pub fn raw_object(&self) -> RawObject {
        RawObject::Handle(self.handle)
    }

    pub fn raw_object_wrapper(&self) -> RawObjectWrapper {
        RawObjectWrapper::new(
            RawObject::Handle(self.handle),
            self.readable(),
            self.writable(),
        )
    }

    // FIXME: not sure if we really want to dup here
    #[cfg(feature = "sh")]
    pub fn try_clone(&self) -> io::Result<Self> {
        let obj = RawObjectWrapper::new(RawObject::Handle(self.handle), true, true).dup_sh()?;
        Ok(Pipe {
            handle: obj.raw_value(),
            kind: self.kind,
        })
    }

    fn new_read(handle: RawHandle) -> Self {
        Self {
            handle,
            kind: PipeKind::Read,
        }
    }

    fn new_write(handle: RawHandle) -> Self {
        Self {
            handle,
            kind: PipeKind::Write,
        }
    }
}

impl Read for Pipe {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        RawObjectWrapper::new(RawObject::new(self.handle), true, false).read(buf)
    }
}

impl Write for Pipe {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        RawObjectWrapper::new(RawObject::new(self.handle), false, true).write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        RawObjectWrapper::new(RawObject::new(self.handle), false, true).flush()
    }
}

impl AsRawObject for Pipe {
    fn as_raw_object(&self) -> RawObject {
        RawObject::Handle(self.handle)
    }
}

impl Drop for Pipe {
    fn drop(&mut self) {
        // XXX: ignore error?
        unsafe {
            handleapi::CloseHandle(self.handle);
        }
    }
}

/// Determine whether the given file descriptor is a TTY.
pub fn is_tty(stream: Option<RawObject>) -> bool {
    stream
        .map(|obj| match obj {
            RawObject::Handle(handle) => {
                let mut out = 0;
                let res = unsafe { consoleapi::GetConsoleMode(handle, &mut out) };
                res != 0
            }
            _ => false,
        })
        .unwrap_or(false)
}

impl AsRawObject for File {
    fn as_raw_object(&self) -> RawObject {
        RawObject::Handle(self.as_raw_handle())
    }
}

impl AsRawObject for io::Stdin {
    fn as_raw_object(&self) -> RawObject {
        RawObject::Handle(self.as_raw_handle())
    }
}

impl AsRawObject for io::Stdout {
    fn as_raw_object(&self) -> RawObject {
        RawObject::Handle(self.as_raw_handle())
    }
}

impl AsRawObject for io::Stderr {
    fn as_raw_object(&self) -> RawObject {
        RawObject::Handle(self.as_raw_handle())
    }
}

impl AsRawObject for TcpStream {
    fn as_raw_object(&self) -> RawObject {
        RawObject::Socket(self.as_raw_socket())
    }
}

// would very much like to do this but doesn't work
/*
impl<T: AsRawHandle> AsRawObject for T {
    fn as_raw_object(&self) -> RawObject {
        RawObject::Handle(self.as_raw_handle())
    }
}

impl<T: AsRawSocket> AsRawObject for T {
    fn as_raw_object(&self) -> RawObject {
        RawObject::Socket(self.as_raw_socket())
    }
}
*/
