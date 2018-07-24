use libc;
use nix::{fcntl, unistd};

use std::ffi::OsStr;
use std::io::{self, Read, Write};
use std::os::unix::ffi::OsStrExt;
use std::os::unix::io::{AsRawFd, FromRawFd, RawFd};
use std::process::Stdio;

use super::{AsRawObject, PipeKind};

impl super::OsStrExt for OsStr {
    fn try_as_bytes(&self) -> Result<&[u8], super::Utf8Error> {
        Ok(self.as_bytes())
    }
}

#[derive(Copy, Clone, Debug)]
pub struct RawObject(RawFd);

impl RawObject {
    pub fn new(fd: RawFd) -> Self {
        RawObject(fd)
    }

    pub fn raw_value(&self) -> RawFd {
        self.0
    }
}

/// A wrapper around a raw file descriptor that enables reading and writing using the standard
/// traits.  Take note that this wrapper does *NOT* close the file descriptor when dropping.
#[derive(Clone, Debug)]
pub struct RawObjectWrapper {
    pub fd: RawObject,
    pub readable: bool,
    pub writable: bool,
}
// TODO: add way to determine if fd is readable and/or writable

impl RawObjectWrapper {
    pub fn new(fd: RawObject, readable: bool, writable: bool) -> Self {
        Self {
            fd: fd,
            readable: readable,
            writable: writable,
        }
    }

    pub fn inner_object(&self) -> RawObject {
        self.fd
    }

    // implement here until TryFrom trait is stable
    pub fn try_from(fd: RawObject) -> io::Result<Self> {
        use nix::fcntl::OFlag;

        let res =
            fcntl::fcntl(fd.raw_value(), fcntl::F_GETFL).map_err(|_| io::Error::last_os_error())?;
        let mode = OFlag::from_bits(res & OFlag::O_ACCMODE.bits()).unwrap();

        Ok(RawObjectWrapper::new(
            fd,
            mode == OFlag::O_RDONLY || mode == OFlag::O_RDWR,
            mode == OFlag::O_WRONLY || mode == OFlag::O_RDWR,
        ))
    }

    /// Duplicate a file descriptor.  This is equivalent to `dup`.
    pub fn dup(&self) -> io::Result<RawObject> {
        unistd::dup(self.fd.raw_value())
            .map(|fd| RawObject(fd))
            .map_err(|_| io::Error::last_os_error())
    }

    /// Duplicate a file descriptor such that its new value is `val`.  This is equivalent to `dup2`.
    pub fn dup_as(&self, val: RawObject) -> io::Result<RawObject> {
        unistd::dup2(self.fd.raw_value(), val.raw_value())
            .map(|fd| RawObject(fd))
            .map_err(|_| io::Error::last_os_error())
    }

    /// Duplicate a file descriptor such that it is greater than or equal to `min`.
    pub fn dup_above(&self, min: RawObject) -> io::Result<RawObject> {
        fcntl::fcntl(self.fd.raw_value(), fcntl::F_DUPFD(min.raw_value()))
            .map(|fd| RawObject(fd))
            .map_err(|_| io::Error::last_os_error())
    }

    /// Duplicate a file descriptor such that it is greater than or equal to 10.
    #[cfg(feature = "sh")]
    pub fn dup_sh(&self) -> io::Result<RawObject> {
        self.dup_above(RawObject(::posix::sh::option::FD_COUNT as RawFd + 1))
    }
}

impl Read for RawObjectWrapper {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        unistd::read(self.fd.raw_value(), buf).map_err(|_| io::Error::last_os_error())
    }
}

impl Write for RawObjectWrapper {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        unistd::write(self.fd.raw_value(), buf).map_err(|_| io::Error::last_os_error())
    }

    fn flush(&mut self) -> io::Result<()> {
        // XXX: may want to try fsync() or something and ignore failures due to invalid fd type
        Ok(())
    }
}

/// An platform-independent abstraction for pipes that automatically closes the pipe on drop.
#[derive(Debug)]
pub struct Pipe {
    fd: RawFd,
    kind: PipeKind,
}

impl Pipe {
    pub fn create() -> io::Result<(Self, Self)> {
        let (read, write) = unistd::pipe().map_err(|_| io::Error::last_os_error())?;
        Ok((Pipe::new_read(read), Pipe::new_write(write)))
    }

    pub fn readable(&self) -> bool {
        self.kind == PipeKind::Read
    }

    pub fn writable(&self) -> bool {
        self.kind == PipeKind::Write
    }

    // XXX: might be better to only allow conversion to RawObjectWrapper
    pub fn raw_object(&self) -> RawObject {
        RawObject(self.fd)
    }

    pub fn raw_object_wrapper(&self) -> RawObjectWrapper {
        RawObjectWrapper::new(RawObject(self.fd), self.readable(), self.writable())
    }

    // FIXME: not sure if we really want to dup here
    #[cfg(feature = "sh")]
    pub fn try_clone(&self) -> io::Result<Self> {
        let obj = RawObjectWrapper::new(RawObject(self.fd), true, true).dup_sh()?;
        Ok(Pipe {
            fd: obj.raw_value(),
            kind: self.kind,
        })
    }

    fn new_read(fd: RawFd) -> Self {
        Self {
            fd,
            kind: PipeKind::Read,
        }
    }

    fn new_write(fd: RawFd) -> Self {
        Self {
            fd,
            kind: PipeKind::Write,
        }
    }
}

impl Read for Pipe {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        RawObjectWrapper::new(RawObject::new(self.fd), true, false).read(buf)
    }
}

impl Write for Pipe {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        RawObjectWrapper::new(RawObject::new(self.fd), false, true).write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        RawObjectWrapper::new(RawObject::new(self.fd), false, true).flush()
    }
}

impl AsRawObject for Pipe {
    fn as_raw_object(&self) -> RawObject {
        RawObject(self.fd)
    }
}

impl Drop for Pipe {
    fn drop(&mut self) {
        // XXX: ignore error?
        unistd::close(self.fd);
    }
}

impl From<Pipe> for Stdio {
    fn from(pipe: Pipe) -> Self {
        use std::mem;

        let obj = pipe.as_raw_object();
        let stdio = unsafe { Stdio::from_raw_fd(obj.raw_value()) };

        // prevent drop from closing the file descriptor as Stdio takes ownership
        mem::forget(pipe);

        stdio
    }
}

/// Determine whether the given file descriptor is a TTY.
pub fn is_tty(stream: Option<RawObject>) -> bool {
    stream
        .map(|fd| unsafe { libc::isatty(fd.raw_value()) == 1 })
        .unwrap_or(false)
}

impl<T: AsRawFd> AsRawObject for T {
    fn as_raw_object(&self) -> RawObject {
        RawObject(self.as_raw_fd())
    }
}
