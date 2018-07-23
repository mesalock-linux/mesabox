use std::ffi::OsStr;
use std::os::unix::ffi::OsStrExt;
use std::os::unix::io::{AsRawFd, RawFd};

impl super::OsStrExt for OsStr {
    fn try_as_bytes(&self) -> Option<&[u8]> {
        self.as_bytes()
    }
}

pub type RawObject = RawFd;

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

    // implement here until TryFrom trait is stable
    pub fn try_from(fd: RawObject) -> nix::Result<Self> {
        use nix::fcntl::OFlag;

        let res = fcntl::fcntl(fd, fcntl::F_GETFL)?;
        let mode = OFlag::from_bits(res & OFlag::O_ACCMODE.bits()).unwrap();

        Ok(RawObjectWrapper::new(
            fd,
            mode == OFlag::O_RDONLY || mode == OFlag::O_RDWR,
            mode == OFlag::O_WRONLY || mode == OFlag::O_RDWR,
        ))
    }

    /// Duplicate a file descriptor such that it is greater than or equal to `min`.
    pub fn dup_above(&self, min: RawObject) -> nix::Result<RawObject> {
        fcntl::fcntl(self.fd, fcntl::F_DUPFD(min))
    }

    /// Duplicate a file descriptor such that it is greater than or equal to 10.
    #[cfg(feature = "sh")]
    pub fn dup_sh(&self) -> nix::Result<RawObject> {
        self.dup_above(::posix::sh::option::FD_COUNT as RawFd + 1)
    }
}

impl Read for RawObjectWrapper {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        unistd::read(self.fd, buf).map_err(|_| io::Error::last_os_error())
    }
}

impl Write for RawObjectWrapper {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        unistd::write(self.fd, buf).map_err(|_| io::Error::last_os_error())
    }

    fn flush(&mut self) -> io::Result<()> {
        // XXX: may want to try fsync() or something and ignore failures due to invalid fd type
        Ok(())
    }
}

/// Determine whether the given file descriptor is a TTY.
pub(crate) fn is_tty(stream: Option<RawObject>) -> bool {
    stream
        .map(|fd| unsafe { libc::isatty(fd) == 1 })
        .unwrap_or(false)
}

impl<'a> UtilRead<'a> for File {
    type Lock = BufReader<&'a mut Self>;

    fn lock_reader<'b: 'a>(&'b mut self) -> StdResult<Self::Lock, LockError> {
        Ok(BufReader::new(self))
    }

    fn raw_fd(&self) -> Option<RawFd> {
        Some(self.as_raw_fd())
    }
}

impl<'a> UtilRead<'a> for io::Stdin {
    type Lock = io::StdinLock<'a>;

    fn lock_reader<'b: 'a>(&'b mut self) -> StdResult<Self::Lock, LockError> {
        Ok(self.lock())
    }

    fn raw_fd(&self) -> Option<RawFd> {
        Some(self.as_raw_fd())
    }
}

impl<'a> UtilWrite<'a> for File {
    type Lock = BufWriter<&'a mut Self>;

    fn lock_writer<'b: 'a>(&'b mut self) -> StdResult<Self::Lock, LockError> {
        Ok(BufWriter::new(self))
    }

    fn raw_fd(&self) -> Option<RawFd> {
        Some(self.as_raw_fd())
    }
}

impl<'a> UtilWrite<'a> for io::Stdout {
    type Lock = io::StdoutLock<'a>;

    fn lock_writer<'b: 'a>(&'b mut self) -> StdResult<Self::Lock, LockError> {
        Ok(self.lock())
    }

    fn raw_fd(&self) -> Option<RawFd> {
        Some(self.as_raw_fd())
    }
}

impl<'a> UtilWrite<'a> for io::Stderr {
    type Lock = io::StderrLock<'a>;

    fn lock_writer<'b: 'a>(&'b mut self) -> StdResult<Self::Lock, LockError> {
        Ok(self.lock())
    }

    fn raw_fd(&self) -> Option<RawFd> {
        Some(self.as_raw_fd())
    }
}
