use kernel32;
use winapi::um::winnt::DUPLICATE_SAME_ACCESS;
use winapi::um::{consoleapi, fileapi};
use winapi::shared::{ntdef, minwindef};

use std::ffi::OsStr;
use std::fs::File;
use std::io::{self, BufReader, BufWriter, Read, Write};
use std::os::windows::io::{AsRawHandle, RawHandle};
use std::ptr;

use {UtilRead, UtilWrite, LockError};

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
pub struct RawObject(RawHandle);

impl RawObject {
    pub fn new(handle: RawHandle) -> Self {
        RawObject(handle)
    }

    pub fn raw_value(&self) -> RawHandle {
        self.0
    }
}

unsafe impl Send for RawObject { }
unsafe impl Sync for RawObject { }

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
        let mut duplicate = ptr::null_mut();
        let res = unsafe {
            kernel32::DuplicateHandle(
                kernel32::GetCurrentProcess(),
                self.object.0,
                kernel32::GetCurrentProcess(),
                &mut duplicate,
                0,
                minwindef::FALSE,
                DUPLICATE_SAME_ACCESS
            )
        };
        // == 0 is correct
        if res == 0 {
            Err(io::Error::last_os_error())
        } else {
            Ok(RawObject(duplicate))
        }
    }
}

impl Read for RawObjectWrapper {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let mut bytes_read = 0;
        let res = unsafe { fileapi::ReadFile(self.object.0, buf.as_mut_ptr() as _, buf.len() as _, &mut bytes_read, ntdef::NULL as _) };
        if res == 0 {
            Err(io::Error::last_os_error())
        } else {
            Ok(bytes_read as _)
        }
    }
}

impl Write for RawObjectWrapper {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let mut bytes_written = 0;
        let res = unsafe { fileapi::WriteFile(self.object.0, buf.as_ptr() as _, buf.len() as _, &mut bytes_written, ntdef::NULL as _) };
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

/// Determine whether the given file descriptor is a TTY.
pub(crate) fn is_tty(stream: Option<RawObject>) -> bool {
    stream
        .map(|obj| {
            let mut out = 0;
            let res = unsafe { consoleapi::GetConsoleMode(obj.0, &mut out) };
            res != 0
        }).unwrap_or(false)
}

impl<'a> UtilRead<'a> for File {
    type Lock = BufReader<&'a mut Self>;

    fn lock_reader<'b: 'a>(&'b mut self) -> Result<Self::Lock, LockError> {
        Ok(BufReader::new(self))
    }

    fn raw_object(&self) -> Option<RawObject> {
        Some(RawObject(self.as_raw_handle()))
    }
}

impl<'a> UtilRead<'a> for io::Stdin {
    type Lock = io::StdinLock<'a>;

    fn lock_reader<'b: 'a>(&'b mut self) -> Result<Self::Lock, LockError> {
        Ok(self.lock())
    }

    fn raw_object(&self) -> Option<RawObject> {
        Some(RawObject(self.as_raw_handle()))
    }
}

impl<'a> UtilWrite<'a> for File {
    type Lock = BufWriter<&'a mut Self>;

    fn lock_writer<'b: 'a>(&'b mut self) -> Result<Self::Lock, LockError> {
        Ok(BufWriter::new(self))
    }

    fn raw_object(&self) -> Option<RawObject> {
        Some(RawObject(self.as_raw_handle()))
    }
}

impl<'a> UtilWrite<'a> for io::Stdout {
    type Lock = io::StdoutLock<'a>;

    fn lock_writer<'b: 'a>(&'b mut self) -> Result<Self::Lock, LockError> {
        Ok(self.lock())
    }

    fn raw_object(&self) -> Option<RawObject> {
        Some(RawObject(self.as_raw_handle()))
    }
}

impl<'a> UtilWrite<'a> for io::Stderr {
    type Lock = io::StderrLock<'a>;

    fn lock_writer<'b: 'a>(&'b mut self) -> Result<Self::Lock, LockError> {
        Ok(self.lock())
    }

    fn raw_object(&self) -> Option<RawObject> {
        Some(RawObject(self.as_raw_handle()))
    }
}
