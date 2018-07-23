#[cfg(unix)]
pub use self::unix::*;
#[cfg(windows)]
pub use self::windows::*;

use std::ffi::OsString;

#[cfg(unix)]
mod unix;
#[cfg(windows)]
mod windows;

#[derive(Debug, Fail)]
#[fail(display = "invalid UTF 8 in {:?}", _0)]
pub struct Utf8Error(OsString);

/// This trait just exists to convert OsStr to byte slices on Windows basically.
pub trait OsStrExt {
    fn try_as_bytes(&self) -> Result<&[u8], Utf8Error>;
}
