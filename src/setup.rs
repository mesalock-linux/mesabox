use super::{LockError, LockableRead, LockableWrite, UtilRead, UtilWrite};
use std::fs::File;
use std::io::{self, BufRead, BufReader, BufWriter, Empty, Sink, Write};
use std::net::TcpStream;
use std::result::Result as StdResult;
use util::{AsRawObject, RawObject, RawObjectWrapper, ReadableVec, UtilReadDyn, UtilWriteDyn};

impl<'a, 'b, T: UtilRead<'a>> UtilRead<'a> for &'b mut T {
    type Lock = T::Lock;

    fn lock_reader<'c: 'a>(&'c mut self) -> StdResult<Self::Lock, LockError> {
        (**self).lock_reader()
    }

    fn raw_object(&self) -> Option<RawObject> {
        (**self).raw_object()
    }
}

impl<'a, 'b, T: UtilWrite<'a>> UtilWrite<'a> for &'b mut T {
    type Lock = T::Lock;

    fn lock_writer<'c: 'a>(&'c mut self) -> StdResult<Self::Lock, LockError> {
        (**self).lock_writer()
    }

    fn raw_object(&self) -> Option<RawObject> {
        (**self).raw_object()
    }
}

impl<'a, T: UtilRead<'a>> LockableRead<'a> for T {
    fn lock_reader_dyn<'b: 'a>(&'b mut self) -> StdResult<Box<BufRead + 'a>, LockError> {
        self.lock_reader().map(|v| Box::new(v) as Box<BufRead + 'a>)
    }
}

impl<'a, T: UtilWrite<'a>> LockableWrite<'a> for T {
    fn lock_writer_dyn<'b: 'a>(&'b mut self) -> StdResult<Box<Write + 'a>, LockError> {
        self.lock_writer().map(|v| Box::new(v) as Box<Write + 'a>)
    }
}

impl<'a> UtilRead<'a> for UtilReadDyn {
    type Lock = Box<BufRead + 'a>;

    fn lock_reader<'c: 'a>(&'c mut self) -> StdResult<Self::Lock, LockError> {
        self.inner.lock_reader_dyn() as StdResult<Box<BufRead + 'a>, LockError>
    }

    fn raw_object(&self) -> Option<RawObject> {
        self.fd()
    }
}

impl<'a> UtilWrite<'a> for UtilWriteDyn {
    type Lock = Box<Write + 'a>;

    fn lock_writer<'b: 'a>(&'b mut self) -> StdResult<Self::Lock, LockError> {
        self.inner.lock_writer_dyn()
    }

    fn raw_object(&self) -> Option<RawObject> {
        self.fd()
    }
}

// TODO: implement for other common things like File, BufReader, etc.

impl<'a, 'b> UtilRead<'a> for &'b [u8] {
    type Lock = &'a [u8];

    fn lock_reader<'c: 'a>(&'c mut self) -> StdResult<Self::Lock, LockError> {
        Ok(self)
    }
}

impl<'a> UtilRead<'a> for ReadableVec<u8> {
    type Lock = &'a [u8];

    fn lock_reader<'b: 'a>(&'b mut self) -> StdResult<Self::Lock, LockError> {
        Ok(&self.0)
    }
}

impl<'a> UtilWrite<'a> for Vec<u8> {
    type Lock = &'a mut Self;

    fn lock_writer<'b: 'a>(&'b mut self) -> StdResult<Self::Lock, LockError> {
        Ok(self)
    }
}

impl<'a> UtilRead<'a> for RawObjectWrapper {
    type Lock = BufReader<&'a mut Self>;

    fn lock_reader<'b: 'a>(&'b mut self) -> StdResult<Self::Lock, LockError> {
        Ok(BufReader::new(self))
    }

    fn raw_object(&self) -> Option<RawObject> {
        Some(self.inner_object())
    }
}

impl<'a> UtilWrite<'a> for RawObjectWrapper {
    type Lock = BufWriter<&'a mut Self>;

    fn lock_writer<'b: 'a>(&'b mut self) -> StdResult<Self::Lock, LockError> {
        Ok(BufWriter::new(self))
    }

    fn raw_object(&self) -> Option<RawObject> {
        Some(self.inner_object())
    }
}

impl<'a> UtilRead<'a> for Empty {
    type Lock = &'a mut Empty;

    fn lock_reader<'b: 'a>(&'b mut self) -> StdResult<Self::Lock, LockError> {
        Ok(self)
    }
}

impl<'a> UtilWrite<'a> for Sink {
    type Lock = &'a mut Sink;

    fn lock_writer<'b: 'a>(&'b mut self) -> StdResult<Self::Lock, LockError> {
        Ok(self)
    }
}

impl<'a> UtilRead<'a> for File {
    type Lock = BufReader<&'a mut Self>;

    fn lock_reader<'b: 'a>(&'b mut self) -> Result<Self::Lock, LockError> {
        Ok(BufReader::new(self))
    }

    fn raw_object(&self) -> Option<RawObject> {
        Some(self.as_raw_object())
    }
}

impl<'a> UtilRead<'a> for io::Stdin {
    type Lock = io::StdinLock<'a>;

    fn lock_reader<'b: 'a>(&'b mut self) -> Result<Self::Lock, LockError> {
        Ok(self.lock())
    }

    fn raw_object(&self) -> Option<RawObject> {
        Some(self.as_raw_object())
    }
}

impl<'a> UtilWrite<'a> for File {
    type Lock = BufWriter<&'a mut Self>;

    fn lock_writer<'b: 'a>(&'b mut self) -> Result<Self::Lock, LockError> {
        Ok(BufWriter::new(self))
    }

    fn raw_object(&self) -> Option<RawObject> {
        Some(self.as_raw_object())
    }
}

impl<'a> UtilWrite<'a> for io::Stdout {
    type Lock = io::StdoutLock<'a>;

    fn lock_writer<'b: 'a>(&'b mut self) -> Result<Self::Lock, LockError> {
        Ok(self.lock())
    }

    fn raw_object(&self) -> Option<RawObject> {
        Some(self.as_raw_object())
    }
}

impl<'a> UtilWrite<'a> for io::Stderr {
    type Lock = io::StderrLock<'a>;

    fn lock_writer<'b: 'a>(&'b mut self) -> Result<Self::Lock, LockError> {
        Ok(self.lock())
    }

    fn raw_object(&self) -> Option<RawObject> {
        Some(self.as_raw_object())
    }
}

impl<'a> UtilRead<'a> for TcpStream {
    type Lock = BufReader<&'a mut Self>;

    fn lock_reader<'b: 'a>(&'b mut self) -> Result<Self::Lock, LockError> {
        Ok(BufReader::new(self))
    }

    fn raw_object(&self) -> Option<RawObject> {
        Some(self.as_raw_object())
    }
}

impl<'a> UtilWrite<'a> for TcpStream {
    type Lock = BufWriter<&'a mut Self>;

    fn lock_writer<'b: 'a>(&'b mut self) -> Result<Self::Lock, LockError> {
        Ok(BufWriter::new(self))
    }

    fn raw_object(&self) -> Option<RawObject> {
        Some(self.as_raw_object())
    }
}
