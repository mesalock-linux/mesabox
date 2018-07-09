use std::borrow::{Borrow, Cow};
use std::collections::HashMap;
use std::ffi::{OsString, OsStr};
use std::fs::File;
use std::hash::Hash;
use std::process::Stdio;
use std::os::unix::io::{AsRawFd, FromRawFd};
use std::iter::{FromIterator, FusedIterator};
use std::mem;
use std::rc::Rc;
use std::slice;

use super::error::CommandError;
use super::ast::FunctionBody;
use super::builtin::{Builtin, BuiltinSet};
use super::option::FD_COUNT;
use util::RawFdWrapper;

// XXX: not exactly happy that we need to clone the data for Piped, but due to issues with
//      lifetimes in SimpleCommand::run_command() and IoRedirect::setup() the only alternative
//      seem to be dropping into unsafe code
// XXX: if output is a Vec<u8>, maybe just wait to gather all output until end?  In this situation
//      everything would need to be treated as a foreground process with no pipes, but maybe this
//      would be fine?  only other way i can think of might be shared memory (probably posix shared
//      memory?) because we can get a file descriptor from that
#[derive(Debug)]
pub enum EnvFd {
    Null,
    Inherit,
    Piped(Vec<u8>),
    File(File),
    Fd(RawFdWrapper),
    // TODO: child stdin/stdout/stderr
}

impl EnvFd {
    pub fn into_stdio(self) -> Result<Stdio, CommandError> {
        use self::EnvFd::*;

        Ok(match self {
            Null => Stdio::null(),
            Inherit => Stdio::inherit(),
            Piped(_) => Stdio::piped(),
            File(file) => file.into(),
            Fd(fd) => {
                // XXX: make sure this is right with the stdlib and such
                let new_fd = fd.dup_sh().map_err(|e| CommandError::DupFd { fd: fd.fd, err: e })?;
                unsafe { Stdio::from_raw_fd(new_fd) }
            }
        })
    }

    pub fn try_clone(&self) -> Result<Self, CommandError> {
        Ok(match self {
            EnvFd::Fd(fd) => EnvFd::Fd(fd.clone()),
            EnvFd::File(file) => {
                // XXX: maybe just convert into Fd?
                let fd = file.as_raw_fd();
                let new_fd = RawFdWrapper::new(fd, false, false).dup_sh().map_err(|e| CommandError::DupFd { fd: fd, err: e })?;
                let new_file = unsafe { File::from_raw_fd(new_fd) };
                EnvFd::File(new_file)
            }
            EnvFd::Piped(data) => EnvFd::Piped(data.clone()),
            EnvFd::Inherit => EnvFd::Inherit,
            EnvFd::Null => EnvFd::Null,
        })
    }
}

impl Default for EnvFd {
    fn default() -> Self {
        EnvFd::Null
    }
}

#[derive(Debug)]
pub enum Locality<T> {
    Local(T, usize, Option<Box<Locality<T>>>),
    Global(T),
}

impl<T> Locality<T> {
    pub fn enter_scope(&mut self) {
        if let Locality::Local(_, ref mut count, _) = self {
            *count += 1;
        }
    }

    pub fn exit_scope(&mut self) {
        use self::Locality::*;

        let prev = match self {
            Local(_, 0, ref mut prev) => mem::replace(prev, None),
            Local(_, ref mut count, _) => {
                *count -= 1;
                return
            }
            Global(_) => return,
        };

        // NOTE: this should be fine as exit_scope() should not be called in the base scope
        mem::replace(self, *prev.unwrap());
    }

    pub fn current_val(&self) -> &T {
        use self::Locality::*;

        match self {
            Local(val, _, _) => val,
            Global(val) => val,
        }
    }

    pub fn current_val_mut(&mut self) -> &mut T {
        use self::Locality::*;

        match self {
            Local(val, _, _) => val,
            Global(val) => val,
        }
    }
}

impl<T: Default> Default for Locality<T> {
    fn default() -> Self {
        Locality::Global(T::default())
    }
}

#[derive(Debug)]
pub struct Environment {
    vars: HashMap<OsString, OsString>,
    export_vars: HashMap<OsString, Option<OsString>>,
    funcs: HashMap<OsString, Rc<FunctionBody>>,
    fds: [Locality<EnvFd>; FD_COUNT],

    // BuiltinSet is designed so that by enabling options the set of builtins can be changed
    builtins: BuiltinSet,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
            export_vars: HashMap::new(),
            funcs: HashMap::new(),

            fds: Default::default(),

            builtins: BuiltinSet::new(vec![]),
        }
    }

    // NOTE: using Cow with OsStr is annoying as From<OsStr> is not implemented apparently
    // TODO: switch to entry api
    pub fn set_var(&mut self, name: Cow<OsStr>, new_val: OsString) -> Option<OsString> {
        //self.vars.get_mut(name).map(|value| mem::replace(value, new_val)).or_else(|| self.vars.insert(name.clone().into(), new_val))
        // this is why NLL will be a good thing
        if let Some(value) = self.vars.get_mut::<OsStr>(name.as_ref()) {
            return Some(mem::replace(value, new_val));
        } else if let Some(value) = self.export_vars.get_mut::<OsStr>(name.as_ref()) {
            return mem::replace(value, Some(new_val));
        }
        self.vars.insert(name.into_owned(), new_val)
    }

    pub fn set_export_var(&mut self, name: Cow<OsStr>, new_val: OsString) -> Option<Option<OsString>> {
        if let Some(value) = self.export_vars.get_mut::<OsStr>(name.as_ref()) {
            return Some(mem::replace(value, Some(new_val)));
        }
        self.export_vars.insert(name.into_owned(), Some(new_val))
    }

    pub fn export_var(&mut self, name: Cow<OsStr>) {
        if let Some(value) = self.vars.remove::<OsStr>(name.as_ref()) {
            self.export_vars.insert(name.into_owned(), Some(value));
        } else if !self.export_vars.contains_key::<OsStr>(name.as_ref()) {
            self.export_vars.insert(name.into_owned(), None);
        }
    }

    pub fn get_var<Q: ?Sized>(&self, name: &Q) -> Option<&OsString>
    where
        Q: AsRef<OsStr>,
    {
        let name = name.as_ref();
        // XXX: maybe the opposite order is better, not sure
        self.vars.get(name).or_else(|| self.export_vars.get(name).and_then(|var| var.as_ref()))
    }

    pub fn remove_var<Q: ?Sized>(&mut self, name: &Q) -> Option<OsString>
    where
        Q: AsRef<OsStr>,
    {
        let name = name.as_ref();
        self.vars.remove(name).or_else(|| self.export_vars.remove(name).and_then(|var| var))
    }

    fn set_fd(&mut self, fd: usize, value: Locality<EnvFd>) -> Locality<EnvFd> {
        mem::replace(&mut self.fds[fd], value)
    }

    pub fn set_global_fd(&mut self, fd: usize, value: EnvFd) -> Locality<EnvFd> {
        self.set_fd(fd, Locality::Global(value))
    }

    pub fn set_local_fd(&mut self, fd: usize, value: EnvFd) -> Locality<EnvFd> {
        let current = mem::replace(&mut self.fds[fd], Locality::default());

        if let Locality::Local(_, 0, prev) = current {
            self.fds[fd] = Locality::Local(value, 0, prev);
            Default::default()
        } else {
            let new_val = Locality::Local(value, 0, Some(Box::new(current)));
            self.set_fd(fd, new_val)
        }
    }

    pub fn get_fd(&self, fd: usize) -> &Locality<EnvFd> {
        &self.fds[fd]
    }

    pub fn fds(&self) -> &[Locality<EnvFd>; FD_COUNT] {
        &self.fds
    }

    pub fn current_fds(&mut self) -> CurrentFdIter {
        CurrentFdIter { fd_iter: self.fds.iter_mut() }
    }

    pub fn enter_scope(&mut self) {
        for fd in &mut self.fds {
            fd.enter_scope();
        }
    }

    pub fn exit_scope(&mut self) {
        for fd in &mut self.fds {
            fd.exit_scope();
        }
    }

    pub fn set_func<Q: ?Sized>(&mut self, name: &Q, new_val: Rc<FunctionBody>) -> Option<Rc<FunctionBody>>
    where
        OsString: Borrow<Q>,
        Q: Hash + Eq + Clone + Into<OsString>,
    {
        if let Some(value) = self.funcs.get_mut(name) {
            return Some(mem::replace(value, new_val));
        }
        self.funcs.insert(name.clone().into(), new_val)
    }

    pub fn get_func<Q: ?Sized>(&self, name: &Q) -> Option<Rc<FunctionBody>>
    where
        OsString: Borrow<Q>,
        Q: Hash + Eq,
    {
        self.funcs.get(name).map(|func| func.clone())
    }

    pub fn remove_func<Q: ?Sized>(&mut self, name: &Q) -> Option<Rc<FunctionBody>>
    where
        Q: AsRef<OsStr>,
    {
        self.funcs.remove(name.as_ref())
    }

    pub fn get_builtin<Q: ?Sized>(&self, name: &Q) -> Option<Builtin>
    where
        Q: AsRef<OsStr>,
    {
        self.builtins.find(name.as_ref())
    }

    pub fn iter(&self) -> EnvIter<impl Iterator<Item = (&OsStr, &OsStr)>> {
        EnvIter {
            inner: self.vars.iter().map(|(key, val)| (key.as_ref(), val.as_ref())).chain(self.export_iter()),
        }
    }

    pub fn export_iter(&self) -> EnvIter<impl Iterator<Item = (&OsStr, &OsStr)>> {
        EnvIter {
            inner: self.export_vars.iter().filter_map(|(key, val)| {
                if let Some(val) = val {
                    Some((key.as_ref(), val.as_ref()))
                } else {
                    None
                }
            })
        }
    }
}

impl FromIterator<(OsString, OsString)> for Environment {
    fn from_iter<I: IntoIterator<Item = (OsString, OsString)>>(iter: I) -> Self {
        iter.into_iter().into()
    }
}

impl<T: Iterator<Item = (OsString, OsString)>> From<T> for Environment {
    fn from(iter: T) -> Self {
        Self {
            vars: iter.collect(),
            export_vars: HashMap::new(),
            funcs: HashMap::new(),

            fds: Default::default(),

            builtins: BuiltinSet::new(vec![]),
        }
    }
}

pub struct CurrentFdIter<'a> {
    fd_iter: slice::IterMut<'a, Locality<EnvFd>>,
}

impl<'a> Iterator for CurrentFdIter<'a> {
    type Item = &'a mut EnvFd;

    fn next(&mut self) -> Option<Self::Item> {
        self.fd_iter.next().map(|value| value.current_val_mut())
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.fd_iter.size_hint()
    }
}

#[derive(Clone, Debug)]
pub struct EnvIter<'a, I: Iterator<Item = (&'a OsStr, &'a OsStr)>> {
    inner: I,
}

impl<'a, I: Iterator<Item = (&'a OsStr, &'a OsStr)>> Iterator for EnvIter<'a, I> {
    type Item = (&'a OsStr, &'a OsStr);

    fn next(&mut self) -> Option<(&'a OsStr, &'a OsStr)> {
        self.inner.next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }
}

impl<'a, I: Iterator<Item = (&'a OsStr, &'a OsStr)>> ExactSizeIterator for EnvIter<'a, I> { }
impl<'a, I: Iterator<Item = (&'a OsStr, &'a OsStr)>> FusedIterator for EnvIter<'a, I> { }
