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
use super::ast::{ExitCode, FunctionBody};
use super::builtin::{Builtin, BuiltinSet};
use super::option::FD_COUNT;
use util::RawFdWrapper;

pub trait TryClone: Sized {
    fn try_clone(&self) -> Result<Self, CommandError>;
}

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
    Piped(Vec<u8>),
    File(File),
    Fd(RawFdWrapper),
    Pipeline,
    ChildStdout(RawFdWrapper),
}

impl EnvFd {
    pub fn into_stdio(self) -> Result<Stdio, CommandError> {
        use self::EnvFd::*;

        Ok(match self {
            Null => Stdio::null(),
            Piped(_) => Stdio::piped(),
            File(file) => file.into(),
            Fd(fd) | ChildStdout(fd) => {
                // XXX: make sure this is right with the stdlib and such
                let new_fd = fd.dup_sh().map_err(|e| CommandError::DupFd { fd: fd.fd, err: e })?;
                unsafe { Stdio::from_raw_fd(new_fd) }
            }
            Pipeline => Stdio::piped(),
        })
    }
}

impl TryClone for EnvFd {
    fn try_clone(&self) -> Result<Self, CommandError> {
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
            EnvFd::Null => EnvFd::Null,
            EnvFd::Pipeline => EnvFd::Pipeline,
            EnvFd::ChildStdout(fd) => EnvFd::ChildStdout(fd.clone())
        })
    }

    /*pub fn create_pipe() -> Result<Self, CommandError> {
        let (read, write) = unistd::pipe().map_err(|e| CommandError::Pipe(e))?;
        Ok(EnvFd::Pipeline(RawFdWrapper::new(read, true, false), RawFdWrapper::new(write, false, true)))
    }*/
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

    pub fn set_val(&mut self, value: T) {
        match self {
            Locality::Local(ref mut cur_value, 0, _) => *cur_value = value,
            Locality::Local(ref mut cur_value, ref mut count, ref mut prev) => {
                let cur_value = mem::replace(cur_value, value);
                let count = mem::replace(count, 0);
                let prev_val = mem::replace(prev, None);
                *prev = Some(Box::new(Locality::Local(cur_value, count - 1, prev_val)));
            }
            Locality::Global(_) => {
                let old_self = mem::replace(self, Locality::Local(value, 0, None));
                if let Locality::Local(_, _, ref mut prev) = self {
                    *prev = Some(Box::new(old_self));
                }
            }
        }
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

impl<T: Clone> Clone for Locality<T> {
    fn clone(&self) -> Self {
        match self {
            Locality::Local(item, count, prev) => Locality::Local(item.clone(), *count, prev.clone()),
            Locality::Global(item) => Locality::Global(item.clone()),
        }
    }
}

impl<T: TryClone> TryClone for Locality<T> {
    fn try_clone(&self) -> Result<Self, CommandError> {
        Ok(match self {
            Locality::Local(item, count, prev) => {
                let prev = match prev {
                    Some(v) => Some(Box::new(v.try_clone()?)),
                    None => None,
                };
                Locality::Local(item.try_clone()?, *count, prev)
            }
            Locality::Global(item) => Locality::Global(item.try_clone()?),
        })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum CheckBreak {
    None,
    Break,
    Continue,
}

#[derive(Clone, Debug)]
pub struct SpecialVars {
    last_exitcode: ExitCode,
    args: Locality<Vec<OsString>>,
}

impl SpecialVars {
    pub fn new() -> Self {
        Self {
            last_exitcode: 0,
            args: Locality::default(),
        }
    }

    pub fn set_last_exitcode(&mut self, code: ExitCode) {
        self.last_exitcode = code;
    }

    pub fn get_last_exitcode(&self) -> ExitCode {
        self.last_exitcode
    }

    pub fn set_positionals(&mut self, args: Vec<OsString>) {
        self.args.set_val(args);
    }

    pub fn get_positionals(&self) -> &[OsString] {
        self.args.current_val()
    }

    pub fn enter_scope(&mut self) {
        self.args.enter_scope();
    }

    pub fn exit_scope(&mut self) {
        self.args.exit_scope();
    }
}

#[derive(Debug)]
pub struct Environment {
    special_vars: SpecialVars,

    // variables local to the current shell
    vars: HashMap<OsString, OsString>,

    // exportable variables (NOTE: may want to make the value an Arc for this and vars to allow the
    // lock to release as soon as possible)
    export_vars: HashMap<OsString, Option<OsString>>,

    // functions
    funcs: HashMap<OsString, Rc<FunctionBody>>,

    // FIXME: figure out how to make multi-threaded (this might be small enough to just clone rather
    //        than use an Arc)
    fds: [Locality<EnvFd>; FD_COUNT],

    // BuiltinSet is designed so that by enabling options the set of builtins can be changed
    builtins: BuiltinSet,

    // used to track how far back to go for break/continue statements
    break_counter: usize,

    // break/continue type
    break_type: CheckBreak,

    // track how many loops deep we are (if 0, break/continue don't do anything)
    loop_depth: usize,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            special_vars: SpecialVars::new(),

            vars: HashMap::new(),
            export_vars: HashMap::new(),
            funcs: HashMap::new(),

            fds: Default::default(),

            builtins: BuiltinSet::new(vec![]),

            break_counter: 0,
            break_type: CheckBreak::None,
            loop_depth: 0,
        }
    }

    pub fn set_break_type(&mut self, kind: CheckBreak) {
        self.break_type = kind;
    }

    pub fn break_type(&self) -> CheckBreak {
        self.break_type.clone()
    }

    pub fn set_break_counter(&mut self, count: usize) {
        self.break_counter = count;
    }

    pub fn dec_break_counter(&mut self) {
        self.break_counter -= 1;
    }

    pub fn break_counter(&self) -> usize {
        self.break_counter
    }

    pub fn set_loop_depth(&mut self, depth: usize) {
        self.loop_depth = depth;
    }

    pub fn inc_loop_depth(&mut self) {
        self.loop_depth += 1;
    }

    pub fn dec_loop_depth(&mut self) {
        self.loop_depth -= 1;
    }

    pub fn loop_depth(&self) -> usize {
        self.loop_depth
    }

    pub fn special_vars(&mut self) -> &mut SpecialVars {
        &mut self.special_vars
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

    fn set_fd(&mut self, fd: usize, value: Locality<EnvFd>) {
        mem::replace(&mut self.fds[fd], value);
    }

    pub fn set_global_fd(&mut self, fd: usize, value: EnvFd) {
        self.set_fd(fd, Locality::Global(value));
    }

    pub fn set_local_fd(&mut self, fd: usize, value: EnvFd) {
        self.fds[fd].set_val(value);
    }

    pub fn get_fd(&self, fd: usize) -> &Locality<EnvFd> {
        &self.fds[fd]
    }

    pub fn get_fd_mut(&mut self, fd: usize) -> &mut Locality<EnvFd> {
        &mut self.fds[fd]
    }

    pub fn fds(&self) -> &[Locality<EnvFd>; FD_COUNT] {
        &self.fds
    }

    pub fn current_fds(&mut self) -> CurrentFdIter {
        CurrentFdIter { fd_iter: self.fds.iter_mut() }
    }

    // XXX: may want to split into enter_scope_vars() and enter_scope_fds(), and then create an
    //      enter_scope() method that runs them both.  adding locality to the {export_,}vars will
    //      mean either making the hashmaps themselves local or making the variables inside the
    //      hashmaps local.  with many variables and few scopes, i imagine the latter would be
    //      slower than the former (as you would need to enter_scope() every variable within the
    //      hashmaps), but if you have many scopes accessing a variable in the outermost scope
    //      hashmap would be very slow if using the former method (as you would need to hash the
    //      value many times (unless there's a way to retrieve the hash and search the hashmap
    //      using the hash rather than the actual key?))
    //      am just considering writing a custom hashmap that lets us compute the hash once and use
    //      it to check all inner hashmaps because of the latter issue (probably should just make
    //      it a unique structure storing both vars and export_vars in such a case to improve the
    //      performance of setting variables)
    pub fn enter_scope(&mut self) {
        for fd in &mut self.fds {
            fd.enter_scope();
        }
        self.special_vars.enter_scope();
    }

    pub fn exit_scope(&mut self) {
        for fd in &mut self.fds {
            fd.exit_scope();
        }
        self.special_vars.exit_scope();
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
            special_vars: SpecialVars::new(),
            vars: HashMap::new(),
            export_vars: iter.map(|(key, value)| (key, Some(value))).collect(),
            funcs: HashMap::new(),

            fds: Default::default(),

            builtins: BuiltinSet::new(vec![]),

            break_counter: 0,
            break_type: CheckBreak::None,
            loop_depth: 0,
        }
    }
}

impl TryClone for Environment {
    fn try_clone(&self) -> Result<Self, CommandError> {
        let mut fds: [Locality<EnvFd>; FD_COUNT] = Default::default();

        for (new_fd, fd) in fds.iter_mut().zip(self.fds.iter()) {
            *new_fd = fd.try_clone()?;
        }
        Ok(Self {
            special_vars: self.special_vars.clone(),
            vars: self.vars.clone(),
            export_vars: self.export_vars.clone(),
            funcs: self.funcs.clone(),

            fds: fds,

            builtins: self.builtins.clone(),

            break_counter: self.break_counter,
            break_type: self.break_type.clone(),
            loop_depth: self.loop_depth,
        })
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

#[derive(Debug)]
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
