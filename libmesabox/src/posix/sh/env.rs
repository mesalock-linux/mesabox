use std::borrow::{Borrow, Cow};
use std::ffi::{OsStr, OsString};
use std::fs::File;
use std::hash::Hash;
use std::iter::{FromIterator, FusedIterator};
use std::mem;
use std::os::unix::io::FromRawFd;
use std::process::Stdio;
use std::rc::Rc;

use super::ast::FunctionBody;
use super::builtin::{Builtin, BuiltinSet};
use super::error::CommandError;
use super::types::scoped_array::ScopedArrayIter;
use super::types::{FdArray, Locality, Scoped, ScopedMap, TryClone};
use util::{AsRawObject, ExitCode, Pipe, RawObjectWrapper};

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
    Fd(RawObjectWrapper),
    Pipe(Pipe),
    Pipeline,
    ChildStdout(RawObjectWrapper),
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
                // NOTE: the reason we dup here is that these file descriptor/objects are *NOT*
                //       guaranteed to be owned (in fact, they most likely are not)
                let new_fd = fd.dup_sh().map_err(|e| CommandError::DupFd {
                    fd: fd.fd.raw_value(),
                    err: e,
                })?;
                unsafe { Stdio::from_raw_fd(new_fd.raw_value()) }
            }
            Pipe(pipe) => pipe.into(),
            Pipeline => Stdio::piped(),
        })
    }
}

impl TryClone for EnvFd {
    fn try_clone(&self) -> Result<Self, CommandError> {
        Ok(match self {
            EnvFd::Fd(fd) => EnvFd::Fd(fd.clone()),
            EnvFd::File(file) => {
                // XXX: should be fine as we should only use this when borrow checking is too
                //      strict (although the read/write part for RawObjectWrapper is wrong)
                let obj = file.as_raw_object();
                let new_fd = RawObjectWrapper::new(obj, true, true);
                EnvFd::Fd(new_fd)
            }
            EnvFd::Piped(data) => EnvFd::Piped(data.clone()),
            EnvFd::Null => EnvFd::Null,
            EnvFd::Pipeline => EnvFd::Pipeline,
            EnvFd::Pipe(pipe) => {
                // XXX: this behavior should be fine, as try_clone() should only be used for EnvFd
                //      in situations that would be solved by NLL
                EnvFd::Fd(pipe.raw_object_wrapper())
            }
            EnvFd::ChildStdout(fd) => EnvFd::ChildStdout(fd.clone()),
        })
    }
}

impl Default for EnvFd {
    fn default() -> Self {
        EnvFd::Null
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

    pub fn shift_positionals(&mut self) {
        // XXX: maybe switch a VecDeque?  or store the array in reverse?  VecDeque might let us
        //      implement a pop command
        self.args.current_val_mut().remove(0);
    }
}

impl Scoped for SpecialVars {
    fn enter_scope(&mut self) {
        self.args.enter_scope();
    }

    fn exit_scope(&mut self) {
        self.args.exit_scope();
    }
}

#[derive(Debug)]
pub struct Environment {
    special_vars: SpecialVars,

    // variables local to the current shell
    vars: ScopedMap<OsString, OsString>,

    // exportable variables (NOTE: may want to make the value an Arc for this and vars to allow the
    // lock to release as soon as possible)
    export_vars: ScopedMap<OsString, Option<OsString>>,

    // functions
    funcs: ScopedMap<OsString, Rc<FunctionBody>>,

    // FIXME: figure out how to make multi-threaded (this might be small enough to just clone rather
    //        than use an Arc)
    fds: FdArray,

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

            vars: ScopedMap::new(),
            export_vars: ScopedMap::new(),
            funcs: ScopedMap::new(),

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

    pub fn set_export_var(
        &mut self,
        name: Cow<OsStr>,
        new_val: OsString,
    ) -> Option<Option<OsString>> {
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
        self.vars
            .get(name)
            .or_else(|| self.export_vars.get(name).and_then(|var| var.as_ref()))
    }

    pub fn get_var_nonempty<Q: ?Sized>(&self, name: &Q) -> Option<&OsString>
    where
        Q: AsRef<OsStr>,
    {
        self.get_var(name)
            .and_then(|value| if value.is_empty() { None } else { Some(value) })
    }

    pub fn remove_var<Q: ?Sized>(&mut self, name: &Q) -> Option<OsString>
    where
        Q: AsRef<OsStr>,
    {
        let name = name.as_ref();
        self.vars
            .remove(name)
            .or_else(|| self.export_vars.remove(name).and_then(|var| var))
    }

    pub fn set_fd(&mut self, fd: usize, value: EnvFd) {
        self.fds.set_val(fd, value);
    }

    pub fn get_fd(&self, fd: usize) -> &EnvFd {
        &self.fds[fd]
    }

    pub fn fds(&self) -> &FdArray {
        &self.fds
    }

    pub fn current_fds(&mut self) -> ScopedArrayIter<EnvFd> {
        self.fds.iter_mut()
    }

    pub fn set_func<Q: ?Sized>(
        &mut self,
        name: &Q,
        new_val: Rc<FunctionBody>,
    ) -> Option<Rc<FunctionBody>>
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
            inner: self.vars
                .iter()
                .map(|(key, val)| (key.as_ref(), val.as_ref()))
                .chain(self.export_iter()),
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
            }),
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
            vars: ScopedMap::new(),
            export_vars: iter.map(|(key, value)| (key, Some(value))).collect(),
            funcs: ScopedMap::new(),

            fds: Default::default(),

            builtins: BuiltinSet::new(vec![]),

            break_counter: 0,
            break_type: CheckBreak::None,
            loop_depth: 0,
        }
    }
}

impl Scoped for Environment {
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
    fn enter_scope(&mut self) {
        self.fds.enter_scope();
        self.special_vars.enter_scope();
    }

    fn exit_scope(&mut self) {
        self.fds.exit_scope();
        self.special_vars.exit_scope();
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

impl<'a, I: Iterator<Item = (&'a OsStr, &'a OsStr)>> ExactSizeIterator for EnvIter<'a, I> {}
impl<'a, I: Iterator<Item = (&'a OsStr, &'a OsStr)>> FusedIterator for EnvIter<'a, I> {}
