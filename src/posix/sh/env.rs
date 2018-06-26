use fnv::FnvHashMap;

use std::borrow::{Borrow, Cow};
use std::collections::HashMap;
use std::ffi::{OsString, OsStr};
use std::hash::Hash;
use std::iter::{FromIterator, FusedIterator};
use std::mem;
use std::rc::Rc;

use super::UtilSetup;
use super::ast::FunctionBody;
use super::builtin::Builtin;

#[derive(Clone, Debug)]
pub struct Environment<S: UtilSetup> {
    vars: HashMap<OsString, OsString>,
    export_vars: HashMap<OsString, Option<OsString>>,
    funcs: HashMap<OsString, Rc<FunctionBody>>,

    // NOTE: the key could be String, but that would require conversion from user input
    builtins: FnvHashMap<OsString, Rc<Builtin<S>>>,
}

impl<S: UtilSetup> Environment<S> {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
            export_vars: HashMap::new(),
            funcs: HashMap::new(),

            builtins: FnvHashMap::default(),
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

    pub fn add_builtins<I: IntoIterator<Item = (OsString, Builtin<S>)>>(&mut self, builtins: I) {
        for (key, val) in builtins.into_iter() {
            self.builtins.insert(key, Rc::new(val));
        }
    }

    pub fn get_builtin<Q: ?Sized>(&self, name: &Q) -> Option<Rc<Builtin<S>>>
    where
        Q: AsRef<OsStr>,
    {
        self.builtins.get(name.as_ref()).map(|b| b.clone())
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

impl<S: UtilSetup> FromIterator<(OsString, OsString)> for Environment<S> {
    fn from_iter<I: IntoIterator<Item = (OsString, OsString)>>(iter: I) -> Self {
        iter.into_iter().into()
    }
}

impl<S: UtilSetup, T: Iterator<Item = (OsString, OsString)>> From<T> for Environment<S> {
    fn from(iter: T) -> Self {
        Self {
            vars: iter.collect(),
            export_vars: HashMap::new(),
            funcs: HashMap::new(),

            builtins: FnvHashMap::default(),
        }
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
