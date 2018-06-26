use std::borrow::{Borrow, Cow};
use std::collections::HashMap;
use std::collections::hash_map::Iter as HashIter;
use std::ffi::{OsString, OsStr};
use std::hash::Hash;
use std::iter::{Chain, FromIterator, FusedIterator};
use std::mem;
use std::rc::Rc;

use super::ast::FunctionBody;

#[derive(Clone, Debug)]
pub struct Environment {
    vars: HashMap<OsString, OsString>,
    export_vars: HashMap<OsString, OsString>,
    funcs: HashMap<OsString, Rc<FunctionBody>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
            export_vars: HashMap::new(),
            funcs: HashMap::new(),
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
            return Some(mem::replace(value, new_val));
        }
        self.vars.insert(name.into_owned(), new_val)
    }

    pub fn set_export_var(&mut self, name: Cow<OsStr>, new_val: OsString) -> Option<OsString> {
        if let Some(value) = self.export_vars.get_mut::<OsStr>(name.as_ref()) {
            return Some(mem::replace(value, new_val));
        }
        self.export_vars.insert(name.into_owned(), new_val)
    }

    pub fn get_var<Q: ?Sized>(&self, name: &Q) -> Option<&OsString>
    where
        Q: AsRef<OsStr>,
    {
        let name = name.as_ref();
        // XXX: maybe the opposite order is better, not sure
        self.vars.get(name).or_else(|| self.export_vars.get(name))
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

    pub fn get_func<Q: ?Sized>(&self, name: &Q) -> Option<&Rc<FunctionBody>>
    where
        OsString: Borrow<Q>,
        Q: Hash + Eq,
    {
        self.funcs.get(name)
    }

    pub fn iter(&self) -> EnvIter<impl Iterator<Item = (&OsString, &OsString)>> {
        EnvIter { inner: self.vars.iter().chain(self.export_vars.iter()) }
    }

    pub fn export_iter(&self) -> EnvIter<impl Iterator<Item = (&OsString, &OsString)>> {
        EnvIter { inner: self.export_vars.iter() }
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
        }
    }
}

#[derive(Clone, Debug)]
pub struct EnvIter<'a, I: Iterator<Item = (&'a OsString, &'a OsString)>> {
    inner: I,
}

impl<'a, I: Iterator<Item = (&'a OsString, &'a OsString)>> Iterator for EnvIter<'a, I> {
    type Item = (&'a OsStr, &'a OsStr);

    fn next(&mut self) -> Option<(&'a OsStr, &'a OsStr)> {
        self.inner.next().map(|(key, value)| (key.as_os_str(), value.as_os_str()))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }
}

impl<'a, I: Iterator<Item = (&'a OsString, &'a OsString)>> ExactSizeIterator for EnvIter<'a, I> { }
impl<'a, I: Iterator<Item = (&'a OsString, &'a OsString)>> FusedIterator for EnvIter<'a, I> { }
