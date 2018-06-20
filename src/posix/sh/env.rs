use std::borrow::Borrow;
use std::collections::HashMap;
use std::collections::hash_map::Iter as HashIter;
use std::ffi::{OsString, OsStr};
use std::hash::Hash;
use std::iter::{FromIterator, FusedIterator};
use std::mem;

use super::ast::FunctionBody;

#[derive(Clone, Debug)]
pub struct Environment<'a> {
    vars: HashMap<OsString, OsString>,
    funcs: HashMap<OsString, &'a FunctionBody>,
}

impl<'a> Environment<'a> {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
            funcs: HashMap::new(),
        }
    }

    pub fn set_var<Q: ?Sized>(&mut self, name: &Q, new_val: OsString) -> Option<OsString>
    where
        OsString: Borrow<Q>,
        Q: Hash + Eq + Clone + Into<OsString>,
    {
        //self.vars.get_mut(name).map(|value| mem::replace(value, new_val)).or_else(|| self.vars.insert(name.clone().into(), new_val))
        // this is why NLL will be a good thing
        if let Some(value) = self.vars.get_mut(name) {
            return Some(mem::replace(value, new_val));
        }
        self.vars.insert(name.clone().into(), new_val)
    }

    pub fn set_func<Q: ?Sized>(&mut self, name: &Q, new_val: &'a FunctionBody) -> Option<&'a FunctionBody>
    where
        OsString: Borrow<Q>,
        Q: Hash + Eq + Clone + Into<OsString>,
    {
        self.funcs.get_mut(name).map(|value| mem::replace(value, new_val)).or_else(|| self.funcs.insert(name.clone().into(), new_val))
    }

    pub fn get_func<Q: ?Sized>(&self, name: &Q) -> Option<&'a FunctionBody>
    where
        OsString: Borrow<Q>,
        Q: Hash + Eq,
    {
        self.funcs.get(name).map(|v| *v)
    }

    pub fn iter(&self) -> EnvIter {
        EnvIter { inner: self.vars.iter() }
    }
}

impl<'a> FromIterator<(OsString, OsString)> for Environment<'a> {
    fn from_iter<I: IntoIterator<Item = (OsString, OsString)>>(iter: I) -> Self {
        iter.into_iter().into()
    }
}

impl<'a, T: Iterator<Item = (OsString, OsString)>> From<T> for Environment<'a> {
    fn from(iter: T) -> Self {
        Self {
            vars: iter.collect(),
            funcs: HashMap::new(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct EnvIter<'a> {
    inner: HashIter<'a, OsString, OsString>,
}

impl<'a> Iterator for EnvIter<'a> {
    type Item = (&'a OsStr, &'a OsStr);

    fn next(&mut self) -> Option<(&'a OsStr, &'a OsStr)> {
        self.inner.next().map(|(key, value)| (key.as_os_str(), value.as_os_str()))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }
}

impl<'a> ExactSizeIterator for EnvIter<'a> { }
impl<'a> FusedIterator for EnvIter<'a> { }
