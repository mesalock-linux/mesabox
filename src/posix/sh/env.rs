use std::borrow::{Borrow, Cow};
use std::collections::HashMap;
use std::collections::hash_map::Iter as HashIter;
use std::ffi::{OsString, OsStr};
use std::hash::Hash;
use std::iter::{FromIterator, FusedIterator};
use std::mem;
use std::rc::Rc;

use super::ast::FunctionBody;

#[derive(Clone, Debug)]
pub struct Environment {
    vars: HashMap<OsString, OsString>,
    funcs: HashMap<OsString, Rc<FunctionBody>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
            funcs: HashMap::new(),
        }
    }

    // NOTE: using Cow with OsStr is annoying as From<OsStr> is not implemented apparently
    pub fn set_var(&mut self, name: Cow<OsStr>, new_val: OsString) -> Option<OsString> {
        //self.vars.get_mut(name).map(|value| mem::replace(value, new_val)).or_else(|| self.vars.insert(name.clone().into(), new_val))
        // this is why NLL will be a good thing
        if let Some(value) = self.vars.get_mut::<OsStr>(name.as_ref()) {
            return Some(mem::replace(value, new_val));
        }
        self.vars.insert(name.into_owned(), new_val)
    }

    pub fn get_var<Q: ?Sized>(&self, name: &Q) -> Option<&OsString>
    where
        Q: AsRef<OsStr>,
    {
        self.vars.get(name.as_ref())
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

    pub fn iter(&self) -> EnvIter {
        EnvIter { inner: self.vars.iter() }
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
