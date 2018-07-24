use std::borrow::Borrow;
use std::collections::hash_map::{self, Entry, HashMap, RandomState};
use std::fmt::{self, Debug};
use std::hash::{BuildHasher, Hash};
use std::iter::FromIterator;
use std::mem;

use super::{Locality, Scoped};

/// A map meant to abstract the implementation of variable scoping (e.g. for subshells) so that the
/// underlying implementation can be easily changed.
pub struct ScopedMap<K, V, S = RandomState> {
    // XXX: ideally, we would have our own hashmap implementation that hashes the key once and then
    //      uses the hashed key to check a hashmaps each representing their own scope for the
    //      desired variable (of course checking from the innermost scope first).
    maps: Locality<HashMap<K, Option<V>, S>>,
}

impl<K: Hash + Eq, V> ScopedMap<K, V, RandomState> {
    pub fn new() -> Self {
        ScopedMap::with_hasher(RandomState::new())
    }
}

impl<K: Hash + Eq, V, S: BuildHasher + Default> ScopedMap<K, V, S> {
    pub fn with_hasher(hash_builder: S) -> Self {
        Self {
            maps: Locality::new(HashMap::with_hasher(hash_builder), 0, None),
        }
    }

    pub fn contains_key<Q: ?Sized>(&self, k: &Q) -> bool
    where
        K: Borrow<Q>,
        Q: Hash + Eq,
    {
        self.get(k).is_some()
    }

    pub fn get<Q: ?Sized>(&self, k: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq,
    {
        let mut locality = Some(&self.maps);
        while let Some(scope) = locality {
            if let Some(val) = scope.current_val().get(k) {
                return val.as_ref();
            }
            locality = scope.outer_scope();
        }
        None
    }

    pub fn get_mut<Q: ?Sized>(&mut self, k: &Q) -> Option<&mut V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq,
    {
        self.maps
            .current_val_mut()
            .get_mut(k)
            .and_then(|v| v.as_mut())
    }

    pub fn remove<'a, Q: ?Sized>(&mut self, k: &'a Q) -> Option<V>
    where
        K: Borrow<Q> + From<&'a Q>,
        Q: Hash + Eq,
    {
        // XXX: we always allocate here for now, but implementing our own hashmap will hopefully
        //      allow us to only allocate when absolutely necessary
        match self.maps.current_val_mut().entry(k.into()) {
            Entry::Occupied(mut entry) => mem::replace(entry.get_mut(), None),
            Entry::Vacant(entry) => {
                entry.insert(None);
                None
            }
        }
    }

    pub fn insert(&mut self, key: K, val: V) -> Option<V> {
        if self.maps.count > 0 {
            self.maps.set_val(HashMap::default());
        }
        self.maps
            .current_val_mut()
            .insert(key, Some(val))
            .unwrap_or(None)
    }

    // FIXME: no idea how to do this quickly
    pub fn iter<'a>(&'a self) -> Iter<'a, K, V, S> {
        Iter {
            maps: &self.maps,
            inner_iter: self.maps.current_val().iter(),
            found: vec![],
        }
    }
}

impl<K: Eq + Hash, V, S: BuildHasher + Default> ScopedMap<K, V, S> {
    fn default() -> Self {
        Self {
            maps: Default::default(),
        }
    }
}

impl<K, V, S> Scoped for ScopedMap<K, V, S> {
    fn enter_scope(&mut self) {
        self.maps.enter_scope();
    }

    fn exit_scope(&mut self) {
        self.maps.exit_scope();
    }
}

impl<K: Eq + Hash, V, S: BuildHasher + Default> FromIterator<(K, V)> for ScopedMap<K, V, S> {
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
        let map = iter.into_iter().map(|(k, v)| (k, Some(v))).collect();
        ScopedMap {
            maps: Locality::new(map, 0, None),
        }
    }
}

impl<K: Eq + Hash + Debug, V: Debug, S: BuildHasher> Debug for ScopedMap<K, V, S> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.maps.fmt(f)
    }
}

pub struct Iter<'a, K: Hash + Eq + 'a, V: 'a, S: BuildHasher + 'a> {
    maps: &'a Locality<HashMap<K, Option<V>, S>>,
    inner_iter: hash_map::Iter<'a, K, Option<V>>,

    // FIXME: we ideally shouldn't need to allocate here (it's probably easiest to fix this using
    //        a custom hashmap)
    found: Vec<&'a K>,
}

impl<'a, K: Hash + Eq + 'a, V: 'a, S: BuildHasher + 'a> Iterator for Iter<'a, K, V, S> {
    type Item = (&'a K, &'a V);

    fn next(&mut self) -> Option<Self::Item> {
        while let Some((key, Some(value))) = self.inner_iter.next() {
            if !self.found.contains(&key) {
                self.found.push(key);
                return Some((key, value));
            }
        }
        if let Some(scope) = self.maps.outer_scope() {
            self.maps = scope;
            self.inner_iter = self.maps.current_val().iter();
            self.next()
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner_iter.size_hint()
    }
}
