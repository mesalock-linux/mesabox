use std::iter::IntoIterator;
use std::ops::{Index, IndexMut};
use std::slice;

use super::{Locality, Scoped};

use posix::sh::env::EnvFd;
use posix::sh::option::FD_COUNT;

pub type FdArray = ScopedArray<[Locality<EnvFd>; FD_COUNT], EnvFd>;

/// An array meant to abstract the implementation of variable (or, more accurately, file
/// descriptor) scoping (e.g. for subshells) so that the underlying implementation can be easily
/// changed.  The current design is (in some ways) similar to
/// [arrayvec](https://crates.io/crates/arrayvec), but less general.
#[derive(Debug, Default)]
pub struct ScopedArray<T: FixedArray<Item = Locality<V>>, V> {
    inner: T,
}

impl<T: FixedArray<Item = Locality<V>>, V> ScopedArray<T, V> {
    pub fn set_val(&mut self, idx: usize, val: V) {
        self.inner.as_mut()[idx].set_val(val);
    }
}

// XXX: do we really want to implement this?  this allows stuff like arr[idx] = val to work, which
//      will end up ignoring Locality::set_val()
impl<T: FixedArray<Item = Locality<V>>, V> ScopedArray<T, V> {
    pub fn iter_mut(&mut self) -> ScopedArrayIter<V> {
        ScopedArrayIter {
            inner: self.inner.iter_mut(),
        }
    }
}

impl<T: FixedArray<Item = Locality<V>>, V> Scoped for ScopedArray<T, V> {
    fn enter_scope(&mut self) {
        for var in self.inner.iter_mut() {
            var.enter_scope();
        }
    }

    fn exit_scope(&mut self) {
        for var in self.inner.iter_mut() {
            var.exit_scope();
        }
    }
}

impl<'a, T: FixedArray<Item = Locality<V>>, V> IntoIterator for &'a mut ScopedArray<T, V> {
    type Item = &'a mut V;
    type IntoIter = ScopedArrayIter<'a, V>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

impl<T: FixedArray<Item = Locality<V>>, V> Index<usize> for ScopedArray<T, V> {
    type Output = V;

    fn index(&self, index: usize) -> &Self::Output {
        self.inner.as_slice()[index].current_val()
    }
}

impl<T: FixedArray<Item = Locality<V>>, V> IndexMut<usize> for ScopedArray<T, V> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        self.inner.as_mut()[index].current_val_mut()
    }
}

pub struct ScopedArrayIter<'a, V: 'a> {
    inner: IterMut<'a, Locality<V>>,
}

impl<'a, V: 'a> Iterator for ScopedArrayIter<'a, V> {
    type Item = &'a mut V;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|v| v.current_val_mut())
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }
}

pub trait FixedArray {
    type Item;

    fn as_slice(&self) -> &[Self::Item];

    // FIXME: should implement AsMut<[Self::Item]> instead of this
    fn as_mut(&mut self) -> &mut [Self::Item];

    fn iter<'a>(&'a self) -> Iter<'a, Self::Item> {
        Iter {
            inner: self.as_slice().iter(),
        }
    }

    fn iter_mut<'a>(&'a mut self) -> IterMut<'a, Self::Item>;
}

pub struct Iter<'a, T: 'a> {
    inner: slice::Iter<'a, T>,
}

impl<'a, T: 'a> Iterator for Iter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }
}

pub struct IterMut<'a, T: 'a> {
    inner: slice::IterMut<'a, T>,
}

impl<'a, T: 'a> Iterator for IterMut<'a, T> {
    type Item = &'a mut T;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }
}

macro_rules! impl_fixed {
    ($elems:expr) => {
        impl<T> FixedArray for [T; $elems] {
            type Item = T;

            fn as_slice(&self) -> &[T] {
                &self[..]
            }

            fn as_mut(&mut self) -> &mut [T] {
                &mut self[..]
            }

            fn iter_mut<'a>(&'a mut self) -> IterMut<'a, Self::Item> {
                IterMut {
                    inner: (self as &mut [T]).as_mut().iter_mut(),
                }
            }
        }
    };
}

impl_fixed!(FD_COUNT);
