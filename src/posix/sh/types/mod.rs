use std::mem;

use super::error::CommandError;

pub use self::scoped_map::ScopedMap;
pub use self::scoped_array::{FdArray, ScopedArray};

pub mod scoped_map;
pub mod scoped_array;

pub trait TryClone: Sized {
    fn try_clone<'a>(&'a self) -> Result<Self, CommandError>;
}

/// Indicate that a type is scoped.  In this context, "scoped" means that upon calling
/// `exit_scope()`, any values changed after calling `enter_scope()` will be reset to their
/// original value.  For every call to `exit_scope()`, there must one corresponding call to
/// `enter_scope()` that executes prior to the call to `exit_scope()`.  Both `enter_scope()` and
/// `exit_scope()` can be called several times in a row (which indicates that multiple scopes have
/// been entered/exited).
pub trait Scoped {
    /// Enter a scope, saving values in such a way that they will be restored upon a call to
    /// `exit_scope()`.
    fn enter_scope(&mut self);

    /// Exit a scope, restoring all values saved upon the previous `enter_scope()` call.
    fn exit_scope(&mut self);
}

// FIXME: locality design may be flawed as we need to set global scope (but only within the current
//        subshell) in functions, whereas anything set in subshells needs to remain within that
//        subshell.  not sure if the current setup is good enough (it might be okay if we only
//        enter_scope() when entering a subshell, which i don't believe is what happens right now)
// FIXME: should not be pub
#[derive(Debug)]
pub struct Locality<T> {
    item: T,
    count: usize,
    parent: Option<Box<Locality<T>>>,
}

impl<T> Locality<T> {
    pub fn new(item: T, count: usize, parent: Option<Box<Locality<T>>>) -> Self {
        Self {
            item,
            count,
            parent,
        }
    }

    pub fn set_val(&mut self, value: T) {
        if self.count == 0 {
            self.item = value;
        } else {
            let cur_item = mem::replace(&mut self.item, value);
            let count = mem::replace(&mut self.count, 0);
            let prev_locality = mem::replace(&mut self.parent, None);
            self.parent = Some(Box::new(Locality::new(cur_item, count - 1, prev_locality)));
        }
    }

    pub fn current_val(&self) -> &T {
        &self.item
    }

    pub fn current_val_mut(&mut self) -> &mut T {
        &mut self.item
    }

    pub fn outer_scope(&self) -> Option<&Self> {
        self.parent.as_ref().map(|val| &**val)
    }
}

impl<T> Scoped for Locality<T> {
    fn enter_scope(&mut self) {
        self.count += 1;
    }

    fn exit_scope(&mut self) {
        let parent = if self.count == 0 {
            mem::replace(&mut self.parent, None)
        } else {
            self.count -= 1;
            return;
        };

        // NOTE: this should be fine as exit_scope() should not be called in the base scope
        mem::replace(self, *parent.unwrap());
    }
}

impl<T: Default> Default for Locality<T> {
    fn default() -> Self {
        Locality::new(T::default(), 0, None)
    }
}

impl<T: Clone> Clone for Locality<T> {
    fn clone(&self) -> Self {
        Locality::new(self.item.clone(), self.count, self.parent.clone())
    }
}
