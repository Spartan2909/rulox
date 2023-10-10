mod impls;

use crate::LoxError;
use crate::LoxResult;
use crate::LoxValue;

use std::ops::Deref;
use std::sync::Arc;
use std::sync::RwLock;
use std::sync::RwLockReadGuard;
use std::sync::RwLockWriteGuard;

/// A pointer to `T` with shared ownership.
#[derive(Debug)]
pub struct Shared<T: ?Sized>(Arc<RwLock<T>>);

pub(super) type Inner<T> = RwLock<T>;

impl<T> Shared<T> {
    /// Create a [`Shared<T>`] from a value of type `T`.
    pub fn new(value: T) -> Shared<T> {
        Shared(Arc::new(RwLock::new(value)))
    }
}

impl<T: ?Sized> Shared<T> {
    pub(super) fn into_raw(self) -> *const RwLock<T> {
        Arc::into_raw(self.0)
    }

    /// ## Safety
    /// `ptr` must have come from [`Shared<T>::into_raw`].
    pub(super) unsafe fn from_raw(ptr: *const RwLock<T>) -> Shared<T> {
        // SAFETY: Must be guaranteed by caller.
        Shared(unsafe { Arc::from_raw(ptr) })
    }

    /// Returns a read-only RAII guard for `self`.
    pub fn read(&self) -> ReadGuard<T> {
        ReadGuard(self.0.read().unwrap())
    }

    /// Returns a read-write RAII guard for `self`.
    pub fn write(&self) -> WriteGuard<T> {
        WriteGuard(self.0.write().unwrap())
    }
}

impl<T: ?Sized> Clone for Shared<T> {
    fn clone(&self) -> Self {
        Shared(Arc::clone(&self.0))
    }
}

pub struct ReadGuard<'a, T: ?Sized>(RwLockReadGuard<'a, T>);

pub struct WriteGuard<'a, T: ?Sized>(RwLockWriteGuard<'a, T>);

/// A variable defined in Lox code.
pub struct LoxVariable(Shared<LoxValue>);

impl LoxVariable {
    /// Creates a new [`LoxVariable`] wrapping a [`LoxValue`] created from the
    /// argument.
    pub fn new<T: Into<LoxValue>>(value: T) -> LoxVariable {
        LoxVariable(Shared::new(value.into()))
    }

    /// Gets the value of `self`, returning an error if it is not defined.
    pub fn get(&self) -> LoxResult {
        let inner = self.0.read();
        if let &LoxValue::Undefined(name) = inner.deref() {
            Err(LoxError::undefined_variable(name))
        } else {
            Ok(inner.clone())
        }
    }

    #[doc(hidden)] // Not public API.
    pub fn overwrite(&self, value: LoxValue) {
        *self.0.write() = value;
    }

    #[doc(hidden)] // Not public API.
    pub fn close_over(&self) -> LoxVariable {
        LoxVariable(self.0.clone())
    }
}
