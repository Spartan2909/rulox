mod shared_ptr_impls;

use crate::LoxError;
use crate::LoxResult;
use crate::LoxValue;

use std::sync::Arc;
use std::sync::PoisonError;
use std::sync::RwLock;
use std::sync::RwLockReadGuard;
use std::sync::RwLockWriteGuard;

/// A shared, mutable pointer to `T`.
#[derive(Debug)]
#[cfg_attr(feature = "serialise", derive(serde::Serialize))]
pub struct Shared<T: ?Sized>(pub(crate) Arc<RwLock<T>>);

impl<T> Shared<T> {
    /// Construct a new shared pointer to `value`.
    pub fn new(value: T) -> Shared<T> {
        Shared(Arc::new(RwLock::new(value)))
    }
}

impl<T: ?Sized> Shared<T> {
    /// Returns a read-only RAII guard for the contents of `self`.
    pub fn read(&self) -> ReadGuard<T> {
        ReadGuard(self.0.read().unwrap_or_else(PoisonError::into_inner))
    }

    /// Returns a read-write RAII guard for the contents of `self`.
    pub fn write(&self) -> WriteGuard<T> {
        WriteGuard(self.0.write().unwrap_or_else(PoisonError::into_inner))
    }

    pub(crate) fn as_ptr(&self) -> *const RwLock<T> {
        Arc::as_ptr(&self.0)
    }

    pub(crate) fn into_inner(self) -> Arc<RwLock<T>> {
        self.0
    }
}

impl<T: ?Sized> Clone for Shared<T> {
    fn clone(&self) -> Self {
        Shared(Arc::clone(&self.0))
    }
}

impl<T: ?Sized> From<Arc<RwLock<T>>> for Shared<T> {
    fn from(value: Arc<RwLock<T>>) -> Self {
        Shared(value)
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

    /// Gets the value of `self`.
    ///
    /// ## Errors
    /// Returns an error if `self` is not defined.
    pub fn get(&self) -> LoxResult {
        let inner = self.0.read();
        if let LoxValue::Undefined(name) = *inner {
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
    #[must_use]
    pub fn close_over(&self) -> LoxVariable {
        LoxVariable(self.0.clone())
    }
}
