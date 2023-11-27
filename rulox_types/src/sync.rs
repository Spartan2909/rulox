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
pub type Shared<T> = Arc<RwLock<T>>;

#[cfg_attr(not(feature = "serialise"), allow(dead_code))]
pub type Inner<T> = RwLock<T>;

/// Returns a read-only RAII guard for the shared pointer.
pub fn read<T: ?Sized>(ptr: &Shared<T>) -> ReadGuard<T> {
    ReadGuard(ptr.read().unwrap_or_else(PoisonError::into_inner))
}

/// Returns a read-write RAII guard for the shared pointer.
pub fn write<T: ?Sized>(ptr: &Shared<T>) -> WriteGuard<T> {
    WriteGuard(ptr.write().unwrap_or_else(PoisonError::into_inner))
}

pub struct ReadGuard<'a, T: ?Sized>(RwLockReadGuard<'a, T>);

pub struct WriteGuard<'a, T: ?Sized>(RwLockWriteGuard<'a, T>);

/// A variable defined in Lox code.
pub struct LoxVariable(Shared<LoxValue>);

impl LoxVariable {
    /// Creates a new [`LoxVariable`] wrapping a [`LoxValue`] created from the
    /// argument.
    pub fn new<T: Into<LoxValue>>(value: T) -> LoxVariable {
        LoxVariable(Arc::new(RwLock::new(value.into())))
    }

    /// Gets the value of `self`.
    ///
    /// ## Errors
    /// Returns an error if `self` is not defined.
    pub fn get(&self) -> LoxResult {
        let inner = read(&self.0);
        if let LoxValue::Undefined(name) = *inner {
            Err(LoxError::undefined_variable(name))
        } else {
            Ok(inner.clone())
        }
    }

    #[doc(hidden)] // Not public API.
    pub fn overwrite(&self, value: LoxValue) {
        *write(&self.0) = value;
    }

    #[doc(hidden)] // Not public API.
    #[must_use]
    pub fn close_over(&self) -> LoxVariable {
        LoxVariable(self.0.clone())
    }
}
