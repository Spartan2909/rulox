mod impls;

use crate::LoxError;
use crate::LoxResult;
use crate::LoxValue;

use std::ops::Deref;
use std::sync::Arc;
use std::sync::RwLock;
use std::sync::RwLockReadGuard;
use std::sync::RwLockWriteGuard;

pub type Shared<T> = Arc<RwLock<T>>;

pub(super) type Inner<T> = RwLock<T>;

/// Returns a read-only RAII guard for the shared pointer.
pub fn read<T: ?Sized>(ptr: &Shared<T>) -> ReadGuard<T> {
    ReadGuard(ptr.read().unwrap())
}

/// Returns a read-write RAII guard for the shared pointer.
pub fn write<T: ?Sized>(ptr: &Shared<T>) -> WriteGuard<T> {
    WriteGuard(ptr.write().unwrap())
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

    /// Gets the value of `self`, returning an error if it is not defined.
    pub fn get(&self) -> LoxResult {
        let inner = read(&self.0);
        if let &LoxValue::Undefined(name) = inner.deref() {
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
    pub fn close_over(&self) -> LoxVariable {
        LoxVariable(self.0.clone())
    }
}
