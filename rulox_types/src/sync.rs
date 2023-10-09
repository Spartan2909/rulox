mod impls;

use crate::LoxError;
use crate::LoxResult;
use crate::LoxValue;

use std::ops::Deref;
use std::sync::Arc;
use std::sync::RwLock;
use std::sync::RwLockReadGuard;
use std::sync::RwLockWriteGuard;

#[derive(Debug)]
pub struct Shared<T>(Arc<RwLock<T>>);

impl<T> Shared<T> {
    pub fn new(value: T) -> Shared<T> {
        Shared(Arc::new(RwLock::new(value)))
    }

    pub fn read(&self) -> ReadGuard<T> {
        ReadGuard(self.0.read().unwrap())
    }

    pub fn write(&self) -> WriteGuard<T> {
        WriteGuard(self.0.write().unwrap())
    }
}

impl<T> Clone for Shared<T> {
    fn clone(&self) -> Self {
        Shared(Arc::clone(&self.0))
    }
}

#[derive(Debug)]
pub struct ReadGuard<'a, T>(RwLockReadGuard<'a, T>);

#[derive(Debug)]
pub struct WriteGuard<'a, T>(RwLockWriteGuard<'a, T>);

/// A variable defined in Lox code.
/// 
/// There is no public way to create a [`LoxVariable`].
pub struct LoxVariable(Shared<LoxValue>);

impl LoxVariable {
    #[doc(hidden)]
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
