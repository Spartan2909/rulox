use crate::interop::LoxObject;
use crate::LoxError;
use crate::LoxResult;
use crate::LoxValue;

use std::any::TypeId;
use std::ops::Deref;
use std::ops::DerefMut;
use std::sync::Arc;
use std::sync::PoisonError;
use std::sync::RwLock;
use std::sync::RwLockReadGuard;
use std::sync::RwLockWriteGuard;

/// A shared, mutable pointer to `T`.
#[derive(Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
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

impl<T: LoxObject> Shared<T> {
    /// Convert `self` to a pointer to the target type.
    pub fn upcast(self) -> Shared<dyn LoxObject> {
        Shared(self.into_inner() as _)
    }
}

impl Shared<dyn LoxObject> {
    /// Attempts to downcast `self` to a concrete type.
    ///
    /// ## Errors
    ///
    /// Returns `Err(self)` if the value wrapped by `self` is not an instance of `T`.
    pub fn downcast<T: LoxObject>(self) -> Result<Shared<T>, Shared<dyn LoxObject>> {
        let t = TypeId::of::<T>();
        let concrete_self_type = (*self.read()).type_id();
        if t == concrete_self_type {
            // SAFETY: The `TypeId`s of `*self.read()` and `T` are the same, so they are the same
            // type.
            Ok(Shared(unsafe {
                Arc::from_raw(Arc::into_raw(self.0).cast())
            }))
        } else {
            Err(self)
        }
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

impl<'a, T: ?Sized> Deref for ReadGuard<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub struct WriteGuard<'a, T: ?Sized>(RwLockWriteGuard<'a, T>);

impl<'a, T: ?Sized> Deref for WriteGuard<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a, T: ?Sized> DerefMut for WriteGuard<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

/// A variable defined in Lox code.
pub struct LoxVariable(Shared<LoxVariableInner>);

enum LoxVariableInner {
    Value(LoxValue),
    Undefined(&'static str),
}

impl LoxVariable {
    /// Creates a new [`LoxVariable`] wrapping a [`LoxValue`] created from the
    /// argument.
    pub fn new<T: Into<LoxValue>>(value: T) -> LoxVariable {
        LoxVariable(Shared::new(LoxVariableInner::Value(value.into())))
    }

    /// Gets the value of `self`.
    ///
    /// ## Errors
    ///
    /// Returns an error if `self` is not defined.
    pub fn get(&self) -> LoxResult {
        match &*self.0.read() {
            LoxVariableInner::Value(ref value) => Ok(value.clone()),
            LoxVariableInner::Undefined(name) => Err(LoxError::undefined_variable(name)),
        }
    }

    #[doc(hidden)] // Not public API.
    pub fn overwrite(&self, value: LoxValue) {
        let mut inner = self.0.write();
        match &mut *inner {
            LoxVariableInner::Value(current) => *current = value,
            LoxVariableInner::Undefined(_) => *inner = LoxVariableInner::Value(value),
        }
    }

    #[doc(hidden)] // Not public API.
    #[must_use]
    pub fn close_over(&self) -> LoxVariable {
        LoxVariable(self.0.clone())
    }

    #[doc(hidden)]
    pub fn undefined(name: &'static str) -> LoxVariable {
        LoxVariable(Shared::new(LoxVariableInner::Undefined(name)))
    }
}
