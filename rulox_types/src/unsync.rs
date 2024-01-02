mod shared_ptr_impls;

use crate::LoxError;
use crate::LoxResult;
use crate::LoxValue;

use core::cell::Ref;
use core::cell::RefCell;
use core::cell::RefMut;
use std::cell::UnsafeCell;
use std::rc::Rc;

/// A shared, mutable pointer to `T`.
#[derive(Debug)]
#[cfg_attr(feature = "serialise", derive(serde::Serialize))]
pub struct Shared<T: ?Sized>(pub(crate) Rc<RefCell<T>>);

impl<T> Shared<T> {
    /// Construct a new shared pointer to `value`.
    pub fn new(value: T) -> Shared<T> {
        Shared(Rc::new(RefCell::new(value)))
    }
}

impl<T: ?Sized> Shared<T> {
    /// Returns a read-only RAII guard for the contents of `self`.
    pub fn read(&self) -> ReadGuard<T> {
        ReadGuard(self.0.borrow())
    }

    /// Returns a read-write RAII guard for the contents of `self`.
    pub fn write(&self) -> WriteGuard<T> {
        WriteGuard(self.0.borrow_mut())
    }

    pub(crate) fn as_ptr(&self) -> *const RefCell<T> {
        Rc::as_ptr(&self.0)
    }

    pub(crate) fn into_inner(self) -> Rc<RefCell<T>> {
        self.0
    }
}

impl<T: ?Sized> Clone for Shared<T> {
    fn clone(&self) -> Self {
        Shared(Rc::clone(&self.0))
    }
}

impl<T: ?Sized> From<Rc<RefCell<T>>> for Shared<T> {
    fn from(value: Rc<RefCell<T>>) -> Self {
        Shared(value)
    }
}

pub struct ReadGuard<'a, T: ?Sized>(Ref<'a, T>);

pub struct WriteGuard<'a, T: ?Sized>(RefMut<'a, T>);

struct CloneCell<T: Clone>(UnsafeCell<T>);

impl<T: Clone> CloneCell<T> {
    const fn new(value: T) -> CloneCell<T> {
        CloneCell(UnsafeCell::new(value))
    }

    fn set(&self, value: T) {
        // SAFETY: There are no pther references to `self.0`.
        let inner = unsafe { &mut *self.0.get() };
        *inner = value;
    }

    fn get(&self) -> T {
        // SAFETY: There are no other references to `self.0`.
        let inner = unsafe { &*self.0.get() };
        inner.clone()
    }
}

/// A variable defined in Lox code.
pub struct LoxVariable(Rc<CloneCell<LoxValue>>);

impl LoxVariable {
    /// Creates a new [`LoxVariable`] wrapping a [`LoxValue`] created from the
    /// argument.
    pub fn new<T: Into<LoxValue>>(value: T) -> LoxVariable {
        LoxVariable(Rc::new(CloneCell::new(value.into())))
    }

    /// Gets the value of `self`.
    ///
    /// ## Errors
    /// Returns an error if `self` is not defined.
    pub fn get(&self) -> LoxResult {
        let inner = self.0.get();
        if let LoxValue::Undefined(name) = inner {
            Err(LoxError::undefined_variable(name))
        } else {
            Ok(inner)
        }
    }

    #[doc(hidden)] // Not public API.
    pub fn overwrite(&self, value: LoxValue) {
        self.0.set(value);
    }

    #[doc(hidden)] // Not public API.
    #[must_use]
    pub fn close_over(&self) -> LoxVariable {
        LoxVariable(Rc::clone(&self.0))
    }
}
