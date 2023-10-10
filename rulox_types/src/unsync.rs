mod impls;

use crate::LoxError;
use crate::LoxResult;
use crate::LoxValue;

use core::cell::Ref;
use core::cell::RefCell;
use core::cell::RefMut;
use std::cell::UnsafeCell;
use std::rc::Rc;

/// A pointer to `T` with shared ownership.
#[derive(Debug)]
pub struct Shared<T: ?Sized>(Rc<RefCell<T>>);

pub(super) type Inner<T> = RefCell<T>;

impl<T> Shared<T> {
    /// Create a [`Shared<T>`] from a value of type `T`.
    pub fn new(value: T) -> Shared<T> {
        Shared(Rc::new(RefCell::new(value)))
    }
}

impl<T: ?Sized> Shared<T> {
    pub(super) fn into_raw(self) -> *const RefCell<T> {
        Rc::into_raw(self.0)
    }

    /// ## Safety
    /// `ptr` must have come from [`Shared<T>::into_raw`].
    pub(super) unsafe fn from_raw(ptr: *const RefCell<T>) -> Shared<T> {
        // SAFETY: Must be guaranteed by caller.
        Shared(unsafe { Rc::from_raw(ptr) })
    }

    /// Returns a read-only RAII guard for `self`.
    pub fn read(&self) -> ReadGuard<T> {
        ReadGuard(self.0.borrow())
    }

    /// Returns a read-write RAII guard for `self`.
    pub fn write(&self) -> WriteGuard<T> {
        WriteGuard(self.0.borrow_mut())
    }
}

impl<T: ?Sized> Clone for Shared<T> {
    fn clone(&self) -> Self {
        Shared(Rc::clone(&self.0))
    }
}

pub struct ReadGuard<'a, T: ?Sized>(Ref<'a, T>);

pub struct WriteGuard<'a, T: ?Sized>(RefMut<'a, T>);

struct CloneCell<T: Clone>(UnsafeCell<T>);

impl<T: Clone> CloneCell<T> {
    fn new(value: T) -> CloneCell<T> {
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

    /// Gets the value of `self`, returning an error if it is not defined.
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
    pub fn close_over(&self) -> LoxVariable {
        LoxVariable(Rc::clone(&self.0))
    }
}
