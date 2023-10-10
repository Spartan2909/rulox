mod impls;

use crate::LoxError;
use crate::LoxResult;
use crate::LoxValue;

use core::cell::Ref;
use core::cell::RefCell;
use core::cell::RefMut;
use std::cell::UnsafeCell;
use std::rc::Rc;

pub type Shared<T> = Rc<RefCell<T>>;

pub(super) type Inner<T> = RefCell<T>;

/// Returns a read-only RAII guard for the shared pointer.
pub fn read<T: ?Sized>(ptr: &Shared<T>) -> ReadGuard<T> {
    ReadGuard(ptr.borrow())
}

/// Returns a read-write RAII guard for the shared pointer.
pub fn write<T: ?Sized>(ptr: &Shared<T>) -> WriteGuard<T> {
    WriteGuard(ptr.borrow_mut())
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
