mod impls;

use crate::LoxValue;

use core::cell::Ref;
use core::cell::RefCell;
use core::cell::RefMut;
use std::cell::UnsafeCell;
use std::rc::Rc;

#[derive(Debug)]
pub struct Shared<T>(Rc<RefCell<T>>);

impl<T> Shared<T> {
    pub fn new(value: T) -> Shared<T> {
        Shared(Rc::new(RefCell::new(value)))
    }

    pub fn read(&self) -> ReadGuard<T> {
        ReadGuard(self.0.borrow())
    }

    pub fn write(&self) -> WriteGuard<T> {
        WriteGuard(self.0.borrow_mut())
    }
}

impl<T> Clone for Shared<T> {
    fn clone(&self) -> Self {
        Shared(Rc::clone(&self.0))
    }
}

#[derive(Debug)]
pub struct ReadGuard<'a, T>(Ref<'a, T>);

#[derive(Debug)]
pub struct WriteGuard<'a, T>(RefMut<'a, T>);

struct CloneCell<T: Clone>(UnsafeCell<T>);

impl<T: Clone> CloneCell<T> {
    fn new(value: T) -> CloneCell<T> {
        CloneCell(UnsafeCell::new(value))
    }

    fn set(&self, value: T) {
        let inner = unsafe { &mut *self.0.get() };
        *inner = value;
    }

    fn get(&self) -> T {
        let inner = unsafe { &*self.0.get() };
        inner.clone()
    }
}

pub struct LoxVariable(Rc<CloneCell<LoxValue>>);

impl LoxVariable {
    pub fn new(value: LoxValue) -> LoxVariable {
        LoxVariable(Rc::new(CloneCell::new(value)))
    }

    pub fn get(&self) -> LoxValue {
        self.0.get()
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
