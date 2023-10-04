mod impls;

use core::cell::Ref;
use core::cell::RefCell;
use core::cell::RefMut;
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
