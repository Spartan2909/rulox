mod helper;

use core::cell::Ref;
use core::cell::RefCell;
use core::cell::RefMut;
use core::ops::Deref;
use core::ops::DerefMut;
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

impl<'a, T> Deref for ReadGuard<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug)]
pub struct WriteGuard<'a, T>(RefMut<'a, T>);

impl<'a, T> Deref for WriteGuard<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a, T> DerefMut for WriteGuard<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
