mod helper;

use std::ops::Deref;
use std::ops::DerefMut;
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

impl<'a, T> Deref for ReadGuard<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug)]
pub struct WriteGuard<'a, T>(RwLockWriteGuard<'a, T>);

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
