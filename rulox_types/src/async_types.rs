use crate::hash_ptr;
use crate::write;
use crate::LoxArgs;
use crate::LoxError;
use crate::LoxResult;
use crate::LoxValue;

use std::future::Future;
use std::hash::Hash;
use std::ops::Deref;
use std::ops::DerefMut;
use std::pin::Pin;
use std::ptr;
use std::task::Context;
use std::task::Poll;

pub struct Coroutine {
    fun: Box<dyn Fn(LoxArgs) -> Box<dyn Future<Output = LoxResult> + Send + Sync> + Send + Sync>,
    params: Vec<&'static str>,
}

impl Coroutine {
    #[doc(hidden)]
    pub fn new(
        fun: Box<
            dyn Fn(LoxArgs) -> Box<dyn Future<Output = LoxResult> + Send + Sync> + Send + Sync,
        >,
        params: Vec<&'static str>,
    ) -> Coroutine {
        Coroutine { fun, params }
    }

    pub(super) fn params(&self) -> &[&'static str] {
        &self.params
    }

    pub fn start(&self, args: LoxArgs) -> LoxFuture {
        LoxFuture {
            handle: (self.fun)(args),
            done: false,
        }
    }
}

impl PartialEq for Coroutine {
    fn eq(&self, other: &Self) -> bool {
        let f1: *const _ = self.fun.as_ref();
        let f2: *const _ = other.fun.as_ref();
        ptr::eq(f1 as *const (), f2 as *const ())
    }
}

impl Hash for Coroutine {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        hash_ptr(self.fun.as_ref(), state);
    }
}

pub struct LoxFuture {
    handle: Box<dyn Future<Output = LoxResult> + Send + Sync>,
    done: bool,
}

impl LoxFuture {
    pub(super) fn done(&self) -> bool {
        self.done
    }
}

impl PartialEq for LoxFuture {
    fn eq(&self, other: &Self) -> bool {
        let h1: *const _ = self.handle.as_ref();
        let h2: *const _ = other.handle.as_ref();
        ptr::eq(h1 as *const (), h2 as *const ())
    }
}

impl Hash for LoxFuture {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        hash_ptr(self.handle.as_ref(), state);
    }
}

impl Future for LoxFuture {
    type Output = LoxResult;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        if self.done {
            return Poll::Ready(Err(LoxError::finished_coroutine()));
        }

        // SAFETY: `self` is not moved out of.
        let this = unsafe { self.get_unchecked_mut() };
        // SAFETY: `self.handle` will not be moved as `self` is pinned.
        let handle = unsafe { Pin::new_unchecked(this.handle.deref_mut()) };
        match Future::poll(handle, cx) {
            Poll::Ready(result) => {
                this.done = true;
                Poll::Ready(result)
            }
            Poll::Pending => Poll::Pending,
        }
    }
}

impl Future for LoxValue {
    type Output = LoxResult;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        match self.deref() {
            LoxValue::Future(fut) => Future::poll(Pin::new(write(fut).deref_mut()), cx),
            _ => Poll::Ready(Err(LoxError::type_error(format!("Cannot await {self}")))),
        }
    }
}

impl Future for &LoxValue {
    type Output = LoxResult;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        match self.deref() {
            LoxValue::Future(fut) => Future::poll(Pin::new(write(fut).deref_mut()), cx),
            _ => Poll::Ready(Err(LoxError::type_error(format!("Cannot await {self}")))),
        }
    }
}
