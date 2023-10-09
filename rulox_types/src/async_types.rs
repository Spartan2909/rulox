use crate::LoxArgs;
use crate::LoxError;
use crate::LoxResult;
use crate::LoxValue;

use std::future::Future;
use std::ops::Deref;
use std::ops::DerefMut;
use std::pin::Pin;
use std::task::Context;
use std::task::Poll;

pub struct Coroutine {
    fun: Box<dyn Fn(LoxArgs) -> Box<dyn Future<Output = LoxResult> + Send + Sync> + Send + Sync>,
    params: Vec<&'static str>,
}

impl Coroutine {
    pub(super) fn new(
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

    pub(super) fn start(&self, args: LoxArgs) -> LoxFuture {
        LoxFuture {
            handle: (self.fun)(args),
            done: false,
        }
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
            LoxValue::Future(fut) => Future::poll(Pin::new(fut.write().deref_mut()), cx),
            _ => Poll::Ready(Err(LoxError::type_error(format!("Cannot await {self}")))),
        }
    }
}

impl Future for &LoxValue {
    type Output = LoxResult;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        match self.deref() {
            LoxValue::Future(fut) => Future::poll(Pin::new(fut.write().deref_mut()), cx),
            _ => Poll::Ready(Err(LoxError::type_error(format!("Cannot await {self}")))),
        }
    }
}
