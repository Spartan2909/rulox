use crate::functions::LoxArgs;
use crate::hash::hash_ptr;
use crate::LoxError;
use crate::LoxResult;
use crate::Shared;

use std::fmt;
use std::future::Future;
use std::hash::Hash;
use std::hash::Hasher;
use std::pin::pin;
use std::pin::Pin;
use std::ptr;
use std::task::Context;
use std::task::Poll;

use pin_project::pin_project;

#[cfg(feature = "serde")]
use serde::Serialize;

type RawLoxFuture = Pin<Box<dyn Future<Output = LoxResult> + Send + Sync>>;

/// An asynchronous function.
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Coroutine {
    #[cfg_attr(feature = "serde", serde(skip_serializing))]
    fun: Box<dyn Fn(LoxArgs) -> RawLoxFuture + Send + Sync>,
    params: Vec<&'static str>,
}

impl Coroutine {
    #[doc(hidden)]
    pub fn new(
        fun: Box<dyn Fn(LoxArgs) -> RawLoxFuture + Send + Sync>,
        params: Vec<&'static str>,
    ) -> Coroutine {
        Coroutine { fun, params }
    }

    pub(super) fn params(&self) -> &[&'static str] {
        &self.params
    }

    /// Creates a future that executes `self`.
    ///
    /// Note that the future returned by this function will not do anything
    /// unless `await`ed.
    pub fn start(&self, args: LoxArgs) -> LoxFuture {
        LoxFuture(Shared::new(LoxFutureInner {
            handle: (self.fun)(args),
            done: false,
        }))
    }
}

impl fmt::Debug for Coroutine {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Coroutine")
            .field("params", &self.params)
            .finish_non_exhaustive()
    }
}

impl PartialEq for Coroutine {
    fn eq(&self, other: &Self) -> bool {
        let f1: *const _ = self.fun.as_ref();
        let f2: *const _ = other.fun.as_ref();
        ptr::eq(f1.cast::<()>(), f2.cast())
    }
}

impl Hash for Coroutine {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        hash_ptr(self.fun.as_ref(), state);
    }
}

#[cfg_attr(feature = "serde", derive(Serialize))]
#[doc(hidden)]
#[pin_project]
pub struct LoxFutureInner {
    #[cfg_attr(feature = "serde", serde(skip_serializing))]
    #[pin]
    handle: RawLoxFuture,
    done: bool,
}

impl LoxFutureInner {
    pub(super) const fn done(&self) -> bool {
        self.done
    }
}

impl fmt::Debug for LoxFutureInner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("LoxFutureInner")
            .field("done", &self.done)
            .finish_non_exhaustive()
    }
}

impl PartialEq for LoxFutureInner {
    fn eq(&self, other: &Self) -> bool {
        let h1 = &*self.handle.as_ref();
        let h2 = &*other.handle.as_ref();
        ptr::addr_eq(h1, h2)
    }
}

impl Hash for LoxFutureInner {
    fn hash<H: Hasher>(&self, state: &mut H) {
        hash_ptr(&*self.handle.as_ref(), state);
    }
}

impl Future for LoxFutureInner {
    type Output = LoxResult;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        if self.done {
            return Poll::Ready(Err(LoxError::finished_coroutine()));
        }

        let this = self.project();
        let handle = this.handle;
        match Future::poll(handle, cx) {
            Poll::Ready(result) => {
                *this.done = true;
                Poll::Ready(result)
            }
            Poll::Pending => Poll::Pending,
        }
    }
}

/// An asynchronous computation recieived from [`Coroutine::start`].
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct LoxFuture(pub(super) Shared<LoxFutureInner>);

impl Future for LoxFuture {
    type Output = LoxResult;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        Future::poll(Pin::new(&mut *self.0.write()), cx)
    }
}

impl Future for &LoxFuture {
    type Output = LoxResult;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        Future::poll(Pin::new(&mut *self.0.write()), cx)
    }
}
