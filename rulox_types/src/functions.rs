use crate::error::LoxError;
use crate::hash::hash_ptr;
use crate::LoxRc;
use crate::LoxResult;
use crate::LoxValue;

#[cfg(feature = "async")]
use crate::async_types;

use std::fmt;
use std::fmt::Debug;
use std::hash::Hash;
use std::hash::Hasher;
use std::ptr;

#[cfg(feature = "serialise")]
use serde::Serialize;

/// A function defined in Lox code.
#[cfg_attr(feature = "serialise", derive(Serialize))]
pub struct LoxFn {
    #[cfg(feature = "sync")]
    #[cfg_attr(feature = "serialise", serde(skip_serializing))]
    fun: Box<dyn Fn(LoxArgs) -> LoxResult + Send + Sync>,
    #[cfg(not(feature = "sync"))]
    fun: Box<dyn Fn(LoxArgs) -> LoxResult>,
    params: Vec<&'static str>,
}

impl LoxFn {
    /// Creates a new [`LoxFn`] with the given body and parameter names.
    #[cfg(not(feature = "sync"))]
    pub fn new<F: Fn(LoxArgs) -> LoxResult + 'static>(fun: F, params: Vec<&'static str>) -> Self {
        Self {
            fun: Box::new(fun),
            params,
        }
    }

    /// Creates a new [`LoxFn`] with the given body and parameter names.
    #[cfg(feature = "sync")]
    pub fn new<F: Fn(LoxArgs) -> LoxResult + Send + Sync + 'static>(
        fun: F,
        params: Vec<&'static str>,
    ) -> Self {
        Self {
            fun: Box::new(fun),
            params,
        }
    }

    pub(super) fn call(&self, args: LoxArgs) -> LoxResult {
        (self.fun)(args.check_arity(self.params.len())?)
    }

    pub(super) fn params(&self) -> &[&'static str] {
        &self.params
    }
}

impl Debug for LoxFn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("LoxFn")
            .field("params", &self.params)
            .finish()
    }
}

impl PartialEq for LoxFn {
    fn eq(&self, other: &Self) -> bool {
        let ptr1: *const _ = self.fun.as_ref();
        let ptr2: *const _ = other.fun.as_ref();
        ptr::eq(ptr1 as *const (), ptr2 as *const ()) && self.params == other.params
    }
}

impl Hash for LoxFn {
    fn hash<H: Hasher>(&self, state: &mut H) {
        hash_ptr(self.fun.as_ref(), state);
    }
}

#[derive(Clone, PartialEq, Hash)]
#[cfg_attr(feature = "serialise", derive(Serialize))]
#[doc(hidden)] // Not public API.
pub enum LoxMethod {
    Sync(LoxRc<LoxFn>),
    #[cfg(feature = "async")]
    Async(LoxRc<async_types::Coroutine>),
}

impl LoxMethod {
    pub(super) fn params(&self) -> &[&'static str] {
        match self {
            LoxMethod::Sync(fun) => &fun.params,
            #[cfg(feature = "async")]
            LoxMethod::Async(fun) => fun.params(),
        }
    }

    pub(super) fn get_sync(self) -> Option<LoxRc<LoxFn>> {
        match self {
            LoxMethod::Sync(fun) => Some(fun),
            #[cfg(feature = "async")]
            LoxMethod::Async(_) => None,
        }
    }

    pub(super) fn call(&self, args: LoxArgs) -> LoxResult {
        let args = args.check_arity(self.params().len() - 1)?;
        match self {
            LoxMethod::Sync(fun) => (fun.fun)(args),
            #[cfg(feature = "async")]
            LoxMethod::Async(fun) => Ok(LoxValue::Future(LoxRc::new(fun.start(args).into()))),
        }
    }
}

#[doc(hidden)]
impl Debug for LoxMethod {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LoxMethod::Sync(fun) => write!(f, "Sync({:#?})", fun.params),
            #[cfg(feature = "async")]
            LoxMethod::Async(fun) => write!(f, "Async({:#?})", fun.params()),
        }
    }
}

#[doc(hidden)] // Not public API.
impl From<LoxFn> for LoxMethod {
    fn from(value: LoxFn) -> Self {
        LoxMethod::Sync(LoxRc::new(value))
    }
}

#[doc(hidden)] // Not public API.
impl From<LoxRc<LoxFn>> for LoxMethod {
    fn from(value: LoxRc<LoxFn>) -> Self {
        LoxMethod::Sync(value)
    }
}

#[doc(hidden)] // Not public API.
#[cfg(feature = "async")]
impl From<async_types::Coroutine> for LoxMethod {
    fn from(value: async_types::Coroutine) -> Self {
        LoxMethod::Async(LoxRc::new(value))
    }
}

#[doc(hidden)] // Not public API.
#[cfg(feature = "async")]
impl From<LoxRc<async_types::Coroutine>> for LoxMethod {
    fn from(value: LoxRc<async_types::Coroutine>) -> Self {
        LoxMethod::Async(value)
    }
}

/// Arguments to a Lox function.
#[cfg_attr(feature = "serialise", derive(Serialize))]
pub struct LoxArgs {
    pub(crate) head: Option<LoxValue>,
    pub(crate) main: Vec<LoxValue>,
}

impl LoxArgs {
    /// Creates a new set of arguments from a `Vec<LoxValue>`.
    ///
    /// See also the various [`From`] implementations.
    pub fn new(values: Vec<LoxValue>) -> LoxArgs {
        LoxArgs {
            head: None,
            main: values,
        }
    }

    /// Returns an iterator that moves values out of `self`.
    pub fn drain(&mut self) -> impl Iterator<Item = LoxValue> + '_ {
        self.head.take().into_iter().chain(self.main.drain(..))
    }

    /// Returns the number of items in `self`.
    pub fn len(&self) -> usize {
        if self.head.is_some() {
            self.main.len() + 1
        } else {
            self.main.len()
        }
    }

    /// Returns `true` if `self.len()` equals 0, and false, otherwise
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Gets the `index`th element of `self`, if it exists.
    pub fn get(&self, index: usize) -> Option<&LoxValue> {
        if let Some(head) = &self.head {
            if index == 0 {
                Some(head)
            } else {
                self.main.get(index - 1)
            }
        } else {
            self.main.get(index)
        }
    }

    pub(crate) fn check_arity(self, arity: usize) -> Result<LoxArgs, LoxError> {
        if self.main.len() == arity {
            Ok(self)
        } else {
            Err(LoxError::incorrect_arity(arity, self.main.len()))
        }
    }

    pub(super) fn with_head(mut self, value: LoxValue) -> LoxArgs {
        self.head = Some(value);
        self
    }
}

impl From<Vec<LoxValue>> for LoxArgs {
    fn from(value: Vec<LoxValue>) -> Self {
        LoxArgs::new(value)
    }
}

impl<const N: usize> From<[LoxValue; N]> for LoxArgs {
    fn from(value: [LoxValue; N]) -> Self {
        value.to_vec().into()
    }
}
