use crate::error::LoxError;
use crate::hash::hash_ptr;
use crate::private::Sealed;
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
    #[cfg_attr(feature = "serialise", serde(skip_serializing))]
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
            .finish_non_exhaustive()
    }
}

impl PartialEq for LoxFn {
    fn eq(&self, other: &Self) -> bool {
        let ptr1: *const _ = self.fun.as_ref();
        let ptr2: *const _ = other.fun.as_ref();
        ptr::eq(ptr1.cast::<()>(), ptr2.cast::<()>()) && self.params == other.params
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

    #[cfg_attr(not(feature = "async"), allow(clippy::unnecessary_wraps))]
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
            LoxMethod::Async(fun) => Ok(LoxValue::Future(fun.start(args))),
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
        self.head.as_ref().map_or_else(
            || self.main.get(index),
            |head| {
                if index == 0 {
                    Some(head)
                } else {
                    self.main.get(index - 1)
                }
            },
        )
    }

    /// Attempts to extract `self` into the given tuple.
    ///
    /// ## Errors
    /// Errors if the types of the values in `self` do not match those in the
    /// target.
    ///
    /// ## Examples
    /// ```
    /// # use rulox_types::LoxArgs;
    /// # use rulox_types::LoxError;
    /// fn do_some_stuff(name: String, age: usize) {
    ///     // ...
    ///     # assert_eq!(name, "Jane");
    ///     # assert_eq!(age, 45);
    /// }
    ///
    /// # fn main() -> Result<(), LoxError> {
    /// let args: LoxArgs = ["Jane".into(), 45.into()].into();
    /// let (name, age) = args.extract()?;
    /// do_some_stuff(name, age);
    /// # Ok(())
    /// # }
    /// ```
    pub fn extract<T: ConcreteLoxArgs>(self) -> Result<T, LoxError> {
        T::extract_from_args(self)
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

/// A trait for tuples than can be extracted from a [`LoxArgs`].
///
/// See [`LoxArgs::extract`] for more details.
///
/// This trait is sealed, and cannot be implemented for types outside of rulox.
pub trait ConcreteLoxArgs: Sealed + Sized {
    /// Attempts to extract `Self` from the given `args`.
    ///
    /// See [`LoxArgs::extract`] for more details.
    #[allow(clippy::missing_errors_doc)]
    fn extract_from_args(args: LoxArgs) -> Result<Self, LoxError>;
}

macro_rules! count {
    ( $start:ident $( $rest:ident )* ) => {
        1 + count!( $( $rest )* )
    };
    () => {
        0
    };
}

macro_rules! impl_concrete_lox_args {
    ( $( $ty:ident )* ) => {
        impl_concrete_lox_args! { | $( $ty )* }
    };
    ( $( $start:ident )* | $next:ident $( $end:ident )* ) => {
        impl_concrete_lox_args! { @ $( $start )* }
        impl_concrete_lox_args! { $( $start )* $next | $( $end )* }
    };
    ( $( $ty:ident )* | ) => {
        impl_concrete_lox_args! { @ $( $ty )* }
    };
    ( @ $( $ty:ident )* ) => {
        impl<$( $ty: TryFrom<LoxValue>, )*> Sealed for ( $( $ty, )* )
            where $( LoxError: From<<$ty as TryFrom<LoxValue>>::Error> ),* {}

        impl<$( $ty: TryFrom<LoxValue>, )*> ConcreteLoxArgs for ( $( $ty, )* )
            where $( LoxError: From<<$ty as TryFrom<LoxValue>>::Error> ),*
        {
            fn extract_from_args(mut __args: LoxArgs) -> Result<Self, LoxError> {
                const __COUNT: usize = count!( $( $ty )* );
                let __len = __args.main.len();
                let mut __drain = __args.drain();
                Ok(( $(
                    $ty::try_from(
                        __drain
                        .next()
                        .ok_or(LoxError::incorrect_arity(__COUNT, __len))?
                    )?,
                )* ))
            }
        }
    };
}

impl_concrete_lox_args! { T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12 T13 T14 T15 T16 T17 T18 T19 T20 }
