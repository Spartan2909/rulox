//! `rulox_types` is a collection of types used by the `rulox` crate to
//! represent dynamically typed values.

#![warn(missing_docs)]

#[cfg(feature = "async")]
#[doc(hidden)]
pub mod async_types;
#[cfg(feature = "async")]
pub use async_types::Coroutine;
#[cfg(feature = "async")]
pub use async_types::LoxFuture;

#[cfg(feature = "serialise")]
mod serialise;

#[cfg_attr(feature = "sync", path = "sync.rs")]
#[cfg_attr(not(feature = "sync"), path = "unsync.rs")]
mod shared;
pub use shared::read;
pub use shared::write;
pub use shared::LoxVariable;
#[doc(hidden)]
pub use shared::Shared;

mod to_tokens;

mod primitive_methods {
    use crate::LoxResult;
    use crate::LoxValue;

    pub(super) fn is_bool(value: LoxValue) -> LoxResult {
        Ok(LoxValue::Bool(matches!(value, LoxValue::Bool(_))))
    }

    pub(super) fn is_str(value: LoxValue) -> LoxResult {
        Ok(LoxValue::Bool(matches!(value, LoxValue::Str(_))))
    }

    pub(super) fn is_num(value: LoxValue) -> LoxResult {
        Ok(LoxValue::Bool(matches!(value, LoxValue::Num(_))))
    }

    pub(super) fn is_arr(value: LoxValue) -> LoxResult {
        Ok(LoxValue::Bool(matches!(value, LoxValue::Arr(_))))
    }

    #[cfg(feature = "async")]
    pub(super) fn is_function(value: LoxValue) -> LoxResult {
        Ok(LoxValue::Bool(matches!(
            value,
            LoxValue::BoundMethod(_, _) | LoxValue::Coroutine(_) | LoxValue::PrimitiveMethod(_, _)
        )))
    }

    #[cfg(not(feature = "async"))]
    pub(super) fn is_function(value: LoxValue) -> LoxResult {
        Ok(LoxValue::Bool(matches!(
            value,
            LoxValue::BoundMethod(_, _) | LoxValue::PrimitiveMethod(_, _)
        )))
    }

    pub(super) fn is_class(value: LoxValue) -> LoxResult {
        Ok(LoxValue::Bool(matches!(value, LoxValue::Class(_))))
    }

    pub(super) fn is_map(value: LoxValue) -> LoxResult {
        Ok(LoxValue::Bool(matches!(value, LoxValue::Map(_))))
    }

    pub(super) fn is_bytes(value: LoxValue) -> LoxResult {
        Ok(LoxValue::Bool(matches!(value, LoxValue::Bytes(_))))
    }

    pub(super) fn is_error(value: LoxValue) -> LoxResult {
        Ok(LoxValue::Bool(matches!(value, LoxValue::Error(_))))
    }

    pub(super) fn is_nil(value: LoxValue) -> LoxResult {
        Ok(LoxValue::Bool(matches!(value, LoxValue::Nil)))
    }
}

#[cfg(not(feature = "sync"))]
#[doc(hidden)]
pub use std::rc::Rc as LoxRc;
#[cfg(feature = "sync")]
#[doc(hidden)]
pub use std::sync::Arc as LoxRc;

use std::any::Any;
use std::any::TypeId;
use std::cmp;
use std::collections::HashMap;
use std::collections::VecDeque;
use std::convert::Infallible;
use std::error::Error;
use std::fmt;
use std::fmt::Debug;
use std::hash::Hash;
use std::mem;
use std::ops;
use std::ops::Deref;
use std::process::ExitCode;
use std::process::Termination;
use std::ptr;
use std::sync::OnceLock;
use std::vec;

#[cfg(feature = "async")]
use std::future::Future;

use bytes::Bytes;

use castaway::cast;

#[cfg(feature = "serialise")]
use serde::Serialize;

#[derive(Debug, Clone)]
#[cfg(feature = "sync")]
struct ExternalError(LoxRc<dyn Error + Send + Sync>);

#[derive(Debug, Clone)]
#[cfg(not(feature = "sync"))]
struct ExternalError(LoxRc<dyn Error>);

impl Hash for ExternalError {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        LoxRc::as_ptr(&self.0).hash(state);
    }
}

impl PartialEq for ExternalError {
    fn eq(&self, other: &Self) -> bool {
        LoxRc::ptr_eq(&self.0, &other.0)
    }
}

/// An error raised during compilation or execution.
#[derive(Debug, Clone, PartialEq, Hash)]
#[cfg_attr(feature = "serialise", derive(Serialize))]
pub struct LoxError {
    inner: LoxErrorInner,
    trace: VecDeque<&'static str>,
}

impl LoxError {
    fn type_error(message: String) -> LoxError {
        LoxError {
            inner: LoxErrorInner::TypeError(message),
            trace: VecDeque::new(),
        }
    }

    fn undefined_variable(kind: &'static str) -> LoxError {
        LoxError {
            inner: LoxErrorInner::UndefinedVariable(kind),
            trace: VecDeque::new(),
        }
    }

    fn invalid_property(property: &'static str, object: String) -> LoxError {
        LoxError {
            inner: LoxErrorInner::InvalidProperty { property, object },
            trace: VecDeque::new(),
        }
    }

    fn non_existent_super(name: &'static str) -> LoxError {
        LoxError {
            inner: LoxErrorInner::NonExistentSuper(name),
            trace: VecDeque::new(),
        }
    }

    fn index_out_of_range(index: usize) -> LoxError {
        LoxError {
            inner: LoxErrorInner::IndexOutOfRange(index),
            trace: VecDeque::new(),
        }
    }

    /// Returns an error corresponding to an invalid key in a map or array.
    pub fn invalid_key(key: LoxValue) -> LoxError {
        LoxError {
            inner: LoxErrorInner::InvalidKey(Box::new(key)),
            trace: VecDeque::new(),
        }
    }

    fn incorrect_arity(expected: usize, found: usize) -> LoxError {
        LoxError {
            inner: LoxErrorInner::IncorrectArity { expected, found },
            trace: VecDeque::new(),
        }
    }

    fn value(value: LoxValue) -> LoxError {
        LoxError {
            inner: LoxErrorInner::Value(Box::new(value)),
            trace: VecDeque::new(),
        }
    }

    #[cfg(feature = "async")]
    fn finished_coroutine() -> LoxError {
        LoxError {
            inner: LoxErrorInner::FinishedCoroutine,
            trace: VecDeque::new(),
        }
    }

    /// Pushes a new function to this error's stack trace.
    #[cold]
    pub fn push_trace(&mut self, value: &'static str) {
        self.trace.push_front(value);
    }

    #[doc(hidden)]
    pub fn push_trace_front(&mut self, value: &'static str) {
        self.trace.push_front(value);
    }

    #[doc(hidden)] // Not public API.
    pub fn with_trace(mut self, trace: Vec<&'static str>) -> LoxError {
        self.trace = trace.into();
        self
    }

    #[doc(hidden)] // Not public API.
    pub fn into_value(self) -> LoxValue {
        if let LoxErrorInner::Value(value) = self.inner {
            *value
        } else {
            LoxValue::Error(self)
        }
    }

    /// Creates a [`LoxError`] from another error.
    #[cfg(feature = "sync")]
    pub fn external<E: Error + Send + Sync + 'static>(value: E) -> LoxError {
        LoxError {
            inner: LoxErrorInner::External(ExternalError(LoxRc::new(value))),
            trace: VecDeque::new(),
        }
    }

    /// Creates a [`LoxError`] from another error.
    #[cfg(not(feature = "sync"))]
    pub fn external<E: Error + 'static>(value: E) -> LoxError {
        LoxError {
            inner: LoxErrorInner::External(ExternalError(LoxRc::new(value))),
            trace: VecDeque::new(),
        }
    }
}

impl From<Infallible> for LoxError {
    fn from(value: Infallible) -> Self {
        match value {}
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
#[cfg_attr(feature = "serialise", derive(Serialize))]
enum LoxErrorInner {
    /// An error that occurs when attempting to use a LoxValue with an invalid type.
    TypeError(String),
    /// An error that occurs when attempting to convert a LoxValue into a Rust type that is too small.
    SizeError {
        found: usize,
    },
    /// An error that occurs when variables are accessed without first being defined.
    UndefinedVariable(&'static str),
    InvalidProperty {
        property: &'static str,
        object: String,
    },
    NonExistentSuper(&'static str),
    IndexOutOfRange(usize),
    InvalidKey(Box<LoxValue>),
    IncorrectArity {
        expected: usize,
        found: usize,
    },
    Value(Box<LoxValue>),
    #[cfg_attr(
        feature = "serialise",
        serde(serialize_with = "serialise::external_error")
    )]
    External(ExternalError),
    #[cfg(feature = "serialise")]
    #[doc(hidden)]
    Arbitrary(String),
    #[cfg(feature = "async")]
    FinishedCoroutine,
}

impl fmt::Display for LoxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "traceback (most recent call last):")?;
        for &fun in self.trace.iter() {
            writeln!(f, "in '{fun}'")?;
        }
        writeln!(f)?;

        match &self.inner {
            LoxErrorInner::TypeError(message) => {
                write!(f, "{}", message)
            }
            LoxErrorInner::SizeError { found } => {
                write!(
                    f,
                    "could not convert LoxValue to value of size {:?} (LoxValue too large)",
                    found
                )
            }
            LoxErrorInner::UndefinedVariable(name) => write!(f, "undefined variable '{name}'"),
            LoxErrorInner::InvalidProperty { property, object } => {
                write!(f, "invalid property '{property}' on {object}")
            }
            LoxErrorInner::NonExistentSuper(name) => {
                write!(f, "function '{name}' has no super function")
            }
            LoxErrorInner::IndexOutOfRange(index) => write!(f, "index out of range: {index}"),
            LoxErrorInner::InvalidKey(key) => write!(f, "invalid key: {key}"),
            LoxErrorInner::IncorrectArity { expected, found } => {
                write!(f, "expected {expected} arguments, found {found}")
            }
            LoxErrorInner::Value(value) => {
                write!(f, "error: {value}")
            }
            LoxErrorInner::External(err) => fmt::Display::fmt(&err.0, f),
            #[cfg(feature = "serialise")]
            LoxErrorInner::Arbitrary(string) => f.write_str(string),
            #[cfg(feature = "async")]
            LoxErrorInner::FinishedCoroutine => write!(f, "cannot await a finished coroutine"),
        }
    }
}

impl Error for LoxError {}

#[doc(hidden)] // Not public API.
#[macro_export]
macro_rules! extract {
    ($expr:expr, $var_name:ident) => {
        match $expr {
            Ok(expr) => expr,
            Err(mut err) => {
                err.push_trace_front($var_name);
                return Err(err);
            }
        }
    };
}

/// An enum used for error reporting.
#[derive(Debug, Clone)]
enum LoxValueType {
    Bool,
    Str,
    Num,
    Arr,
    Function(Vec<&'static str>),
    Class,
    Instance(&'static str),
    Map,
    Bytes,
    Error,
    #[cfg(feature = "async")]
    Coroutine(Vec<&'static str>),
    #[cfg(feature = "async")]
    Future(bool),
    External,
    Nil,
}

impl fmt::Display for LoxValueType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Bool => write!(f, "bool"),
            Self::Str => write!(f, "string"),
            Self::Num => write!(f, "number"),
            Self::Arr => write!(f, "array"),
            Self::Function(params) => write!(f, "function({:#?})", params),
            Self::Class => write!(f, "class"),
            Self::Instance(class) => write!(f, "instance of {class}"),
            Self::Map => write!(f, "map"),
            Self::Bytes => write!(f, "bytes"),
            Self::Error => f.write_str("error"),
            #[cfg(feature = "async")]
            Self::Coroutine(params) => write!(f, "async function({:#?})", params),
            #[cfg(feature = "async")]
            Self::Future(done) => write!(f, "{}future", if *done { "completed " } else { "" }),
            Self::External => write!(f, "external object"),
            Self::Nil => write!(f, "nil"),
        }
    }
}

macro_rules! loxvalue_to_loxvaluetype {
    ( $($t:ty),* ) => { $(
        impl From<$t> for LoxValueType {
            fn from(value: $t) -> Self {
                match value {
                    LoxValue::Bool(_) => Self::Bool,
                    LoxValue::Str(_) => Self::Str,
                    LoxValue::Num(_) => Self::Num,
                    LoxValue::Arr(_) => Self::Arr,
                    LoxValue::Function(f) => Self::Function(f.params.to_vec()),
                    LoxValue::BoundMethod(f, _) => Self::Function(f.params().to_vec()),
                    LoxValue::PrimitiveMethod(_, _) => Self::Function(vec![]),
                    LoxValue::Class(_) => Self::Class,
                    LoxValue::Instance(instance) => Self::Instance(read(&instance).class.name.clone()),
                    LoxValue::Map(_) => Self::Map,
                    LoxValue::Bytes(_) => Self::Bytes,
                    LoxValue::Error(_) => Self::Error,
                    #[cfg(feature = "async")]
                    LoxValue::Coroutine(f) => Self::Coroutine(f.params().to_vec()),
                    #[cfg(feature = "async")]
                    LoxValue::Future(fut) => Self::Future(read(&fut).done()),
                    LoxValue::External(_) => Self::External,
                    LoxValue::Nil => Self::Nil,
                    LoxValue::Undefined(_) => unreachable!(),
                }
            }
        }
    )* };
}

loxvalue_to_loxvaluetype! { LoxValue, &LoxValue, &mut LoxValue }

/// A result returned from most Lox operations.
pub type LoxResult = Result<LoxValue, LoxError>;

/// An entry in a hashmap.
#[derive(Clone, PartialEq, PartialOrd, Hash)]
#[cfg_attr(feature = "serialise", derive(Serialize))]
#[repr(transparent)]
pub struct Entry(LoxValue);

impl Entry {
    fn verify_key(key: LoxValue) -> Result<Entry, LoxError> {
        match key {
            LoxValue::Num(n) if n.is_nan() => Err(LoxError::invalid_key(key)),
            LoxValue::Bool(_) | LoxValue::Num(_) | LoxValue::Str(_) => Ok(Entry(key)),
            _ => Err(LoxError::invalid_key(key)),
        }
    }

    /// Extracts the [`LoxValue`] from `self`.
    pub fn into_inner(self) -> LoxValue {
        self.0
    }
}

impl TryFrom<LoxValue> for Entry {
    type Error = LoxError;

    fn try_from(value: LoxValue) -> Result<Self, Self::Error> {
        Entry::verify_key(value)
    }
}

impl From<Entry> for LoxValue {
    fn from(value: Entry) -> Self {
        value.into_inner()
    }
}

impl Debug for Entry {
    #[inline(always)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(&self.0, f)
    }
}

impl Eq for Entry {}

/// A dynamically typed value used by Lox programs.
#[non_exhaustive]
#[derive(Clone)]
#[cfg_attr(feature = "serialise", derive(Serialize))]
pub enum LoxValue {
    /// A boolean value.
    Bool(bool),
    /// A string.
    Str(LoxRc<String>),
    /// A floating-point number.
    Num(f64),
    /// An array of [`LoxValue`]s.
    Arr(Shared<Vec<Self>>),
    /// A function.
    Function(LoxRc<LoxFn>),
    #[doc(hidden)]
    BoundMethod(LoxMethod, Shared<LoxInstance>),
    #[doc(hidden)]
    #[cfg_attr(
        feature = "serialise",
        serde(serialize_with = "serialise::primitive_method")
    )]
    PrimitiveMethod(fn(LoxValue) -> LoxResult, Box<LoxValue>),
    /// A class.
    Class(LoxRc<LoxClass>),
    /// An instance of a class.
    Instance(Shared<LoxInstance>),
    /// A set of key-value pairs.
    Map(Shared<HashMap<Entry, Entry>>),
    /// A sequence of bytes.
    Bytes(Bytes),
    /// A wrapped error.
    Error(LoxError),
    /// An asynchronous function.
    #[cfg(feature = "async")]
    Coroutine(LoxRc<async_types::Coroutine>),
    /// A value returned from an async function.
    #[cfg(feature = "async")]
    Future(Shared<async_types::LoxFuture>),
    /// Nothing.
    Nil,
    /// An object that couldn't normally be represented in Lox.
    #[cfg_attr(feature = "serialise", serde(serialize_with = "serialise::external"))]
    External(Shared<DynLoxObject>),
    #[doc(hidden)] // Not public API.
    Undefined(&'static str),
}

impl LoxValue {
    /// Returns the value wrapped by `self` if `self` is a `LoxValue::Bool`.
    pub fn as_bool(&self) -> Result<bool, LoxError> {
        if let LoxValue::Bool(value) = self {
            Ok(*value)
        } else {
            Err(LoxError::type_error(format!("{self} is not a boolean")))
        }
    }

    /// Returns the value wrapped by `self` if `self` is a `LoxValue::Str`.
    pub fn as_str(&self) -> Result<LoxRc<String>, LoxError> {
        if let LoxValue::Str(value) = self {
            Ok(LoxRc::clone(value))
        } else {
            Err(LoxError::type_error(format!("{self} is not a string")))
        }
    }

    /// Returns the value wrapped by `self` if `self` is a `LoxValue::Num`.
    pub fn as_num(&self) -> Result<f64, LoxError> {
        if let LoxValue::Num(value) = self {
            Ok(*value)
        } else {
            Err(LoxError::type_error(format!("{self} is not a number")))
        }
    }

    /// Returns the value wrapped by `self` if `self` is a `LoxValue::Arr`.
    pub fn as_arr(&self) -> Result<Shared<Vec<LoxValue>>, LoxError> {
        if let LoxValue::Arr(value) = self {
            Ok(LoxRc::clone(value))
        } else {
            Err(LoxError::type_error(format!("{self} is not an array")))
        }
    }

    /// Returns the value wrapped by `self` if `self` is a `LoxValue::Function`.
    pub fn as_function(&self) -> Result<LoxRc<LoxFn>, LoxError> {
        if let LoxValue::Function(value) = self {
            Ok(LoxRc::clone(value))
        } else {
            Err(LoxError::type_error(format!("{self} is not a function")))
        }
    }

    /// Returns the value wrapped by `self` if `self` is a `LoxValue::Class`.
    pub fn as_class(&self) -> Result<LoxRc<LoxClass>, LoxError> {
        if let LoxValue::Class(value) = self {
            Ok(LoxRc::clone(value))
        } else {
            Err(LoxError::type_error(format!("{self} is not a class")))
        }
    }

    /// Returns the value wrapped by `self` if `self` is a `LoxValue::Instance`.
    pub fn as_instance(&self) -> Result<Shared<LoxInstance>, LoxError> {
        if let LoxValue::Instance(value) = self {
            Ok(LoxRc::clone(value))
        } else {
            Err(LoxError::type_error(format!("{self} is not an instance")))
        }
    }

    /// Returns the value wrapped by `self` if `self` is a `LoxValue::Map`.
    pub fn as_map(&self) -> Result<Shared<HashMap<Entry, Entry>>, LoxError> {
        if let LoxValue::Map(value) = self {
            Ok(LoxRc::clone(value))
        } else {
            Err(LoxError::type_error(format!("{self} is not a map")))
        }
    }

    /// Returns the value wrapped by `self` if `self` is a `LoxValue::Bytes`.
    pub fn as_bytes(&self) -> Result<Bytes, LoxError> {
        if let LoxValue::Bytes(value) = self {
            Ok(value.clone())
        } else {
            Err(LoxError::type_error(format!("{self} is not a bytestring")))
        }
    }

    /// Returns the value wrapped by `self` if `self` is a `LoxValue::Error`.
    pub fn as_error(&self) -> Result<LoxError, LoxError> {
        if let LoxValue::Error(value) = self {
            Ok(value.clone())
        } else {
            Err(LoxError::type_error(format!("{self} is not an error")))
        }
    }

    /// Returns the value wrapped by `self` if `self` is a
    /// `LoxValue::Coroutine`.
    #[cfg(feature = "async")]
    pub fn as_coroutine(&self) -> Result<LoxRc<Coroutine>, LoxError> {
        if let LoxValue::Coroutine(value) = self {
            Ok(LoxRc::clone(value))
        } else {
            Err(LoxError::type_error(format!("{self} is not a coroutine")))
        }
    }

    /// Returns the value wrapped by `self` if `self` is a `LoxValue::Future`.
    #[cfg(feature = "async")]
    pub fn as_future(&self) -> Result<Shared<LoxFuture>, LoxError> {
        if let LoxValue::Future(value) = self {
            Ok(LoxRc::clone(value))
        } else {
            Err(LoxError::type_error(format!("{self} is not a future")))
        }
    }

    /// Gets the element of `self` corresponding to `index`.
    pub fn index(&self, index: LoxValue) -> Result<LoxValue, LoxError> {
        let output = match self {
            LoxValue::Arr(arr) => {
                let index = index.clone().try_into().map_err(|_| {
                    LoxError::type_error(format!("invalid base for index: {index}"))
                })?;

                read(arr)
                    .get(index)
                    .ok_or(LoxError::index_out_of_range(index))?
                    .clone()
            }
            LoxValue::Map(map) => read(map)
                .get(&Entry(index.clone()))
                .ok_or(LoxError::invalid_key(index))?
                .0
                .clone(),
            _ => {
                return Err(LoxError::type_error(format!(
                    "cannot index into a value of type {}",
                    LoxValueType::from(self)
                )));
            }
        };

        Ok(output)
    }

    /// Sets the element of `self` corresponding to `index` to `value`.
    pub fn index_set(&self, index: LoxValue, value: LoxValue) -> Result<(), LoxError> {
        match self {
            LoxValue::Arr(arr) => {
                let index = index.clone().try_into().map_err(|_| {
                    LoxError::type_error(format!("invalid base for index: {index}"))
                })?;

                if index < read(arr).len() {
                    write(arr)[index] = value;
                } else {
                    return Err(LoxError::index_out_of_range(index));
                }

                Ok(())
            }
            LoxValue::Map(map) => {
                write(map).insert(Entry(index), Entry(value));

                Ok(())
            }
            _ => Err(LoxError::type_error(format!(
                "cannot index into a value of type {}",
                LoxValueType::from(self)
            ))),
        }
    }

    /// Returns `false` if self is `false` or `nil`, and `true` otherwise.
    pub fn is_truthy(&self) -> bool {
        !matches!(self, LoxValue::Bool(false) | LoxValue::Nil)
    }

    #[doc(hidden)] // Not public API.
    pub fn function(func: LoxFn) -> LoxValue {
        LoxValue::Function(LoxRc::new(func))
    }

    #[doc(hidden)] // Not public API.
    pub fn map<const N: usize>(values: [(LoxValue, LoxValue); N]) -> LoxResult {
        let map: Result<HashMap<_, _>, LoxError> = values
            .into_iter()
            .map(|(key, value)| Ok((Entry::verify_key(key)?, Entry(value))))
            .collect();
        Ok(LoxValue::Map(Shared::new(map?.into())))
    }

    #[doc(hidden)] // Not public API.
    #[cfg(feature = "async")]
    pub fn coroutine<
        F: Fn(LoxArgs) -> Box<dyn Future<Output = LoxResult> + Send + Sync> + Send + Sync + 'static,
    >(
        fun: F,
        params: Vec<&'static str>,
    ) -> LoxValue {
        LoxValue::Coroutine(LoxRc::new(async_types::Coroutine::new(
            Box::new(fun),
            params,
        )))
    }

    #[inline(always)]
    fn get_impl(&self, key: &'static str) -> Result<LoxValue, Option<LoxError>> {
        static PRIMITIVE_METHODS: OnceLock<HashMap<&'static str, fn(LoxValue) -> LoxResult>> =
            OnceLock::new();
        fn init_primitives() -> HashMap<&'static str, fn(LoxValue) -> LoxResult> {
            HashMap::from_iter([
                ("is_bool", primitive_methods::is_bool as fn(_) -> _),
                ("is_str", primitive_methods::is_str),
                ("is_num", primitive_methods::is_num),
                ("is_arr", primitive_methods::is_arr),
                ("is_function", primitive_methods::is_function),
                ("is_class", primitive_methods::is_class),
                ("is_map", primitive_methods::is_map),
                ("is_bytes", primitive_methods::is_bytes),
                ("is_error", primitive_methods::is_error),
                ("is_nil", primitive_methods::is_nil),
            ])
        }

        if let LoxValue::Instance(instance) = self {
            if let Some(attr) = read(instance).attributes.get(key) {
                Ok(attr.clone())
            } else {
                Ok(LoxValue::BoundMethod(
                    read(instance).class.get(key).ok_or(None)?,
                    instance.clone(),
                ))
            }
        } else if let Some(method) = PRIMITIVE_METHODS.get_or_init(init_primitives).get(key) {
            Ok(LoxValue::PrimitiveMethod(*method, Box::new(self.clone())))
        } else if let LoxValue::External(object) = self {
            read(object).get(LoxRc::clone(object), key)
        } else {
            Err(None)
        }
    }

    /// Gets the attribute corresponding to `self.key`, if it exists.
    pub fn get(&self, key: &'static str) -> LoxResult {
        self.get_impl(key)
            .map_err(|err| err.unwrap_or(LoxError::invalid_property(key, self.to_string())))
    }

    /// Gets the attribute corresponding to `self.key` to `value`, if it exists.
    pub fn set(&self, key: &'static str, value: LoxValue) -> LoxResult {
        if let LoxValue::Instance(instance) = self {
            write(instance)
                .attributes
                .insert(key.to_string(), value.clone());
            Ok(value)
        } else if let LoxValue::External(object) = self {
            write(object)
                .set(LoxRc::clone(object), key, value.clone())
                .map(|()| value)
                .map_err(|err| {
                    err.unwrap_or_else(|| LoxError::invalid_property(key, self.to_string()))
                })
        } else {
            Err(LoxError::invalid_property(key, self.to_string()))
        }
    }

    #[doc(hidden)] // Not public API.
    pub fn expect_class(&self) -> Result<&LoxRc<LoxClass>, LoxError> {
        if let LoxValue::Class(class) = self {
            Ok(class)
        } else {
            Err(LoxError::type_error(format!(
                "Cannot use {self} as a superclass"
            )))
        }
    }

    fn super_fn_impl(&self, name: &'static str) -> Option<LoxMethod> {
        let instance = self.as_instance().ok()?;
        let mut class = &read(&instance).class;
        while class.methods.get(name).is_none() {
            class = class.superclass.as_ref()?;
        }
        Some(class.superclass.as_ref()?.methods.get(name)?.clone())
    }

    #[doc(hidden)] // Not public API.
    pub fn super_fn(&self, name: &'static str) -> Result<LoxMethod, LoxError> {
        self.super_fn_impl(name)
            .ok_or(LoxError::non_existent_super(name))
    }

    #[doc(hidden)] // Not public API.
    pub fn bind<F: Into<LoxMethod>>(fun: F, instance: LoxValue) -> LoxValue {
        LoxValue::BoundMethod(fun.into(), instance.as_instance().unwrap())
    }

    #[doc(hidden)] // Not public API.
    pub fn into_error(self) -> LoxError {
        if let LoxValue::Error(err) = self {
            err
        } else {
            LoxError::value(self)
        }
    }

    /// Calls `self` with the given arguments, if `self` is a function or a
    /// class.
    pub fn call(&self, mut args: LoxArgs) -> LoxResult {
        match self {
            Self::Function(func) => (func.fun)(args.check_arity(func.params.len())?),
            Self::BoundMethod(func, instance) => {
                args.head = Some(LoxValue::Instance(instance.clone()));
                func.call(args.check_arity(func.params().len() - 1)?)
            }
            Self::Class(class) => {
                let instance = LoxValue::Instance(Shared::new(
                    LoxInstance {
                        class: LoxRc::clone(class),
                        attributes: HashMap::new(),
                    }
                    .into(),
                ));
                let mut args = args.check_arity(class.arity())?;
                if let Some(initialiser) = &class.initialiser {
                    args.head = Some(instance.clone());
                    (initialiser.fun)(args)
                } else {
                    Ok(instance)
                }
            }
            Self::PrimitiveMethod(func, object) => func((**object).clone()),
            #[cfg(feature = "async")]
            Self::Coroutine(func) => Ok(LoxValue::Future(Shared::new(
                func.start(args.check_arity(func.params().len())?).into(),
            ))),
            _ => panic!("cannot call value of type {}", LoxValueType::from(self)),
        }
    }

    #[inline(always)]
    fn as_external_error(&self) -> LoxError {
        LoxError::type_error(format!("cannot cast {self} to an external object"))
    }

    /// Returns the external object wrapped by `self` if it exists, or a type
    /// error if it doesn't.
    #[cfg(feature = "sync")]
    pub fn as_external(self) -> Result<Shared<dyn LoxObject + Send + Sync>, LoxError> {
        if let LoxValue::External(obj) = self {
            Ok(obj)
        } else {
            Err(self.as_external_error())
        }
    }

    /// Returns the external object wrapped by `self` if it exists, or a type
    /// error if it doesn't.
    #[cfg(not(feature = "sync"))]
    pub fn as_external(self) -> Result<Shared<dyn LoxObject>, LoxError> {
        if let LoxValue::External(obj) = self {
            Ok(obj)
        } else {
            Err(self.as_external_error())
        }
    }

    /// Creates a new `LoxValue::External` with the given value.
    #[cfg(feature = "sync")]
    pub fn external<T: LoxObject + Send + Sync + 'static>(value: T) -> LoxValue {
        let shared = Shared::new(value.into());
        LoxValue::External(shared as Shared<dyn LoxObject + Send + Sync>)
    }

    /// Creates a new `LoxValue::External` with the given value.
    #[cfg(not(feature = "sync"))]
    pub fn external<T: LoxObject + 'static>(value: T) -> LoxValue {
        let shared = Shared::new(value.into());
        LoxValue::External(shared as Shared<dyn LoxObject>)
    }
}

impl fmt::Debug for LoxValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Bool(b) => write!(f, "Bool({b})"),
            Self::Str(s) => write!(f, "Str({})", s),
            Self::Num(n) => write!(f, "Num({n})"),
            Self::Arr(a) => write!(f, "Arr({:#?})", a),
            Self::Function(func) => write!(f, "Function({:#?})", func.params),
            Self::BoundMethod(func, instance) => {
                write!(f, "BoundMethod({:#?}, {:#?})", func.params(), instance)
            }
            Self::PrimitiveMethod(_, object) => {
                write!(f, "PrimitiveMethod({:#?})", object)
            }
            Self::Class(class) => write!(f, "Class({:#?})", class),
            Self::Instance(instance) => write!(f, "Instance({:#?})", read(instance).deref()),
            Self::Map(map) => write!(f, "Map({:#?})", read(map).deref()),
            Self::Bytes(bytes) => write!(f, "Bytes({:#?})", bytes),
            Self::Error(error) => write!(f, "Error({:#?})", error),
            #[cfg(feature = "async")]
            Self::Coroutine(fun) => write!(f, "Coroutine({:#?})", fun.params()),
            #[cfg(feature = "async")]
            Self::Future(_) => write!(f, "Future"),
            Self::Nil => write!(f, "Nil"),
            Self::External(_) => write!(f, "External"),
            Self::Undefined(_) => write!(f, "Undefined"),
        }
    }
}

impl<T> PartialEq<T> for LoxValue
where
    T: Into<LoxValue> + Clone,
{
    fn eq(&self, rhs: &T) -> bool {
        let other: Self = rhs.clone().into();

        match (self, other) {
            (&Self::Bool(b1), Self::Bool(b2)) => b1 == b2,
            (Self::Str(s1), Self::Str(s2)) => s1 == &s2,
            (&Self::Num(n1), Self::Num(n2)) => n1 == n2,
            (Self::Arr(a1), Self::Arr(a2)) => read(a1).deref() == read(&a2).deref(),
            (Self::Function(f1), Self::Function(f2)) => f1 == &f2,
            (Self::BoundMethod(f1, _), Self::BoundMethod(f2, _)) => f1 == &f2,
            (Self::PrimitiveMethod(f1, _), Self::PrimitiveMethod(f2, _)) => f1 == &f2,
            (Self::Class(c1), Self::Class(c2)) => c1 == &c2,
            (Self::Instance(i1), Self::Instance(i2)) => read(i1).deref() == read(&i2).deref(),
            (Self::Map(m1), Self::Map(m2)) => read(m1).deref() == read(&m2).deref(),
            (Self::Bytes(b1), Self::Bytes(b2)) => b1 == &b2,
            (Self::Error(e1), Self::Error(e2)) => e1 == &e2,
            #[cfg(feature = "async")]
            (Self::Coroutine(f1), Self::Coroutine(f2)) => f1 == &f2,
            #[cfg(feature = "async")]
            (Self::Future(f1), Self::Future(f2)) => read(f1).deref() == read(&f2).deref(),
            (Self::Nil, Self::Nil) => true,
            (Self::External(e1), Self::External(e2)) => LoxRc::ptr_eq(e1, &e2),
            _ => false,
        }
    }
}

fn hash_ptr<T: ?Sized, H: std::hash::Hasher>(ptr: *const T, state: &mut H) {
    (ptr as *const ()).hash(state)
}

impl Hash for LoxValue {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::Bool(b) => b.hash(state),
            Self::Str(s) => s.hash(state),
            &Self::Num(mut n) => {
                if n.is_nan() {
                    n = fastrand::f64();
                }
                let bits = n.to_bits();
                let sign: i8 = if bits >> 63 == 0 { 1 } else { -1 };
                let mut exponent = ((bits >> 52) & 0x7ff) as i16;
                let mantissa = if exponent == 0 {
                    (bits & 0xfffffffffffff) << 1
                } else {
                    (bits & 0xfffffffffffff) | 0x10000000000000
                };
                exponent -= 1023 + 52;
                (mantissa, exponent, sign).hash(state);
            }
            Self::Arr(arr) => read(arr).hash(state),
            Self::Function(func) => hash_ptr(func.fun.as_ref(), state),
            Self::BoundMethod(func, _) => func.hash(state),
            Self::PrimitiveMethod(func, _) => func.hash(state),
            Self::Class(class) => class.hash(state),
            Self::Instance(instance) => read(instance).hash(state),
            Self::Map(_) => panic!("cannot hash a hashmap"),
            Self::Bytes(bytes) => bytes.hash(state),
            Self::Error(err) => err.hash(state),
            #[cfg(feature = "async")]
            Self::Coroutine(func) => func.hash(state),
            #[cfg(feature = "async")]
            Self::Future(fut) => read(fut).hash(state),
            Self::Nil => {}
            Self::External(external) => LoxRc::as_ptr(external).hash(state),
            Self::Undefined(_) => unreachable!(),
        }
    }
}

impl fmt::Display for LoxValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Bool(value) => {
                write!(f, "{}", value)
            }
            Self::Str(string) => f.write_str(string),
            Self::Num(value) => {
                write!(f, "{}", value)
            }
            Self::Arr(values) => {
                let mut buf = "[".to_string();
                for value in read(values).iter().take(read(values).len() - 1) {
                    buf += &(value.to_string() + ", ");
                }
                if let Some(value) = read(values).last() {
                    buf += &value.to_string();
                }

                buf += "]";
                f.write_str(&buf)
            }
            Self::Function(_) => {
                write!(f, "<function>")
            }
            Self::BoundMethod(_, _) => write!(f, "<bound method>"),
            Self::PrimitiveMethod(_, _) => write!(f, "<bound method>"),
            Self::Class(_) => write!(f, "<class>"),
            Self::Instance(instance) => {
                write!(f, "<instance of {}>", read(instance).class.name)
            }
            Self::Map(map) => {
                let mut buf = "{\n".to_string();
                for (key, value) in read(map).deref() {
                    buf.push_str("    \n");
                    buf += &(key.0.to_string() + ": ");
                    buf += &value.0.to_string();
                }
                buf.push_str("}");
                f.write_str(&buf)
            }
            Self::Bytes(bytes) => write!(f, "{:?}", bytes),
            Self::Error(error) => write!(f, "{error}"),
            #[cfg(feature = "async")]
            Self::Coroutine(_) => write!(f, "<async function>"),
            #[cfg(feature = "async")]
            Self::Future(_) => write!(f, "<future>"),
            Self::Nil => {
                write!(f, "nil")
            }
            Self::External(_) => write!(f, "<external object>"),
            Self::Undefined(_) => unreachable!(),
        }
    }
}

impl<T> From<&T> for LoxValue
where
    T: Clone + Into<LoxValue>,
{
    fn from(value: &T) -> Self {
        value.clone().into()
    }
}

impl From<bool> for LoxValue {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}

impl From<char> for LoxValue {
    fn from(value: char) -> Self {
        Self::Str(value.to_string().into())
    }
}

impl From<Vec<char>> for LoxValue {
    fn from(chars: Vec<char>) -> Self {
        Self::Str(chars.into_iter().collect::<String>().into())
    }
}

impl From<String> for LoxValue {
    fn from(value: String) -> Self {
        Self::from(Vec::from_iter(value.chars()))
    }
}

impl From<&str> for LoxValue {
    fn from(value: &str) -> Self {
        Self::from(value.to_string())
    }
}

impl From<Vec<LoxValue>> for LoxValue {
    fn from(values: Vec<LoxValue>) -> Self {
        Self::Arr(Shared::new(values.into()))
    }
}

impl<const N: usize> From<[LoxValue; N]> for LoxValue {
    fn from(values: [LoxValue; N]) -> Self {
        Self::from(Vec::from(values))
    }
}

impl From<LoxError> for LoxValue {
    fn from(value: LoxError) -> Self {
        LoxValue::Error(value)
    }
}

impl From<()> for LoxValue {
    fn from(_value: ()) -> Self {
        LoxValue::Nil
    }
}

impl From<LoxRc<LoxClass>> for LoxValue {
    fn from(value: LoxRc<LoxClass>) -> Self {
        LoxValue::Class(value)
    }
}

impl From<HashMap<Entry, Entry>> for LoxValue {
    fn from(value: HashMap<Entry, Entry>) -> Self {
        LoxValue::Map(LoxRc::new(value.into()))
    }
}

impl From<Bytes> for LoxValue {
    fn from(value: Bytes) -> Self {
        LoxValue::Bytes(value)
    }
}

impl TryFrom<LoxValue> for bool {
    type Error = LoxError;

    fn try_from(value: LoxValue) -> Result<Self, Self::Error> {
        if let LoxValue::Bool(b) = value {
            Ok(b)
        } else {
            Err(LoxError::type_error(format!(
                "expected bool, found {}",
                LoxValueType::from(value)
            )))
        }
    }
}

impl TryFrom<LoxValue> for String {
    type Error = LoxError;

    fn try_from(value: LoxValue) -> Result<Self, Self::Error> {
        if let LoxValue::Str(string) = value {
            Ok(string.to_string())
        } else {
            Err(LoxError::type_error(format!(
                "expected string, found {}",
                LoxValueType::from(value)
            )))
        }
    }
}

impl TryFrom<LoxValue> for LoxRc<String> {
    type Error = LoxError;

    fn try_from(value: LoxValue) -> Result<Self, Self::Error> {
        if let LoxValue::Str(string) = value {
            Ok(string)
        } else {
            Err(LoxError::type_error(format!(
                "expected string, found {}",
                LoxValueType::from(value)
            )))
        }
    }
}

impl TryFrom<LoxValue> for LoxRc<LoxFn> {
    type Error = LoxError;

    fn try_from(value: LoxValue) -> Result<Self, Self::Error> {
        if let LoxValue::Function(fun) = value {
            Ok(fun)
        } else {
            Err(LoxError::type_error(format!(
                "expected function, found {}",
                LoxValueType::from(value)
            )))
        }
    }
}

impl TryFrom<LoxValue> for Shared<HashMap<Entry, Entry>> {
    type Error = LoxError;

    fn try_from(value: LoxValue) -> Result<Self, Self::Error> {
        value.as_map()
    }
}

impl TryFrom<LoxValue> for HashMap<Entry, Entry> {
    type Error = LoxError;

    fn try_from(value: LoxValue) -> Result<Self, Self::Error> {
        value.as_map().map(|value| read(&value).clone())
    }
}

impl TryFrom<LoxValue> for Bytes {
    type Error = LoxError;

    fn try_from(value: LoxValue) -> Result<Self, Self::Error> {
        value.as_bytes()
    }
}

#[cfg(feature = "async")]
impl TryFrom<LoxValue> for LoxRc<async_types::Coroutine> {
    type Error = LoxError;

    fn try_from(value: LoxValue) -> Result<Self, Self::Error> {
        if let LoxValue::Coroutine(fun) = value {
            Ok(fun)
        } else {
            Err(LoxError::type_error(format!(
                "expected coroutine, found {}",
                LoxValueType::from(value)
            )))
        }
    }
}

impl TryFrom<LoxValue> for Shared<LoxInstance> {
    type Error = LoxError;

    fn try_from(value: LoxValue) -> Result<Self, Self::Error> {
        if let LoxValue::Instance(fun) = value {
            Ok(fun)
        } else {
            Err(LoxError::type_error(format!(
                "expected instance, found {}",
                LoxValueType::from(value)
            )))
        }
    }
}

macro_rules! impl_tryfrom_borrowed_loxvalue {
    ( $($t:ty),* ) => { $(
        impl TryFrom<&LoxValue> for $t {
            type Error = LoxError;

            fn try_from(value: &LoxValue) -> Result<Self, Self::Error> {
                value.clone().try_into()
            }
        }
    ) *
    };
}

impl_tryfrom_borrowed_loxvalue! { bool, String, f32, f64, u8, u16, u32, u64, u128, usize, i8, i16, i32, i64, i128, isize }

macro_rules! impl_numeric {
    ( $($t:ty),* ) => {
    $(
        impl From<$t> for LoxValue {
            fn from(value: $t) -> Self {
                Self::Num(value as f64)
            }
        }

        impl TryFrom<LoxValue> for $t {
            type Error = LoxError;

            fn try_from(value: LoxValue) -> Result<Self, LoxError> {
                match value {
                    LoxValue::Num(ref num) => {
                        if *num > Self::MAX as f64 {
                            Err(LoxError {
                                    inner: LoxErrorInner::SizeError {
                                        found: mem::size_of::<Self>(),
                                    },
                                    trace: VecDeque::new(),
                                }
                            )
                        } else {
                            Ok(*num as Self)
                        }
                    },
                    _ => Err(LoxError::type_error(format!("expected number, found {}", LoxValueType::from(value))))
                }
            }
        }

        impl ops::Add<$t> for LoxValue {
            type Output = LoxResult;

            fn add(self, rhs: $t) -> Self::Output {
                match self {
                    Self::Num(num) => Ok(Self::Num(num + rhs as f64)),
                    _ => Err(LoxError::type_error(format!("cannot add number to {}", LoxValueType::from(self)))),
                }
            }
        }

        impl ops::Sub<$t> for LoxValue {
            type Output = LoxResult;

            fn sub(self, rhs: $t) -> Self::Output {
                match self {
                    Self::Num(num) => Ok(Self::Num(num - rhs as f64)),
                    _ => Err(LoxError::type_error(format!("cannot subtract number from {}", LoxValueType::from(self)))),
                }
            }
        }

        impl ops::Mul<$t> for LoxValue {
            type Output = LoxResult;

            fn mul(self, rhs: $t) -> Self::Output {
                match self {
                    Self::Num(num) => Ok(Self::Num(num * rhs as f64)),
                    _ => Err(LoxError::type_error(format!("cannot multiply {} by number", LoxValueType::from(self)))),
                }
            }
        }

        impl ops::Div<$t> for LoxValue {
            type Output = LoxResult;

            fn div(self, rhs: $t) -> Self::Output {
                match self {
                    Self::Num(num) => Ok(Self::Num(num / rhs as f64)),
                    _ => Err(LoxError::type_error(format!("cannot divide {} by number", LoxValueType::from(self)))),
                }
            }
        }
    ) *
    };
}

impl_numeric! { f32, f64, u8, u16, u32, u64, u128, usize, i8, i16, i32, i64, i128, isize }

impl ops::Add for LoxValue {
    type Output = LoxResult;

    fn add(self, rhs: Self) -> Self::Output {
        let self_type = LoxValueType::from(&self);
        match (self, &rhs) {
            (LoxValue::Str(s1), LoxValue::Str(s2)) => {
                Ok(LoxValue::Str(LoxRc::new(s1.to_string() + s2)))
            }
            (LoxValue::Num(n1), &LoxValue::Num(n2)) => Ok(LoxValue::Num(n1 + n2)),
            (LoxValue::Arr(arr1), LoxValue::Arr(ref arr2)) => {
                write(&arr1).append(&mut write(arr2));
                Ok(LoxValue::Arr(arr1))
            }
            _ => Err(LoxError::type_error(format!(
                "cannot add {} to {}",
                LoxValueType::from(rhs),
                self_type,
            ))),
        }
    }
}

impl ops::Add<&str> for LoxValue {
    type Output = LoxResult;

    fn add(self, rhs: &str) -> Self::Output {
        let other: LoxValue = rhs.into();
        self + other
    }
}

impl ops::Sub for LoxValue {
    type Output = LoxResult;

    fn sub(self, rhs: Self) -> Self::Output {
        if let (&Self::Num(num1), &Self::Num(num2)) = (&self, &rhs) {
            Ok(LoxValue::Num(num1 - num2))
        } else {
            Err(LoxError::type_error(format!(
                "cannot subtract {} from {}",
                LoxValueType::from(rhs),
                LoxValueType::from(self),
            )))
        }
    }
}

impl ops::Mul for LoxValue {
    type Output = LoxResult;

    fn mul(self, rhs: Self) -> Self::Output {
        if let (&Self::Num(num1), &Self::Num(num2)) = (&self, &rhs) {
            Ok(LoxValue::Num(num1 * num2))
        } else {
            Err(LoxError::type_error(format!(
                "cannot multiply {} by {}",
                LoxValueType::from(self),
                LoxValueType::from(rhs),
            )))
        }
    }
}

impl ops::Div for LoxValue {
    type Output = LoxResult;

    fn div(self, rhs: Self) -> Self::Output {
        if let (&Self::Num(num1), &Self::Num(num2)) = (&self, &rhs) {
            Ok(LoxValue::Num(num1 / num2))
        } else {
            Err(LoxError::type_error(format!(
                "cannot divide {} by {}",
                LoxValueType::from(self),
                LoxValueType::from(rhs),
            )))
        }
    }
}

impl ops::Neg for LoxValue {
    type Output = LoxResult;

    fn neg(self) -> Self::Output {
        match self {
            Self::Num(num) => Ok(Self::Num(-num)),
            _ => Err(LoxError::type_error(format!(
                "cannot negate {}",
                LoxValueType::from(self)
            ))),
        }
    }
}

impl ops::Not for LoxValue {
    type Output = LoxResult;

    fn not(self) -> Self::Output {
        match self {
            Self::Bool(b) => Ok(Self::Bool(!b)),
            _ => Err(LoxError::type_error(format!(
                "cannot take logical not of {}",
                LoxValueType::from(self)
            ))),
        }
    }
}

impl<T> PartialOrd<T> for LoxValue
where
    T: Into<LoxValue> + Clone,
{
    fn partial_cmp(&self, other: &T) -> Option<cmp::Ordering> {
        let other: Self = other.clone().into();
        match (self, other) {
            (Self::Bool(b1), Self::Bool(b2)) => b1.partial_cmp(&b2),
            (Self::Num(n1), Self::Num(n2)) => n1.partial_cmp(&n2),
            (Self::Str(s1), Self::Str(s2)) => s1.partial_cmp(&s2),
            (Self::Arr(a1), Self::Arr(a2)) => read(a1).partial_cmp(&read(&a2)),
            _ => None,
        }
    }
}

impl Termination for LoxValue {
    fn report(self) -> ExitCode {
        match self {
            Self::Num(n) => ExitCode::from(n as u8),
            Self::Error(err) => {
                eprintln!("{err}");
                ExitCode::FAILURE
            }
            _ => ExitCode::SUCCESS,
        }
    }
}

impl IntoIterator for LoxValue {
    type Item = Self;
    type IntoIter = LoxIterator;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            Self::Arr(arr) => LoxIterator::Array(read(&arr).clone().into_iter()),
            Self::Str(string) => LoxIterator::String(string.to_string().into_bytes().into_iter()),
            _ => panic!("cannot convert {} into iterator", LoxValueType::from(self)),
        }
    }
}

/// An iterator over a `LoxValue::Array` or a `LoxValue::String`.
#[non_exhaustive]
pub enum LoxIterator {
    #[doc(hidden)] // Not public API.
    Array(vec::IntoIter<LoxValue>),
    #[doc(hidden)] // Not public API.
    String(vec::IntoIter<u8>),
}

const CONT_MASK: u8 = 0b0011_1111;

const fn utf8_first_byte(byte: u8, width: u32) -> u32 {
    (byte & (0x7F >> width)) as u32
}

const fn utf8_acc_cont_byte(ch: u32, byte: u8) -> u32 {
    (ch << 6) | (byte & CONT_MASK) as u32
}

/// ## Panics
/// Panics if `bytes` does not produce a valid UTF-8 string.
fn next_code_point<I: Iterator<Item = u8>>(bytes: &mut I) -> Option<u32> {
    // Decode UTF-8
    let x = bytes.next()?;
    if x < 128 {
        return Some(x as u32);
    }

    // Multibyte case follows
    // Decode from a byte combination out of: [[[x y] z] w]
    let init = utf8_first_byte(x, 2);
    let y = bytes.next().unwrap();
    let mut ch = utf8_acc_cont_byte(init, y);
    if x >= 0xE0 {
        // [[x y z] w] case
        // 5th bit in 0xE0 .. 0xEF is always clear, so `init` is still valid
        let z = bytes.next().unwrap();
        let y_z = utf8_acc_cont_byte((y & CONT_MASK) as u32, z);
        ch = init << 12 | y_z;
        if x >= 0xF0 {
            // [x y z w] case
            // use only the lower 3 bits of `init`
            let w = bytes.next().unwrap();
            ch = (init & 7) << 18 | utf8_acc_cont_byte(y_z, w);
        }
    }

    Some(ch)
}

impl Iterator for LoxIterator {
    type Item = LoxValue;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            LoxIterator::Array(iter) => iter.next(),
            LoxIterator::String(bytes) => Some(LoxValue::Str(
                char::from_u32(next_code_point(bytes)?)
                    .unwrap()
                    .to_string()
                    .into(),
            )),
        }
    }
}

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
}

impl fmt::Debug for LoxFn {
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
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        hash_ptr(self.fun.as_ref(), state);
    }
}

/// An instance of a Lox class.
#[derive(Debug, PartialEq)]
#[cfg_attr(feature = "serialise", derive(Serialize))]
pub struct LoxInstance {
    class: LoxRc<LoxClass>,
    attributes: HashMap<String, LoxValue>,
}

impl LoxInstance {
    /// Checks if `self` is an instance of `target_class` or one of its
    /// subclasses.
    pub fn instance_of(&self, target_class: LoxRc<LoxClass>) -> bool {
        let mut current_class = Some(self.class.clone());
        while let Some(class) = current_class {
            if class == target_class {
                return true;
            } else {
                current_class = class.superclass.clone();
            }
        }
        false
    }
}

impl Hash for LoxInstance {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.class.hash(state);
        let mut attributes: Vec<_> = self.attributes.iter().collect();
        attributes.sort_unstable_by(|(name1, _), (name2, _)| name1.cmp(name2));
        for attributes in attributes {
            attributes.hash(state);
        }
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
    fn params(&self) -> &[&'static str] {
        match self {
            LoxMethod::Sync(fun) => &fun.params,
            #[cfg(feature = "async")]
            LoxMethod::Async(fun) => fun.params(),
        }
    }

    fn get_sync(self) -> Option<LoxRc<LoxFn>> {
        match self {
            LoxMethod::Sync(fun) => Some(fun),
            #[cfg(feature = "async")]
            LoxMethod::Async(_) => None,
        }
    }

    fn call(&self, args: LoxArgs) -> LoxResult {
        match self {
            LoxMethod::Sync(fun) => (fun.fun)(args),
            #[cfg(feature = "async")]
            LoxMethod::Async(fun) => Ok(LoxValue::Future(Shared::new(fun.start(args).into()))),
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

/// A class defined in Lox code.
#[derive(Debug, PartialEq)]
#[cfg_attr(feature = "serialise", derive(Serialize))]
pub struct LoxClass {
    name: &'static str,
    initialiser: Option<LoxRc<LoxFn>>,
    methods: HashMap<&'static str, LoxMethod>,
    superclass: Option<LoxRc<LoxClass>>,
}

impl LoxClass {
    #[doc(hidden)] // Not public API.
    pub fn new(
        name: &'static str,
        methods: HashMap<&'static str, LoxMethod>,
        superclass: Option<LoxRc<LoxClass>>,
    ) -> LoxClass {
        let mut class = LoxClass {
            name,
            initialiser: None,
            methods,
            superclass: Some(superclass.unwrap_or_else(|| {
                let instance_of = |args: LoxArgs| -> LoxResult {
                    let this = args.get(0).unwrap().as_instance().unwrap();
                    let target_class = args.get(0).unwrap().expect_class()?.clone();
                    let is_instance = read(&this).instance_of(target_class);

                    Ok(LoxValue::Bool(is_instance))
                };
                LoxRc::new(LoxClass {
                    name: "object",
                    initialiser: None,
                    methods: HashMap::from_iter([(
                        "instance_of",
                        LoxMethod::Sync(LoxRc::new(LoxFn {
                            fun: Box::new(instance_of),
                            params: vec!["class"],
                        })),
                    )]),
                    superclass: None,
                })
            })),
        };

        class.initialiser = class.get("init").and_then(|fun| fun.get_sync());

        class
    }

    fn get(&self, key: &str) -> Option<LoxMethod> {
        if let Some(method) = self.methods.get(key) {
            Some(method.clone())
        } else {
            self.superclass
                .as_ref()
                .and_then(|superclass| superclass.get(key))
        }
    }

    fn arity(&self) -> usize {
        if let Some(initialiser) = &self.initialiser {
            initialiser.params.len() - 1
        } else {
            0
        }
    }
}

impl Hash for LoxClass {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.initialiser.hash(state);
        self.superclass.hash(state);
        let mut methods: Vec<_> = self
            .methods
            .iter()
            .map(|(&name, method)| (name, method))
            .collect();
        methods.sort_unstable_by(|(name1, _), (name2, _)| name1.cmp(name2));
        for method in methods {
            method.hash(state);
        }
    }
}

/// Arguments to a Lox function.
#[cfg_attr(feature = "serialise", derive(Serialize))]
pub struct LoxArgs {
    head: Option<LoxValue>,
    main: Vec<LoxValue>,
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
    pub fn drain(&mut self) -> Drain {
        Drain {
            head: &mut self.head,
            main: self.main.drain(..),
        }
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

    fn check_arity(self, arity: usize) -> Result<LoxArgs, LoxError> {
        if self.main.len() == arity {
            Ok(self)
        } else {
            Err(LoxError::incorrect_arity(arity, self.main.len()))
        }
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

/// A moving iterator over [`LoxArgs`].
pub struct Drain<'a> {
    head: &'a mut Option<LoxValue>,
    main: vec::Drain<'a, LoxValue>,
}

impl Iterator for Drain<'_> {
    type Item = LoxValue;

    fn next(&mut self) -> Option<Self::Item> {
        self.head.take().or_else(|| self.main.next())
    }
}

#[doc(hidden)]
pub trait ToLoxResult {
    fn to_lox_result(self) -> LoxResult;
}

#[doc(hidden)]
impl<T: Into<LoxValue>> ToLoxResult for T {
    fn to_lox_result(self) -> LoxResult {
        let value: LoxValue = self.into();
        if let LoxValue::Error(err) = value {
            Err(err)
        } else {
            Ok(value)
        }
    }
}

#[cfg(feature = "sync")]
#[doc(hidden)]
impl<T: Into<LoxValue>, E: Error + Send + Sync + 'static> ToLoxResult for Result<T, E> {
    fn to_lox_result(self) -> LoxResult {
        self.map(|value| value.into()).map_err(|err| {
            if cast!(&err, &LoxError).is_ok() {
                cast!(err, LoxError).unwrap()
            } else {
                LoxError::external(err)
            }
        })
    }
}

#[cfg(not(feature = "sync"))]
#[doc(hidden)]
impl<T: Into<LoxValue>, E: Error + 'static> ToLoxResult for Result<T, E> {
    fn to_lox_result(self) -> LoxResult {
        self.map(|value| value.into()).map_err(|err| {
            if let Ok(lox_error) = cast!(&err, &LoxError) {
                lox_error.clone()
            } else {
                LoxError::external(err)
            }
        })
    }
}

/// A convenient alias for a [`LoxObject`] trait object.
#[cfg(feature = "sync")]
pub type DynLoxObject = dyn LoxObject + Send + Sync + 'static;

/// A convenient alias for a [`LoxObject`] trait object.
#[cfg(not(feature = "sync"))]
pub type DynLoxObject = dyn LoxObject + 'static;

/// A trait for foreign objects that can be used in Lox.
pub trait LoxObject: Any {
    /// A human-friendly name for the type.
    fn name() -> String
    where
        Self: Sized;

    /// Gets the field of `self` corresponding to `key`.
    ///
    /// This method should return `Err(None)` if the value is not found.
    fn get(
        &self,
        this: Shared<DynLoxObject>,
        key: &'static str,
    ) -> Result<LoxValue, Option<LoxError>> {
        let (_, _) = (this, key);
        Err(None)
    }

    /// Sets the field of `self` corresponding to `key` to the given value.
    ///
    /// If the key does not exist, `Err(None)` should be returned.
    fn set(
        &mut self,
        this: Shared<DynLoxObject>,
        key: &'static str,
        value: LoxValue,
    ) -> Result<(), Option<LoxError>> {
        let (_, _, _) = (this, key, value);
        Err(None)
    }
}

fn unsync_is<T: LoxObject>(value: &Shared<dyn LoxObject>) -> bool {
    let t = TypeId::of::<T>();
    let concrete = (*read(value).deref()).type_id();
    t == concrete
}

fn sync_is<T: LoxObject>(value: &Shared<dyn LoxObject + Send + Sync>) -> bool {
    let t = TypeId::of::<T>();
    let concrete = (*read(value).deref()).type_id();
    t == concrete
}

/// Objects which can be downcast to a concrete type.
///
/// This trait is sealed, and cannot be implemented for foreign types.
pub trait Downcast: Sized + Sealed {
    /// Attempts to downcast `self` to a concrete type, returning `Err(self)`
    /// if the cast fails.
    fn downcast<T: LoxObject>(self) -> Result<Shared<T>, Self>;
}

impl Sealed for Shared<dyn LoxObject> {}

impl Downcast for Shared<dyn LoxObject> {
    fn downcast<T: LoxObject>(self) -> Result<Shared<T>, Shared<dyn LoxObject>> {
        if unsync_is::<T>(&self) {
            let ptr = LoxRc::into_raw(self);
            // SAFETY: The `TypeId`s are the same, therefore `self` is an
            // instance of `T`.
            Ok(unsafe { Shared::from_raw(ptr as *const shared::Inner<T>) })
        } else {
            todo!()
        }
    }
}

impl Sealed for Shared<dyn LoxObject + Send + Sync> {}

impl Downcast for Shared<dyn LoxObject + Send + Sync> {
    fn downcast<T: LoxObject>(self) -> Result<Shared<T>, Shared<dyn LoxObject + Send + Sync>> {
        if sync_is::<T>(&self) {
            let ptr = LoxRc::into_raw(self);
            // SAFETY: The `TypeId`s are the same, therefore `self` is an
            // instance of `T`.
            Ok(unsafe { Shared::from_raw(ptr as *const shared::Inner<T>) })
        } else {
            todo!()
        }
    }
}

fn obj_from_value<T: LoxObject>(value: &LoxValue) -> Option<Shared<T>> {
    match value {
        LoxValue::External(external) => external.clone().downcast().ok(),
        _ => None,
    }
}

impl<T: LoxObject> TryFrom<LoxValue> for Shared<T> {
    type Error = LoxError;

    fn try_from(value: LoxValue) -> Result<Self, Self::Error> {
        obj_from_value(&value).ok_or(LoxError::type_error(format!(
            "expected {}, found {value}",
            T::name()
        )))
    }
}

mod private {
    pub trait Sealed {}
}
use private::Sealed;

#[cfg(test)]
mod tests;
