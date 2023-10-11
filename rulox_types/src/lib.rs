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
use std::convert::Infallible;
use std::error::Error;
use std::fmt;
use std::fmt::Debug;
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

use castaway::cast;

#[derive(Debug, Clone)]
#[cfg(feature = "sync")]
struct ExternalError(LoxRc<dyn Error + Send + Sync>);

#[derive(Debug, Clone)]
#[cfg(not(feature = "sync"))]
struct ExternalError(LoxRc<dyn Error>);

impl PartialEq for ExternalError {
    fn eq(&self, other: &Self) -> bool {
        LoxRc::ptr_eq(&self.0, &other.0)
    }
}

/// An error raised during compilation or execution.
#[derive(Debug, Clone, PartialEq)]
pub struct LoxError {
    inner: LoxErrorInner,
    trace: Vec<&'static str>,
}

impl LoxError {
    fn type_error(message: String) -> LoxError {
        LoxError {
            inner: LoxErrorInner::TypeError(message),
            trace: vec![],
        }
    }

    fn undefined_variable(kind: &'static str) -> LoxError {
        LoxError {
            inner: LoxErrorInner::UndefinedVariable(kind),
            trace: vec![],
        }
    }

    fn invalid_property(property: &'static str, object: String) -> LoxError {
        LoxError {
            inner: LoxErrorInner::InvalidProperty { property, object },
            trace: vec![],
        }
    }

    fn non_existent_super(name: &'static str) -> LoxError {
        LoxError {
            inner: LoxErrorInner::NonExistentSuper(name),
            trace: vec![],
        }
    }

    fn value(value: LoxValue) -> LoxError {
        LoxError {
            inner: LoxErrorInner::Value(Box::new(value)),
            trace: vec![],
        }
    }

    #[cfg(feature = "async")]
    fn finished_coroutine() -> LoxError {
        LoxError {
            inner: LoxErrorInner::FinishedCoroutine,
            trace: vec![],
        }
    }

    #[doc(hidden)] // Not public API.
    #[cold]
    pub fn push_trace(&mut self, value: &'static str) {
        self.trace.push(value);
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
            trace: vec![],
        }
    }

    /// Creates a [`LoxError`] from another error.
    #[cfg(not(feature = "sync"))]
    pub fn external<E: Error + 'static>(value: E) -> LoxError {
        LoxError {
            inner: LoxErrorInner::External(ExternalError(LoxRc::new(value))),
            trace: vec![],
        }
    }
}

impl From<Infallible> for LoxError {
    fn from(value: Infallible) -> Self {
        match value {}
    }
}

#[derive(Debug, Clone, PartialEq)]
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
    Value(Box<LoxValue>),
    External(ExternalError),
    #[cfg(feature = "async")]
    FinishedCoroutine,
}

impl fmt::Display for LoxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "traceback (most recent call last):")?;
        for &fun in &self.trace {
            writeln!(f, "in '{fun}'")?;
        }

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
            LoxErrorInner::Value(value) => {
                write!(f, "error: {value}")
            }
            LoxErrorInner::External(err) => fmt::Display::fmt(&err.0, f),
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
                err.push_trace($var_name);
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
        }) *
    };
}

loxvalue_to_loxvaluetype! { LoxValue, &LoxValue, &mut LoxValue }

/// A result returned from most Lox operations.
pub type LoxResult = Result<LoxValue, LoxError>;

/// A dynamically typed value used by Lox programs.
#[non_exhaustive]
#[derive(Clone)]
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
    PrimitiveMethod(fn(LoxValue) -> LoxResult, Box<LoxValue>),
    /// A class.
    Class(LoxRc<LoxClass>),
    /// An instance of a class.
    Instance(Shared<LoxInstance>),
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
    #[cfg(not(feature = "sync"))]
    External(Shared<dyn LoxObject>),
    /// An object that couldn't normally be represented in Lox.
    #[cfg(feature = "sync")]
    External(Shared<dyn LoxObject + Send + Sync>),
    #[doc(hidden)] // Not public API.
    Undefined(&'static str),
}

impl LoxValue {
    /// Returns the value wrapped by `self` if `self` is a `LoxValue::Bool`.
    pub fn as_bool(&self) -> Option<bool> {
        if let LoxValue::Bool(value) = self {
            Some(*value)
        } else {
            None
        }
    }

    /// Returns the value wrapped by `self` if `self` is a `LoxValue::Str`.
    pub fn as_str(&self) -> Option<LoxRc<String>> {
        if let LoxValue::Str(value) = self {
            Some(LoxRc::clone(value))
        } else {
            None
        }
    }

    /// Returns the value wrapped by `self` if `self` is a `LoxValue::Num`.
    pub fn as_num(&self) -> Option<f64> {
        if let LoxValue::Num(value) = self {
            Some(*value)
        } else {
            None
        }
    }

    /// Returns the value wrapped by `self` if `self` is a `LoxValue::Arr`.
    pub fn as_arr(&self) -> Option<Shared<Vec<LoxValue>>> {
        if let LoxValue::Arr(value) = self {
            Some(LoxRc::clone(value))
        } else {
            None
        }
    }

    /// Returns the value wrapped by `self` if `self` is a `LoxValue::Function`.
    pub fn as_function(&self) -> Option<LoxRc<LoxFn>> {
        if let LoxValue::Function(value) = self {
            Some(LoxRc::clone(value))
        } else {
            None
        }
    }

    /// Returns the value wrapped by `self` if `self` is a `LoxValue::Class`.
    pub fn as_class(&self) -> Option<LoxRc<LoxClass>> {
        if let LoxValue::Class(value) = self {
            Some(LoxRc::clone(value))
        } else {
            None
        }
    }

    /// Returns the value wrapped by `self` if `self` is a `LoxValue::Instance`.
    pub fn as_instance(&self) -> Option<Shared<LoxInstance>> {
        if let LoxValue::Instance(value) = self {
            Some(LoxRc::clone(value))
        } else {
            None
        }
    }

    /// Returns the value wrapped by `self` if `self` is a `LoxValue::Error`.
    pub fn as_error(&self) -> Option<LoxError> {
        if let LoxValue::Error(value) = self {
            Some(value.clone())
        } else {
            None
        }
    }

    /// Returns the value wrapped by `self` if `self` is a
    /// `LoxValue::Coroutine`.
    #[cfg(feature = "async")]
    pub fn as_coroutine(&self) -> Option<LoxRc<Coroutine>> {
        if let LoxValue::Coroutine(value) = self {
            Some(LoxRc::clone(value))
        } else {
            None
        }
    }

    /// Returns the value wrapped by `self` if `self` is a `LoxValue::Future`.
    #[cfg(feature = "async")]
    pub fn as_future(&self) -> Option<Shared<LoxFuture>> {
        if let LoxValue::Future(value) = self {
            Some(LoxRc::clone(value))
        } else {
            None
        }
    }

    /// Gets the `index`th item of `self`, if `self` is an array.
    pub fn index<T: TryInto<f64> + Into<LoxValue> + Clone + fmt::Display>(
        &self,
        index: T,
    ) -> Result<LoxValue, LoxError> {
        let num: f64 = index
            .clone()
            .try_into()
            .map_err(|_| LoxError::type_error(format!("invalid base for index: {index}")))?;
        let output = match self {
            LoxValue::Arr(arr) => {
                let index = num as usize;

                if index as f64 == num {
                    read(arr)[index].clone()
                } else {
                    return Err(LoxError::type_error(format!(
                        "invalid base for index: {}",
                        num
                    )));
                }
            }
            _ => {
                return Err(LoxError::type_error(format!(
                    "cannot index into a value of type {}",
                    LoxValueType::from(self)
                )));
            }
        };

        Ok(output)
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
    fn get_impl(&self, key: &'static str) -> Result<Option<LoxValue>, LoxError> {
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
                ("is_error", primitive_methods::is_error),
                ("is_nil", primitive_methods::is_nil),
            ])
        }

        if let LoxValue::Instance(instance) = self {
            if let Some(attr) = read(instance).attributes.get(key) {
                Ok(Some(attr.clone()))
            } else {
                Ok(read(instance)
                    .class
                    .get(key)
                    .map(|func| LoxValue::BoundMethod(func, instance.clone())))
            }
        } else if let Some(method) = PRIMITIVE_METHODS.get_or_init(init_primitives).get(key) {
            Ok(Some(LoxValue::PrimitiveMethod(
                *method,
                Box::new(self.clone()),
            )))
        } else if let LoxValue::External(object) = self {
            read(object).get(LoxRc::clone(object), key)
        } else {
            Ok(None)
        }
    }

    /// Gets the attribute corresponding to `self.key`, if it exists.
    pub fn get(&self, key: &'static str) -> LoxResult {
        self.get_impl(key)?
            .ok_or(LoxError::invalid_property(key, self.to_string()))
    }

    /// Gets the attribute corresponding to `self.key` to `value`, if it exists.
    pub fn set(&self, key: &'static str, value: LoxValue) -> LoxResult {
        if let LoxValue::Instance(instance) = self {
            write(instance)
                .attributes
                .insert(key.to_string(), value.clone());
            Ok(value)
        } else if let LoxValue::External(object) = self {
            read(object)
                .set(LoxRc::clone(object), key, value)
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
        let instance = self.as_instance()?;
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
            Self::Function(func) => (func.fun)(args),
            Self::BoundMethod(func, instance) => {
                args.head = Some(LoxValue::Instance(instance.clone()));
                func.call(args)
            }
            Self::Class(class) => {
                let instance = LoxValue::Instance(Shared::new(
                    LoxInstance {
                        class: LoxRc::clone(class),
                        attributes: HashMap::new(),
                    }
                    .into(),
                ));
                if let Some(initialiser) = &class.initialiser {
                    args.head = Some(instance.clone());
                    (initialiser.fun)(args)
                } else {
                    Ok(instance)
                }
            }
            Self::PrimitiveMethod(func, object) => func((**object).clone()),
            #[cfg(feature = "async")]
            Self::Coroutine(func) => Ok(LoxValue::Future(Shared::new(func.start(args).into()))),
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
            (Self::Instance(i1), Self::Instance(i2)) => read(i1).deref() == read(&i2).deref(),
            _ => false,
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
                write!(f, "{}", buf)
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

impl_tryfrom_borrowed_loxvalue! { bool, String, f32, f64, u8, u16, u32, u64, u128, i8, i16, i32, i64, i128 }

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
                                    trace: vec![],
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

impl_numeric! { f32, f64, u8, u16, u32, u64, u128, i8, i16, i32, i64, i128 }

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
pub struct LoxFn {
    #[cfg(feature = "sync")]
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

/// An instance of a Lox class.
#[derive(Debug, PartialEq)]
pub struct LoxInstance {
    class: LoxRc<LoxClass>,
    attributes: HashMap<String, LoxValue>,
}

#[derive(Clone)]
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

impl PartialEq for LoxMethod {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (LoxMethod::Sync(f1), LoxMethod::Sync(f2)) => LoxRc::ptr_eq(f1, f2),
            #[cfg(feature = "async")]
            (LoxMethod::Async(f1), LoxMethod::Async(f2)) => LoxRc::ptr_eq(f1, f2),
            #[allow(unreachable_patterns)]
            _ => false,
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
            superclass: Some(superclass.unwrap_or(LoxRc::new(LoxClass {
                name: "object",
                initialiser: None,
                methods: HashMap::new(),
                superclass: None,
            }))),
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
}

/// Arguments to a Lox function.
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
    /// This method should return `Ok(None)` instead of an error if the value is
    /// not found.
    fn get(
        &self,
        this: Shared<DynLoxObject>,
        key: &'static str,
    ) -> Result<Option<LoxValue>, LoxError> {
        let (_, _) = (this, key);
        Ok(None)
    }

    /// Sets the field of `self` corresponding to `key` to the given value.
    ///
    /// If the operation succeeds, the given value should be cloned and
    /// returned.
    fn set(
        &self,
        this: Shared<DynLoxObject>,
        key: &'static str,
        value: LoxValue,
    ) -> Result<LoxValue, Option<LoxError>> {
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
