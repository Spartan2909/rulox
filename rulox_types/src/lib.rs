//! `rulox_types` is a collection of types used by the `rulox` crate to
//! represent dynamically typed values.

#![warn(missing_docs)]
#![allow(clippy::mutable_key_type)]

mod async_types;
pub use async_types::Coroutine;
pub use async_types::LoxFuture;

mod conversions;

mod error;
pub use error::LoxError;

mod functions;
pub use functions::ConcreteLoxArgs;
pub use functions::LoxArgs;
pub use functions::LoxFn;
use functions::LoxMethod;

mod hash;
pub use hash::MapKey;

mod interop;
pub use interop::LoxObject;

mod iteration;
pub use iteration::IntoIter;

#[doc(hidden)]
pub use interop::ToLoxResult;

mod operations;

mod primitive_methods;

#[cfg(feature = "serde")]
mod serialise;
#[cfg(feature = "serde")]
pub use serialise::hashmap_to_json_map;

mod shared;
pub use shared::LoxVariable;
#[doc(hidden)]
pub use shared::Shared;

mod to_tokens;

mod private {
    pub trait Sealed {}
}

use std::cmp;
use std::collections::HashMap;
use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;
use std::future::Future;
use std::pin::Pin;
use std::process::ExitCode;
use std::process::Termination;
use std::ptr;
use std::sync::Arc;
use std::sync::OnceLock;
use std::vec;

use bytes::Bytes;

#[cfg(feature = "serde")]
use serde::Serialize;

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
    Coroutine(Vec<&'static str>),
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
            Self::Function(params) => write!(f, "function({params:#?})"),
            Self::Class => write!(f, "class"),
            Self::Instance(class) => write!(f, "{class}"),
            Self::Map => write!(f, "map"),
            Self::Bytes => write!(f, "bytes"),
            Self::Error => f.write_str("error"),
            Self::Coroutine(params) => write!(f, "async function({params:#?})"),
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
                    LoxValue::Function(f) => Self::Function(f.params().to_vec()),
                    LoxValue::BoundMethod(f, _) => Self::Function(f.params().to_vec()),
                    LoxValue::PrimitiveMethod(_, _) => Self::Function(vec![]),
                    LoxValue::Class(_) => Self::Class,
                    LoxValue::Instance(instance) => Self::Instance(instance.read().class.name.clone()),
                    LoxValue::Map(_) => Self::Map,
                    LoxValue::Bytes(_) => Self::Bytes,
                    LoxValue::Error(_) => Self::Error,
                    LoxValue::Coroutine(f) => Self::Coroutine(f.params().to_vec()),
                    LoxValue::Future(fut) => Self::Future(fut.0.read().done()),
                    LoxValue::External(_) => Self::External,
                    LoxValue::Nil => Self::Nil,
                }
            }
        }
    )* };
}

loxvalue_to_loxvaluetype! { LoxValue, &LoxValue, &mut LoxValue }

/// A result returned from most Lox operations.
pub type LoxResult = Result<LoxValue, LoxError>;

/// A dynamically typed value used by Lox programs.
#[non_exhaustive]
#[derive(Clone)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum LoxValue {
    /// A boolean value.
    Bool(bool),
    /// A string.
    Str(Arc<str>),
    /// A floating-point number.
    Num(f64),
    /// An array of [`LoxValue`]s.
    Arr(Shared<Vec<Self>>),
    /// A function.
    Function(Arc<LoxFn>),
    #[doc(hidden)]
    BoundMethod(LoxMethod, Shared<LoxInstance>),
    #[doc(hidden)]
    #[cfg_attr(
        feature = "serde",
        serde(serialize_with = "serialise::primitive_method")
    )]
    PrimitiveMethod(fn(LoxArgs) -> LoxResult, Box<LoxValue>),
    /// A class.
    Class(Arc<LoxClass>),
    /// An instance of a class.
    Instance(Shared<LoxInstance>),
    /// A set of key-value pairs.
    Map(Shared<HashMap<MapKey, LoxValue>>),
    /// A sequence of bytes.
    Bytes(Bytes),
    /// A wrapped error.
    Error(Box<LoxError>),
    /// An asynchronous function.
    Coroutine(Arc<Coroutine>),
    /// A value returned from an async function.
    Future(LoxFuture),
    /// Nothing.
    Nil,
    /// An object that couldn't normally be represented in Lox.
    #[cfg_attr(feature = "serde", serde(serialize_with = "serialise::external"))]
    External(Shared<dyn LoxObject>),
}

impl LoxValue {
    /// Returns the value wrapped by `self` if `self` is a `LoxValue::Bool`.
    pub fn as_bool(&self) -> Option<bool> {
        self.expect_bool().ok()
    }

    /// Returns the value wrapped by `self` if `self` is a `LoxValue::Bool`.
    ///
    /// ## Errors
    /// Returns a type error if `self` is not a boolean.
    pub fn expect_bool(&self) -> Result<bool, LoxError> {
        if let LoxValue::Bool(value) = self {
            Ok(*value)
        } else {
            Err(LoxError::type_error(format!("{self} is not a boolean")))
        }
    }

    /// Returns the value wrapped by `self` if `self` is a `LoxValue::Str`.
    pub fn as_str(&self) -> Option<&Arc<str>> {
        self.expect_str().ok()
    }

    /// Returns the value wrapped by `self` if `self` is a `LoxValue::Str`.
    ///
    /// ## Errors
    /// Returns a type error if `self` is not a string.
    pub fn expect_str(&self) -> Result<&Arc<str>, LoxError> {
        if let LoxValue::Str(value) = self {
            Ok(value)
        } else {
            Err(LoxError::type_error(format!("{self} is not a string")))
        }
    }

    /// Returns the value wrapped by `self` if `self` is a `LoxValue::Num`.
    pub fn as_num(&self) -> Option<f64> {
        self.expect_num().ok()
    }

    /// Returns the value wrapped by `self` if `self` is a `LoxValue::Num`.
    ///
    /// ## Errors
    /// Returns a type error if `self` is not a number.
    pub fn expect_num(&self) -> Result<f64, LoxError> {
        if let LoxValue::Num(value) = self {
            Ok(*value)
        } else {
            Err(LoxError::type_error(format!("{self} is not a number")))
        }
    }

    /// Returns the value wrapped by `self` if `self` is a `LoxValue::Arr`.
    pub fn as_arr(&self) -> Option<Shared<Vec<LoxValue>>> {
        self.expect_arr().ok()
    }

    /// Returns the value wrapped by `self` if `self` is a `LoxValue::Arr`.
    ///
    /// ## Errors
    /// Returns a type error if `self` is not an array.
    pub fn expect_arr(&self) -> Result<Shared<Vec<LoxValue>>, LoxError> {
        if let LoxValue::Arr(value) = self {
            Ok(value.clone())
        } else {
            Err(LoxError::type_error(format!("{self} is not an array")))
        }
    }

    /// Returns the value wrapped by `self` if `self` is a `LoxValue::Function`.
    pub fn as_function(&self) -> Option<&Arc<LoxFn>> {
        self.expect_function().ok()
    }

    /// Returns the value wrapped by `self` if `self` is a `LoxValue::Function`.
    ///
    /// ## Errors
    /// Returns a type error if `self` is not a function.
    pub fn expect_function(&self) -> Result<&Arc<LoxFn>, LoxError> {
        if let LoxValue::Function(value) = self {
            Ok(value)
        } else {
            Err(LoxError::type_error(format!("{self} is not a function")))
        }
    }

    /// Returns the value wrapped by `self` if `self` is a `LoxValue::Class`.
    pub fn as_class(&self) -> Option<&Arc<LoxClass>> {
        self.expect_class().ok()
    }

    /// Returns the value wrapped by `self` if `self` is a `LoxValue::Class`.
    ///
    /// ## Errors
    /// Returns a type error if `self` is not a class.
    pub fn expect_class(&self) -> Result<&Arc<LoxClass>, LoxError> {
        if let LoxValue::Class(value) = self {
            Ok(value)
        } else {
            Err(LoxError::type_error(format!("{self} is not a class")))
        }
    }

    /// Returns the value wrapped by `self` if `self` is a `LoxValue::Instance`.
    pub fn as_instance(&self) -> Option<&Shared<LoxInstance>> {
        self.expect_instance().ok()
    }

    /// Returns the value wrapped by `self` if `self` is a `LoxValue::Instance`.
    ///
    /// ## Errors
    /// Returns a type error if `self` is not an instance of a class.
    pub fn expect_instance(&self) -> Result<&Shared<LoxInstance>, LoxError> {
        if let LoxValue::Instance(value) = self {
            Ok(value)
        } else {
            Err(LoxError::type_error(format!("{self} is not an instance")))
        }
    }

    /// Returns the value wrapped by `self` if `self` is a `LoxValue::Map`.
    pub fn as_map(&self) -> Option<&Shared<HashMap<MapKey, LoxValue>>> {
        self.expect_map().ok()
    }

    /// Returns the value wrapped by `self` if `self` is a `LoxValue::Map`.
    ///
    /// ## Errors
    /// Returns a type error if `self` is not a map.
    pub fn expect_map(&self) -> Result<&Shared<HashMap<MapKey, LoxValue>>, LoxError> {
        if let LoxValue::Map(value) = self {
            Ok(value)
        } else {
            Err(LoxError::type_error(format!("{self} is not a map")))
        }
    }

    /// Returns the value wrapped by `self` if `self` is a `LoxValue::Bytes`.
    pub fn as_bytes(&self) -> Option<&Bytes> {
        self.expect_bytes().ok()
    }

    /// Returns the value wrapped by `self` if `self` is a `LoxValue::Bytes`.
    ///
    /// ## Errors
    /// Returns a type error if `self` is not a bytestring.
    pub fn expect_bytes(&self) -> Result<&Bytes, LoxError> {
        if let LoxValue::Bytes(value) = self {
            Ok(value)
        } else {
            Err(LoxError::type_error(format!("{self} is not a bytestring")))
        }
    }

    /// Returns the value wrapped by `self` if `self` is a `LoxValue::Error`.
    pub fn as_error(&self) -> Option<&LoxError> {
        self.expect_error().ok()
    }

    /// Returns the value wrapped by `self` if `self` is a `LoxValue::Error`.
    ///
    /// ## Errors
    /// Returns a type error if `self` is not an error.
    pub fn expect_error(&self) -> Result<&LoxError, LoxError> {
        if let LoxValue::Error(value) = self {
            Ok(value)
        } else {
            Err(LoxError::type_error(format!("{self} is not an error")))
        }
    }

    /// Returns the value wrapped by `self` if `self` is a
    /// `LoxValue::Coroutine`.
    pub fn as_coroutine(&self) -> Option<&Arc<Coroutine>> {
        self.expect_coroutine().ok()
    }

    /// Returns the value wrapped by `self` if `self` is a `LoxValue::Coroutine`.
    ///
    /// ## Errors
    /// Returns a type error if `self` is not a coroutine.
    pub fn expect_coroutine(&self) -> Result<&Arc<Coroutine>, LoxError> {
        if let LoxValue::Coroutine(value) = self {
            Ok(value)
        } else {
            Err(LoxError::type_error(format!("{self} is not a coroutine")))
        }
    }

    /// Returns the value wrapped by `self` if `self` is a `LoxValue::Future`.
    pub fn as_future(&self) -> Option<&LoxFuture> {
        self.expect_future().ok()
    }

    /// Returns the value wrapped by `self` if `self` is a `LoxValue::Future`.
    ///
    /// ## Errors
    /// Returns a type error if `self` is not a future.
    pub fn expect_future(&self) -> Result<&LoxFuture, LoxError> {
        if let LoxValue::Future(value) = self {
            Ok(value)
        } else {
            Err(LoxError::type_error(format!("{self} is not a future")))
        }
    }

    /// Returns the value wrapped by `self` if `self` is a `LoxValue::External`.
    ///
    /// ## Errors
    /// Returns a type error if `self` is not an external object.
    pub fn to_external(self) -> Option<Shared<dyn LoxObject>> {
        self.expect_external().ok()
    }

    /// Returns the value wrapped by `self` if `self` is a
    /// `LoxValue::External`.
    ///
    /// ## Errors
    /// Returns a type error if `self` is not an external object.
    pub fn expect_external(self) -> Result<Shared<dyn LoxObject>, LoxError> {
        if let LoxValue::External(obj) = self {
            Ok(obj)
        } else {
            Err(self.as_external_error())
        }
    }
}

impl LoxValue {
    /// Gets the element of `self` corresponding to `index`.
    ///
    /// ## Errors
    /// Returns an error if `self` cannot be indexed, or if the index is invalid.
    pub fn index(&self, index: LoxValue) -> Result<LoxValue, LoxError> {
        match self {
            LoxValue::Arr(arr) => {
                let index = index.clone().try_into().map_err(|_| {
                    LoxError::type_error(format!("invalid base for index: {index}"))
                })?;

                arr.read()
                    .get(index)
                    .cloned()
                    .ok_or_else(|| LoxError::index_out_of_range(index))
            }
            LoxValue::Map(map) => map
                .read()
                .get(&MapKey::verify_key(index.clone())?)
                .cloned()
                .ok_or_else(|| LoxError::invalid_key(index)),
            LoxValue::Instance(instance) => LoxInstance::get(instance, "[]").map_or_else(
                || Err(LoxError::not_implemented("[]", &LoxValueType::from(self))),
                |method| method.call([index].into()),
            ),
            LoxValue::External(external) => external.read().index(index),
            _ => Err(LoxError::type_error(format!(
                "cannot index into a value of type '{}'",
                LoxValueType::from(self)
            ))),
        }
    }

    /// Sets the element of `self` corresponding to `index` to `value`.
    ///
    /// ## Errors
    /// Returns an error if `self` cannot be indexed, or if the index is invalid.
    pub fn index_set(&self, index: LoxValue, value: LoxValue) -> Result<(), LoxError> {
        match self {
            LoxValue::Arr(arr) => {
                let index = index.clone().try_into().map_err(|_| {
                    LoxError::type_error(format!("invalid base for index: {index}"))
                })?;

                let mut arr = arr.write();
                if index < arr.len() {
                    arr[index] = value;
                    drop(arr);
                } else {
                    return Err(LoxError::index_out_of_range(index));
                }

                Ok(())
            }
            LoxValue::Map(map) => {
                map.write().insert(MapKey::verify_key(index)?, value);

                Ok(())
            }
            LoxValue::Instance(instance) => {
                if let Some(method) = LoxInstance::get(instance, "[] =") {
                    method.call([index, value].into())?;
                    Ok(())
                } else {
                    Err(LoxError::not_implemented("[]=", &LoxValueType::from(self)))
                }
            }
            LoxValue::External(external) => external.write().index_set(index, value),
            _ => Err(LoxError::type_error(format!(
                "cannot index into a value of type {}",
                LoxValueType::from(self)
            ))),
        }
    }

    /// Returns `false` if self is `false` or `nil`, and `true` otherwise.
    pub const fn is_truthy(&self) -> bool {
        !matches!(self, LoxValue::Bool(false) | LoxValue::Nil)
    }

    #[doc(hidden)] // Not public API.
    pub fn function(func: LoxFn) -> LoxValue {
        LoxValue::Function(Arc::new(func))
    }

    #[doc(hidden)] // Not public API.
    pub fn map(values: impl IntoIterator<Item = (LoxValue, LoxValue)>) -> LoxResult {
        let map: Result<HashMap<_, _>, LoxError> = values
            .into_iter()
            .map(|(key, value)| Ok((MapKey::verify_key(key)?, value)))
            .collect();
        Ok(LoxValue::Map(Shared::new(map?)))
    }

    #[doc(hidden)] // Not public API.
    pub fn coroutine<
        F: Fn(LoxArgs) -> Pin<Box<dyn Future<Output = LoxResult> + Send + Sync>>
            + Send
            + Sync
            + 'static,
    >(
        fun: F,
        params: Vec<&'static str>,
    ) -> LoxValue {
        LoxValue::Coroutine(Arc::new(async_types::Coroutine::new(Box::new(fun), params)))
    }

    #[inline(always)]
    fn get_impl(&self, key: &str) -> Result<LoxValue, Option<LoxError>> {
        type PrimitiveMethods = HashMap<&'static str, fn(LoxArgs) -> LoxResult>;
        static PRIMITIVE_METHODS: OnceLock<PrimitiveMethods> = OnceLock::new();
        fn init_primitives() -> HashMap<&'static str, fn(LoxArgs) -> LoxResult> {
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
                ("set_default", primitive_methods::set_default),
            ])
        }

        if let LoxValue::Instance(instance) = self {
            LoxInstance::get(instance, key).ok_or(None)
        } else if let LoxValue::External(object) = self {
            object.read().get(object.clone(), key)
        } else if let Some(method) = PRIMITIVE_METHODS.get_or_init(init_primitives).get(key) {
            Ok(LoxValue::PrimitiveMethod(*method, Box::new(self.clone())))
        } else {
            Err(None)
        }
    }

    /// Gets the attribute corresponding to `self.key`.
    ///
    /// ## Errors
    /// Returns an error if the attribute cannot be read.
    pub fn get(&self, key: &str) -> LoxResult {
        self.get_impl(key).map_err(|err| {
            err.unwrap_or_else(|| LoxError::invalid_property(key.to_string(), self.to_string()))
        })
    }

    /// Gets the attribute corresponding to `self.key` to `value`.
    ///
    /// ## Errors
    /// Returns an error if the attribute cannot be written to.
    pub fn set(&self, key: &str, value: LoxValue) -> LoxResult {
        if let LoxValue::Instance(instance) = self {
            instance
                .write()
                .attributes
                .insert(key.to_string(), value.clone());
            Ok(value)
        } else if let LoxValue::External(object) = self {
            object
                .write()
                .set(object.clone(), key, value.clone())
                .map(|()| value)
                .map_err(|err| {
                    err.unwrap_or_else(|| {
                        LoxError::invalid_property(key.to_string(), self.to_string())
                    })
                })
        } else {
            Err(LoxError::invalid_property(
                key.to_string(),
                self.to_string(),
            ))
        }
    }

    #[doc(hidden)] // Not public API.
    pub fn expect_as_superclass(&self) -> Result<&Arc<LoxClass>, LoxError> {
        if let LoxValue::Class(class) = self {
            Ok(class)
        } else {
            Err(LoxError::type_error(format!(
                "Cannot use {self} as a superclass"
            )))
        }
    }

    fn super_fn_impl(&self, name: &str) -> Option<LoxMethod> {
        let instance = self.as_instance()?;
        let mut class = &instance.read().class;
        while !class.methods.contains_key(name) {
            class = class.superclass.as_ref()?;
        }
        Some(class.superclass.as_ref()?.methods.get(name)?.clone())
    }

    #[doc(hidden)] // Not public API.
    pub fn super_fn(&self, name: &str) -> Result<LoxMethod, LoxError> {
        self.super_fn_impl(name)
            .ok_or_else(|| LoxError::non_existent_super(name.to_string()))
    }

    #[doc(hidden)] // Not public API.
    pub fn bind<F: Into<LoxMethod>>(fun: F, instance: &LoxValue) -> LoxValue {
        LoxValue::BoundMethod(fun.into(), instance.as_instance().unwrap().clone())
    }

    #[doc(hidden)] // Not public API.
    pub fn into_error(self) -> LoxError {
        if let LoxValue::Error(err) = self {
            *err
        } else {
            LoxError::value(self)
        }
    }

    /// Calls `self` with the given arguments, if `self` is a function or a
    /// class.
    ///
    /// ## Errors
    /// Returns an error if `self` cannot be called, or the function in `self`
    /// returns an error.
    pub fn call(&self, mut args: LoxArgs) -> LoxResult {
        match self {
            Self::Function(func) => func.call(args),
            Self::BoundMethod(func, instance) => {
                args.head = Some(LoxValue::Instance(instance.clone()));
                func.call(args)
            }
            Self::Class(class) => {
                let instance = LoxValue::Instance(Shared::new(LoxInstance {
                    class: Arc::clone(class),
                    attributes: HashMap::new(),
                }));
                let mut args = args.check_arity(class.arity())?;
                if let Some(initialiser) = &class.initialiser {
                    args.head = Some(instance);
                    LoxMethod::Sync(initialiser.clone()).call(args)
                } else {
                    Ok(instance)
                }
            }
            Self::PrimitiveMethod(func, object) => func(args.with_head((**object).clone())),
            Self::Coroutine(func) => Ok(LoxValue::Future(
                func.start(args.check_arity(func.params().len())?),
            )),
            Self::Instance(instance) => LoxInstance::get(instance, "()").map_or_else(
                || Err(LoxError::not_implemented("()", &LoxValueType::from(self))),
                |method| method.call(args),
            ),
            Self::External(external) => external.read().call(args),
            _ => Err(LoxError::type_error(format!(
                "cannot call value of type '{}'",
                LoxValueType::from(self)
            ))),
        }
    }

    fn as_external_error(&self) -> LoxError {
        LoxError::type_error(format!("cannot cast {self} to an external object"))
    }

    /// Creates a `LoxValue::Arr` from the given array.
    pub fn arr(arr: Vec<LoxValue>) -> LoxValue {
        LoxValue::Arr(Shared::new(arr))
    }

    /// Creates a new `LoxValue::External` with the given value.
    pub fn external<T: LoxObject + Send + Sync + 'static>(value: T) -> LoxValue {
        let shared = Shared::new(value).into_inner();
        let shared = Shared::from(shared as Arc<_>);
        LoxValue::External(shared)
    }
}

impl Debug for LoxValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Bool(b) => write!(f, "Bool({b})"),
            Self::Str(s) => write!(f, "Str({s})"),
            Self::Num(n) => write!(f, "Num({n})"),
            Self::Arr(a) => write!(f, "Arr({a:#?})"),
            Self::Function(func) => write!(f, "Function({:#?})", func.params()),
            Self::BoundMethod(func, instance) => {
                write!(f, "BoundMethod({:#?}, {:#?})", func.params(), instance)
            }
            Self::PrimitiveMethod(_, object) => {
                write!(f, "PrimitiveMethod({object:#?})")
            }
            Self::Class(class) => write!(f, "Class({class:#?})"),
            Self::Instance(instance) => write!(f, "Instance({:#?})", *instance.read()),
            Self::Map(map) => write!(f, "Map({:#?})", *map.read()),
            Self::Bytes(bytes) => write!(f, "Bytes({bytes:#?})"),
            Self::Error(error) => write!(f, "Error({error:#?})"),
            Self::Coroutine(fun) => write!(f, "Coroutine({:#?})", fun.params()),
            Self::Future(_) => write!(f, "Future"),
            Self::Nil => write!(f, "Nil"),
            Self::External(_) => write!(f, "External"),
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
            (Self::Arr(a1), Self::Arr(a2)) => *a1.read() == *a2.read(),
            (Self::Function(f1), Self::Function(f2)) => f1 == &f2,
            (Self::BoundMethod(f1, _), Self::BoundMethod(f2, _)) => f1 == &f2,
            (Self::PrimitiveMethod(f1, _), Self::PrimitiveMethod(f2, _)) => f1 == &f2,
            (Self::Class(c1), Self::Class(c2)) => c1 == &c2,
            (Self::Instance(i1), Self::Instance(i2)) => *i1.read() == *i2.read(),
            (Self::Map(m1), Self::Map(m2)) => *m1.read() == *m2.read(),
            (Self::Bytes(b1), Self::Bytes(b2)) => b1 == &b2,
            (Self::Error(e1), Self::Error(e2)) => e1 == &e2,
            (Self::Coroutine(f1), Self::Coroutine(f2)) => f1 == &f2,
            (Self::Future(f1), Self::Future(f2)) => *f1.0.read() == *f2.0.read(),
            (Self::Nil, Self::Nil) => true,
            (Self::External(e1), Self::External(e2)) => ptr::eq(e1.as_ptr(), e2.as_ptr()),
            _ => false,
        }
    }
}

impl Display for LoxValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Bool(value) => {
                write!(f, "{value}")
            }
            Self::Str(string) => f.write_str(string),
            Self::Num(value) => {
                write!(f, "{value}")
            }
            Self::Arr(values) => {
                let mut buf = "[".to_string();
                let values = values.read();
                for value in values.iter().take(values.len() - 1) {
                    buf += &(value.to_string() + ", ");
                }
                if let Some(value) = values.last() {
                    buf += &value.to_string();
                }
                drop(values);

                buf += "]";
                f.write_str(&buf)
            }
            Self::Function(_) => {
                write!(f, "<function>")
            }
            Self::BoundMethod(_, _) | Self::PrimitiveMethod(_, _) => write!(f, "<bound method>"),
            Self::Class(_) => write!(f, "<class>"),
            Self::Instance(instance) => {
                write!(f, "<instance of {}>", instance.read().class.name)
            }
            Self::Map(map) => {
                let mut buf = "{\n".to_string();
                for (key, value) in &*map.read() {
                    buf.push_str("    \n");
                    buf += &(key.as_inner().to_string() + ": ");
                    buf += &value.to_string();
                }
                buf.push('}');
                f.write_str(&buf)
            }
            Self::Bytes(bytes) => write!(f, "{bytes:?}"),
            Self::Error(error) => write!(f, "{error}"),
            Self::Coroutine(_) => write!(f, "<async function>"),
            Self::Future(_) => write!(f, "<future>"),
            Self::Nil => {
                write!(f, "nil")
            }
            Self::External(external) => f.write_str(&external.read().representation()),
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
            (Self::Arr(a1), Self::Arr(a2)) => a1.read().partial_cmp(&a2.read()),
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

/// An instance of a Lox class.
#[derive(Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct LoxInstance {
    class: Arc<LoxClass>,
    attributes: HashMap<String, LoxValue>,
}

impl LoxInstance {
    /// Checks if `self` is an instance of `target_class` or one of its
    /// subclasses.
    #[allow(clippy::assigning_clones)] // Not accepted by borrow checker.
    pub fn instance_of(&self, target_class: &LoxClass) -> bool {
        let mut current_class = Some(self.class.clone());
        while let Some(class) = current_class {
            if &*class == target_class {
                return true;
            }
            current_class = class.superclass.clone();
        }
        false
    }

    fn get(this: &Shared<LoxInstance>, key: &str) -> Option<LoxValue> {
        let lock = this.read();
        if let Some(attr) = lock.attributes.get(key) {
            Some(attr.clone())
        } else {
            Some(LoxValue::BoundMethod(lock.class.get(key)?, this.clone()))
        }
    }
}

/// A class defined in Lox code.
#[derive(Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct LoxClass {
    name: &'static str,
    initialiser: Option<Arc<LoxFn>>,
    methods: HashMap<&'static str, LoxMethod>,
    superclass: Option<Arc<LoxClass>>,
}

impl LoxClass {
    #[doc(hidden)] // Not public API.
    pub fn new(
        name: &'static str,
        methods: HashMap<&'static str, LoxMethod>,
        superclass: Option<Arc<LoxClass>>,
    ) -> LoxClass {
        let mut class = LoxClass {
            name,
            initialiser: None,
            methods,
            superclass: Some(superclass.unwrap_or_else(|| {
                let instance_of = |args: LoxArgs| -> LoxResult {
                    let this = args.get(0).unwrap().as_instance().unwrap();
                    let target_class = args.get(0).unwrap().expect_class()?.clone();
                    let is_instance = this.read().instance_of(&target_class);

                    Ok(LoxValue::Bool(is_instance))
                };
                Arc::new(LoxClass {
                    name: "object",
                    initialiser: None,
                    methods: HashMap::from_iter([(
                        "instance_of",
                        LoxMethod::Sync(Arc::new(LoxFn::new(Box::new(instance_of), vec!["class"]))),
                    )]),
                    superclass: None,
                })
            })),
        };

        class.initialiser = class.get("init").and_then(LoxMethod::get_sync);

        class
    }

    fn get(&self, key: &str) -> Option<LoxMethod> {
        self.methods.get(key).map_or_else(
            || {
                self.superclass
                    .as_ref()
                    .and_then(|superclass| superclass.get(key))
            },
            |method| Some(method.clone()),
        )
    }

    fn arity(&self) -> usize {
        self.initialiser
            .as_ref()
            .map_or(0, |initialiser| initialiser.params().len() - 1)
    }
}

#[cfg(test)]
mod tests;
