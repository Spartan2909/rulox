//! `rulox_types` is a collection of types used by the `rulox` crate to
//! represent dynamically typed values.

#![warn(missing_docs)]

#[cfg_attr(feature = "sync", path = "sync.rs")]
#[cfg_attr(not(feature = "sync"), path = "unsync.rs")]
mod shared;
pub use shared::LoxVariable;
use shared::Shared;

mod to_tokens;

use std::rc::Rc;

use std::cmp;
use std::collections::HashMap;
use std::error::Error;
use std::fmt;
use std::mem;
use std::ops;
use std::process::ExitCode;
use std::process::Termination;
use std::ptr;
use std::vec;

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
                    LoxValue::BoundMethod(f, _) => Self::Function(f.params.to_vec()),
                    LoxValue::Class(_) => Self::Class,
                    LoxValue::Instance(instance) => Self::Instance(instance.read().class.name.clone()),
                    LoxValue::Error(_) => Self::Error,
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
    Str(Rc<String>),
    /// A floating-point number.
    Num(f64),
    /// An array of [`LoxValue`]s.
    Arr(Shared<Vec<Self>>),
    /// A function.
    Function(Rc<LoxFn>),
    #[doc(hidden)]
    BoundMethod(Rc<LoxFn>, Shared<LoxInstance>),
    /// A class.
    Class(Rc<LoxClass>),
    /// An instance of a class.
    Instance(Shared<LoxInstance>),
    /// A wrapped error.
    Error(LoxError),
    /// Nothing.
    Nil,
    #[doc(hidden)] // Not public API.
    Undefined(&'static str),
}

impl LoxValue {
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
                    arr.read()[index].clone()
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
        LoxValue::Function(Rc::new(func))
    }

    fn get_impl(&self, key: &str) -> Option<LoxValue> {
        if let LoxValue::Instance(instance) = self {
            if let Some(attr) = instance.read().attributes.get(key) {
                Some(attr.clone())
            } else {
                instance
                    .read()
                    .class
                    .get(key)
                    .map(|func| LoxValue::BoundMethod(func, instance.clone()))
            }
        } else {
            None
        }
    }

    /// Gets the attribute corresponding to `self.key`, if it exists.
    pub fn get(&self, key: &'static str) -> LoxResult {
        self.get_impl(key)
            .ok_or(LoxError::invalid_property(key, self.to_string()))
    }

    /// Gets the attribute corresponding to `self.key` to `value`, if it exists.
    pub fn set(&self, key: &'static str, value: LoxValue) -> LoxResult {
        if let LoxValue::Instance(instance) = self {
            instance
                .write()
                .attributes
                .insert(key.to_string(), value.clone());
            Ok(value)
        } else {
            Err(LoxError::invalid_property(key, self.to_string()))
        }
    }

    fn as_instance(&self) -> Option<Shared<LoxInstance>> {
        if let LoxValue::Instance(instance) = self {
            Some(instance.clone())
        } else {
            None
        }
    }

    #[doc(hidden)] // Not public API.
    pub fn as_class(&self) -> Result<&Rc<LoxClass>, LoxError> {
        if let LoxValue::Class(class) = self {
            Ok(class)
        } else {
            Err(LoxError::type_error(format!(
                "Cannot use {self} as a superclass"
            )))
        }
    }

    fn super_fn_impl(&self, name: &'static str) -> Option<Rc<LoxFn>> {
        let instance = self.as_instance()?;
        let mut class = &instance.read().class;
        while class.methods.get(name).is_none() {
            class = class.superclass.as_ref()?;
        }
        Some(Rc::clone(class.superclass.as_ref()?.methods.get(name)?))
    }

    #[doc(hidden)] // Not public API.
    pub fn super_fn(&self, name: &'static str) -> Result<Rc<LoxFn>, LoxError> {
        self.super_fn_impl(name)
            .ok_or(LoxError::non_existent_super(name))
    }

    #[doc(hidden)] // Not public API.
    pub fn bind(fun: Rc<LoxFn>, instance: LoxValue) -> LoxValue {
        LoxValue::BoundMethod(fun, instance.as_instance().unwrap())
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
                (func.fun)(args)
            }
            Self::Class(class) => {
                let instance = LoxValue::Instance(Shared::new(LoxInstance {
                    class: Rc::clone(class),
                    attributes: HashMap::new(),
                }));
                if let Some(initialiser) = &class.initialiser {
                    args.head = Some(instance.clone());
                    (initialiser.fun)(args)
                } else {
                    Ok(instance)
                }
            }
            _ => panic!("cannot call value of type {}", LoxValueType::from(self)),
        }
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
                write!(f, "BoundMethod({:#?}, {:#?}", func.params, instance)
            }
            Self::Class(class) => write!(f, "Class({:#?})", class),
            Self::Instance(instance) => write!(f, "Instance({:#?})", instance.read()),
            Self::Error(error) => write!(f, "Error({:#?})", error),
            Self::Nil => write!(f, "Nil"),
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
            (Self::Arr(a1), Self::Arr(a2)) => a1 == &a2,
            (Self::Instance(i1), Self::Instance(i2)) => i1 == &i2,
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
                for value in values.read().iter().take(values.read().len() - 1) {
                    buf += &(value.to_string() + ", ");
                }
                if let Some(value) = values.read().last() {
                    buf += &value.to_string();
                }

                buf += "]";
                write!(f, "{}", buf)
            }
            Self::Function(_) => {
                write!(f, "<function>")
            }
            Self::BoundMethod(_, _) => write!(f, "<bound method>"),
            Self::Class(_) => write!(f, "<class>"),
            Self::Instance(instance) => {
                write!(f, "<instance of {}>", instance.read().class.name)
            }
            Self::Error(error) => write!(f, "{error}"),
            Self::Nil => {
                write!(f, "nil")
            }
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
        Self::Arr(values.into())
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
                Ok(LoxValue::Str(Rc::new(s1.to_string() + s2)))
            }
            (LoxValue::Num(n1), &LoxValue::Num(n2)) => Ok(LoxValue::Num(n1 + n2)),
            (LoxValue::Arr(arr1), LoxValue::Arr(ref arr2)) => {
                arr1.write().append(&mut arr2.write());
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
            (Self::Arr(a1), Self::Arr(a2)) => a1.partial_cmp(&a2),
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
            Self::Arr(arr) => LoxIterator::Array(arr.read().clone().into_iter()),
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
    // NOTE: Performance is sensitive to the exact formulation here
    let init = utf8_first_byte(x, 2);
    // SAFETY: `bytes` produces an UTF-8-like string,
    // so the iterator must produce a value here.
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
    fun: Box<dyn Fn(LoxArgs) -> LoxResult>,
    params: Vec<&'static str>,
}

impl LoxFn {
    #[doc(hidden)] // Not public API.
    pub fn new<F: Fn(LoxArgs) -> LoxResult + 'static>(fun: F, params: Vec<&'static str>) -> Self {
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
    class: Rc<LoxClass>,
    attributes: HashMap<String, LoxValue>,
}

/// A class defined in Lox code.
#[derive(Debug, PartialEq)]
pub struct LoxClass {
    name: &'static str,
    initialiser: Option<Rc<LoxFn>>,
    methods: HashMap<&'static str, Rc<LoxFn>>,
    superclass: Option<Rc<LoxClass>>,
}

impl LoxClass {
    #[doc(hidden)] // Not public API.
    pub fn new(
        name: &'static str,
        methods: HashMap<&'static str, Rc<LoxFn>>,
        superclass: Option<Rc<LoxClass>>,
    ) -> LoxClass {
        let mut class = LoxClass {
            name,
            initialiser: methods.get("init").cloned(),
            methods,
            superclass,
        };

        if class.initialiser.is_none() {
            class.initialiser = class.get("init");
        }

        class
    }

    fn get(&self, key: &str) -> Option<Rc<LoxFn>> {
        if let Some(method) = self.methods.get(key) {
            Some(Rc::clone(method))
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

    #[doc(hidden)] // Not public API.
    pub fn drain(&mut self) -> Drain {
        Drain {
            head: &mut self.head,
            main: self.main.drain(..),
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

#[doc(hidden)] // Not public API.
pub struct Drain<'a> {
    head: &'a mut Option<LoxValue>,
    main: vec::Drain<'a, LoxValue>,
}

impl Iterator for Drain<'_> {
    type Item = LoxValue;

    fn next(&mut self) -> Option<Self::Item> {
        if self.head.is_some() {
            self.head.take()
        } else {
            self.main.next()
        }
    }
}

#[cfg(test)]
mod tests;
