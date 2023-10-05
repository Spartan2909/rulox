//! `rulox_types` is a collection of types used by the `rulox` crate
//! to represent dynamically typed values.

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

/// An error that occurred when attempting to use a LoxValue in an invalid location.
#[derive(Debug, Clone)]
pub enum LoxError {
    /// An error that occurred when attempting to use a LoxValue with an invalid type.
    TypeError(String),
    /// An error that occurred when attempting to convert a LoxValue into a Rust type that is too small.
    SizeError { found: usize },
}

impl fmt::Display for LoxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::TypeError(message) => {
                write!(f, "{}", message)
            }
            Self::SizeError { found } => {
                write!(
                    f,
                    "could not convert LoxValue to value of size {:?} (LoxValue too large)",
                    found
                )
            }
        }
    }
}

impl Error for LoxError {}

/// A potential error returned by Lox code.
pub type LoxResult<T> = Result<T, LoxError>;

/// Gets the value from a LoxResult, and panics if it is an error.
/// # Examples
/// ```
/// # use rulox_types::*;
/// let v = LoxValue::from(5) + LoxValue::from(3);
/// assert_eq!(extract(v), LoxValue::from(8));
/// ```
/// # Panics
/// If the contained value is an error.
pub fn extract<T, E>(result: Result<T, E>) -> T
where
    E: ToString,
{
    match result {
        Ok(value) => value,
        Err(e) => panic!("{}", e.to_string()),
    }
}

/// An enum used for error reporting.
#[derive(Debug, Clone)]
pub enum LoxValueType {
    Bool,
    Str,
    Num,
    Arr,
    Function(Vec<&'static str>),
    Class,
    Instance(&'static str),
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
                    LoxValue::Nil => Self::Nil,
                }
            }
        }) *
    };
}

loxvalue_to_loxvaluetype! { LoxValue, &LoxValue, &mut LoxValue }

/// A dynamically typed value used by Lox programs.
#[derive(Clone)]
pub enum LoxValue {
    Bool(bool),
    Str(Rc<String>),
    Num(f64),
    Arr(Shared<Vec<Self>>),
    Function(Rc<LoxFn>),
    BoundMethod(Rc<LoxFn>, Shared<LoxInstance>),
    Class(Rc<LoxClass>),
    Instance(Shared<LoxInstance>),
    Nil,
}

impl LoxValue {
    #[inline(always)]
    fn index_internal<T: TryInto<f64> + Into<LoxValue>>(&self, value: T) -> Option<LoxValue> {
        let num: f64 = value.try_into().ok()?;
        let output = match self {
            LoxValue::Arr(arr) => {
                let index = num as usize;

                if index as f64 == num {
                    arr.read()[index].clone()
                } else {
                    panic!("invalid base for index: {}", num);
                }
            }
            _ => {
                return None;
            }
        };

        Some(output)
    }

    pub fn index<T: TryInto<f64> + Into<LoxValue> + Clone>(&self, value: T) -> LoxValue {
        if let Some(value) = self.index_internal(value.clone()) {
            value
        } else {
            let other: LoxValue = value.into();
            panic!(
                "cannot index {} with {}",
                LoxValueType::from(self),
                LoxValueType::from(other),
            )
        }
    }

    pub fn is_truthy(&self) -> bool {
        !matches!(self, LoxValue::Bool(false) | LoxValue::Nil)
    }

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

    pub fn get(&self, key: &str) -> LoxValue {
        self.get_impl(key)
            .unwrap_or_else(|| panic!("{self} has no attribute '{key}'"))
    }

    pub fn set(&self, key: &str, value: LoxValue) -> LoxValue {
        if let LoxValue::Instance(instance) = self {
            instance
                .write()
                .attributes
                .insert(key.to_string(), value.clone());
            value
        } else {
            panic!("cannot set attribute of {self}")
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
    pub fn as_class(&self) -> Option<&Rc<LoxClass>> {
        if let LoxValue::Class(class) = self {
            Some(class)
        } else {
            None
        }
    }

    pub fn super_fn(&self, name: &str) -> Option<Rc<LoxFn>> {
        let instance = self.as_instance()?;
        let mut class = &instance.read().class;
        while class.methods.get(name).is_none() {
            class = class.superclass.as_ref()?;
        }
        Some(Rc::clone(class.superclass.as_ref()?.methods.get(name)?))
    }

    #[doc(hidden)] // Not public API.
    pub fn super_fn_old(&self, name: &str) -> Option<Rc<LoxFn>> {
        if let LoxValue::BoundMethod(_, instance) = self {
            instance
                .read()
                .class
                .superclass
                .as_ref()
                .and_then(|class| class.get(name))
        } else {
            None
        }
    }

    #[doc(hidden)] // Not public API.
    pub fn bind(fun: Rc<LoxFn>, instance: LoxValue) -> LoxValue {
        LoxValue::BoundMethod(fun, instance.as_instance().unwrap())
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
            Self::Instance(_) => todo!(),
            Self::Nil => write!(f, "Nil"),
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
            Self::Nil => {
                write!(f, "nil")
            }
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

impl TryFrom<LoxValue> for bool {
    type Error = LoxError;

    fn try_from(value: LoxValue) -> Result<Self, Self::Error> {
        if let LoxValue::Bool(b) = value {
            Ok(b)
        } else {
            Err(LoxError::TypeError(format!(
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
            Err(LoxError::TypeError(format!(
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

            fn try_from(value: LoxValue) -> LoxResult<Self> {
                match value {
                    LoxValue::Num(ref num) => {
                        if *num > Self::MAX as f64 {
                            Err(LoxError::SizeError{
                                found: mem::size_of::<Self>()
                            })
                        } else {
                            Ok(*num as Self)
                        }
                    },
                    _ => Err(LoxError::TypeError(format!("expected number, found {}", LoxValueType::from(value))))
                }
            }
        }

        impl ops::Add<$t> for LoxValue {
            type Output = LoxResult<Self>;

            fn add(self, rhs: $t) -> Self::Output {
                match self {
                    Self::Num(num) => Ok(Self::Num(num + rhs as f64)),
                    _ => Err(LoxError::TypeError(format!("cannot add number to {}", LoxValueType::from(self)))),
                }
            }
        }

        impl ops::Sub<$t> for LoxValue {
            type Output = LoxResult<Self>;

            fn sub(self, rhs: $t) -> Self::Output {
                match self {
                    Self::Num(num) => Ok(Self::Num(num - rhs as f64)),
                    _ => Err(LoxError::TypeError(format!("cannot subtract number from {}", LoxValueType::from(self)))),
                }
            }
        }

        impl ops::Mul<$t> for LoxValue {
            type Output = LoxResult<Self>;

            fn mul(self, rhs: $t) -> Self::Output {
                match self {
                    Self::Num(num) => Ok(Self::Num(num * rhs as f64)),
                    _ => Err(LoxError::TypeError(format!("cannot multiply {} by number", LoxValueType::from(self)))),
                }
            }
        }

        impl ops::Div<$t> for LoxValue {
            type Output = LoxResult<Self>;

            fn div(self, rhs: $t) -> Self::Output {
                match self {
                    Self::Num(num) => Ok(Self::Num(num / rhs as f64)),
                    _ => Err(LoxError::TypeError(format!("cannot divide {} by number", LoxValueType::from(self)))),
                }
            }
        }
    ) *
    };
}

impl_numeric! { f32, f64, u8, u16, u32, u64, u128, i8, i16, i32, i64, i128 }

impl ops::Add for LoxValue {
    type Output = LoxResult<Self>;

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
            _ => Err(LoxError::TypeError(format!(
                "cannot add {} to {}",
                LoxValueType::from(rhs),
                self_type,
            ))),
        }
    }
}

impl ops::Add<&str> for LoxValue {
    type Output = LoxResult<Self>;

    fn add(self, rhs: &str) -> Self::Output {
        let other: LoxValue = rhs.into();
        self + other
    }
}

impl ops::Sub for LoxValue {
    type Output = LoxResult<Self>;

    fn sub(self, rhs: Self) -> Self::Output {
        if let (&Self::Num(num1), &Self::Num(num2)) = (&self, &rhs) {
            Ok(LoxValue::Num(num1 - num2))
        } else {
            Err(LoxError::TypeError(format!(
                "cannot subtract {} from {}",
                LoxValueType::from(rhs),
                LoxValueType::from(self),
            )))
        }
    }
}

impl ops::Mul for LoxValue {
    type Output = LoxResult<Self>;

    fn mul(self, rhs: Self) -> Self::Output {
        if let (&Self::Num(num1), &Self::Num(num2)) = (&self, &rhs) {
            Ok(LoxValue::Num(num1 * num2))
        } else {
            Err(LoxError::TypeError(format!(
                "cannot multiply {} by {}",
                LoxValueType::from(self),
                LoxValueType::from(rhs),
            )))
        }
    }
}

impl ops::Div for LoxValue {
    type Output = LoxResult<Self>;

    fn div(self, rhs: Self) -> Self::Output {
        if let (&Self::Num(num1), &Self::Num(num2)) = (&self, &rhs) {
            Ok(LoxValue::Num(num1 / num2))
        } else {
            Err(LoxError::TypeError(format!(
                "cannot divide {} by {}",
                LoxValueType::from(self),
                LoxValueType::from(rhs),
            )))
        }
    }
}

impl ops::Neg for LoxValue {
    type Output = LoxResult<Self>;

    fn neg(self) -> Self::Output {
        match self {
            Self::Num(num) => Ok(Self::Num(-num)),
            _ => Err(LoxError::TypeError(format!(
                "cannot negate {}",
                LoxValueType::from(self)
            ))),
        }
    }
}

impl ops::Not for LoxValue {
    type Output = LoxResult<Self>;

    fn not(self) -> Self::Output {
        match self {
            Self::Bool(b) => Ok(Self::Bool(!b)),
            _ => Err(LoxError::TypeError(format!(
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
        match self {
            Self::Bool(b1) => {
                if let Self::Bool(b2) = other {
                    b1.partial_cmp(&b2)
                } else {
                    None
                }
            }
            Self::Num(n1) => {
                if let Self::Num(n2) = other {
                    n1.partial_cmp(&n2)
                } else {
                    None
                }
            }
            Self::Str(s1) => {
                if let Self::Str(s2) = other {
                    s1.partial_cmp(&s2)
                } else {
                    None
                }
            }
            Self::Arr(a1) => {
                if let Self::Arr(a2) = other {
                    a1.partial_cmp(&a2)
                } else {
                    None
                }
            }
            Self::Function(_) => None,
            Self::BoundMethod(_, _) => None,
            Self::Class(_) => None,
            Self::Instance(_) => None,
            Self::Nil => None,
        }
    }
}

impl Termination for LoxValue {
    fn report(self) -> ExitCode {
        match self {
            Self::Num(n) => ExitCode::from(n as u8),
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

pub enum LoxIterator {
    Array(vec::IntoIter<LoxValue>),
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

pub struct LoxFn {
    fun: Box<dyn Fn(LoxArgs) -> LoxValue>,
    params: Vec<&'static str>,
}

impl LoxFn {
    #[doc(hidden)] // Not public API.
    pub fn new<F: Fn(LoxArgs) -> LoxValue + 'static>(fun: F, params: Vec<&'static str>) -> Self {
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

pub struct LoxArgs {
    head: Option<LoxValue>,
    main: Vec<LoxValue>,
}

impl LoxArgs {
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

pub trait LoxCallable {
    fn lox_call(&self, args: LoxArgs) -> LoxValue;
}

impl LoxCallable for LoxValue {
    fn lox_call(&self, mut args: LoxArgs) -> LoxValue {
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
                    instance
                }
            }
            _ => panic!("cannot call value of type {}", LoxValueType::from(self)),
        }
    }
}

#[cfg(test)]
mod tests;
