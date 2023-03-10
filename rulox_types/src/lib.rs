#![feature(fn_traits, unboxed_closures)]

//! `rulox_types` is a collection of types used by the `rulox` crate
//! to represent dynamically typed values.

use std::{
    collections::HashMap,
    error::Error,
    fmt,
    mem::size_of,
    ops::{Add, BitAnd, BitOr, Div, Index, Mul, Neg, Not, Sub},
};

use proc_macro2::{Punct, Spacing, TokenStream};
use quote::{quote, ToTokens, TokenStreamExt};

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
    Char,
    Str,
    Num,
    Arr,
    Function(Vec<String>),
    Instance(String),
    Nil,
}

impl fmt::Display for LoxValueType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Bool => write!(f, "bool"),
            Self::Char => write!(f, "char"),
            Self::Str => write!(f, "string"),
            Self::Num => write!(f, "number"),
            Self::Arr => write!(f, "array"),
            Self::Function(params) => write!(f, "function({:#?})", params),
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
                    LoxValue::Char(_) => Self::Char,
                    LoxValue::Str(_) => Self::Str,
                    LoxValue::Num(_) => Self::Num,
                    LoxValue::Arr(_) => Self::Arr,
                    LoxValue::Function(f) => Self::Function(f.params.to_vec()),
                    LoxValue::Instance(instance) => Self::Instance(instance.class.clone()),
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
    Char(char),
    Str(Vec<Self>),
    Num(f64),
    Arr(Vec<Self>),
    Function(LoxFn),
    //Function(Box<dyn LoxFn(Vec<LoxValue>) -> LoxValue>, Vec<String>),
    Instance(LoxInstance),
    Nil,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LoxFn {
    ptr: fn(Vec<LoxValue>) -> LoxValue,
    params: Vec<String>,
    method: bool,
}

impl LoxFn {
    pub fn new(ptr: fn(Vec<LoxValue>) -> LoxValue, params: Vec<String>, method: bool) -> Self {
        Self {
            ptr,
            params,
            method,
        }
    }
}

/// An instance of a Lox class.
#[derive(Debug, Clone, PartialEq)]
pub struct LoxInstance {
    class: String,
    attributes: HashMap<String, LoxValue>,
    methods: Vec<String>,
}

impl fmt::Debug for LoxValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Bool(b) => write!(f, "Bool({b})"),
            Self::Char(c) => write!(f, "Char({c}"),
            Self::Str(s) => {
                let mut text = String::new();

                for character in s {
                    text.push(character.try_into().unwrap())
                }

                write!(f, "Str({text})")
            }
            Self::Num(n) => write!(f, "Num({n})"),
            Self::Arr(a) => write!(f, "Arr({:#?})", a),
            Self::Function(func) => write!(f, "Function({:#?})", func.params),
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

        match self {
            Self::Bool(b1) => {
                if let Self::Bool(b2) = other {
                    b1 == &b2
                } else {
                    false
                }
            }
            Self::Char(c1) => {
                if let Self::Char(c2) = other {
                    c1 == &c2
                } else {
                    false
                }
            }
            Self::Str(s1) => {
                if let Self::Str(s2) = other {
                    s1 == &s2
                } else {
                    false
                }
            }
            Self::Num(n1) => {
                if let Self::Num(n2) = other {
                    n1 == &n2
                } else {
                    false
                }
            }
            Self::Arr(a1) => {
                if let Self::Arr(a2) = other {
                    a1 == &a2
                } else {
                    false
                }
            }
            Self::Function(..) => false,
            Self::Instance(i1) => {
                if let Self::Instance(i2) = other {
                    i1 == &i2
                } else {
                    false
                }
            }
            Self::Nil => false,
        }
    }
}

impl fmt::Display for LoxValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Bool(value) => {
                write!(f, "{}", value)
            }
            Self::Char(value) => {
                write!(f, "{}", value)
            }
            Self::Str(chars) => {
                let mut value = "".to_string();

                for c in chars {
                    value.push(c.clone().try_into().unwrap())
                }

                write!(f, "{}", value)
            }
            Self::Num(value) => {
                write!(f, "{}", value)
            }
            Self::Arr(values) => {
                let mut buf = "[".to_string();
                for value in values {
                    buf += &(value.to_string() + ", ");
                }

                // Find a better way of doing this
                buf.pop();
                buf.pop();

                buf += "]";
                write!(f, "{}", buf)
            }
            Self::Function(..) => {
                write!(f, "function")
            }
            Self::Instance(instance) => {
                write!(f, "<Instance of {}>", instance.class)
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
        Self::Char(value)
    }
}

impl From<Vec<char>> for LoxValue {
    fn from(chars: Vec<char>) -> Self {
        let mut values = vec![];
        for c in chars {
            values.push(LoxValue::Char(c));
        }
        Self::Str(values)
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
        Self::Arr(values)
    }
}

impl<const N: usize> From<[LoxValue; N]> for LoxValue {
    fn from(values: [LoxValue; N]) -> Self {
        Self::Arr(Vec::from(values))
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

impl TryFrom<LoxValue> for char {
    type Error = LoxError;

    fn try_from(value: LoxValue) -> Result<Self, Self::Error> {
        if let LoxValue::Char(c) = value {
            Ok(c)
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
        if let LoxValue::Str(chars) = value {
            let mut value = "".to_string();

            for c in chars {
                value.push(c.try_into().unwrap());
            }

            Ok(value)
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

impl_tryfrom_borrowed_loxvalue! { bool, char, String, f32, f64, u8, u16, u32, u64, u128, i8, i16, i32, i64, i128 }

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
                                found: size_of::<Self>()
                            })
                        } else {
                            Ok(*num as Self)
                        }
                    },
                    _ => Err(LoxError::TypeError(format!("expected number, found {}", LoxValueType::from(value))))
                }
            }
        }

        impl Add<$t> for LoxValue {
            type Output = LoxResult<Self>;

            fn add(self, rhs: $t) -> Self::Output {
                match self {
                    Self::Num(num) => Ok(Self::Num(num + rhs as f64)),
                    _ => Err(LoxError::TypeError(format!("cannot add number to {}", LoxValueType::from(self)))),
                }
            }
        }

        impl Sub<$t> for LoxValue {
            type Output = LoxResult<Self>;

            fn sub(self, rhs: $t) -> Self::Output {
                match self {
                    Self::Num(num) => Ok(Self::Num(num - rhs as f64)),
                    _ => Err(LoxError::TypeError(format!("cannot subtract number from {}", LoxValueType::from(self)))),
                }
            }
        }

        impl Mul<$t> for LoxValue {
            type Output = LoxResult<Self>;

            fn mul(self, rhs: $t) -> Self::Output {
                match self {
                    Self::Num(num) => Ok(Self::Num(num * rhs as f64)),
                    _ => Err(LoxError::TypeError(format!("cannot multiply {} by number", LoxValueType::from(self)))),
                }
            }
        }

        impl Div<$t> for LoxValue {
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

impl Add for LoxValue {
    type Output = LoxResult<Self>;

    fn add(self, rhs: Self) -> Self::Output {
        match self {
            LoxValue::Str(s) => {
                if let LoxValue::Str(other) = rhs {
                    let mut result = s.clone();
                    result.extend(other);
                    Ok(LoxValue::Str(result))
                } else {
                    Err(LoxError::TypeError(format!(
                        "cannot add {} to string",
                        LoxValueType::from(rhs)
                    )))
                }
            }
            LoxValue::Num(n) => {
                if let LoxValue::Num(other) = rhs {
                    Ok(LoxValue::from(n + other))
                } else {
                    Err(LoxError::TypeError(format!(
                        "cannot add {} to number",
                        LoxValueType::from(rhs)
                    )))
                }
            }
            LoxValue::Arr(values) => {
                if let LoxValue::Arr(other) = rhs {
                    let mut result = values.clone();
                    result.extend(other);
                    Ok(LoxValue::from(result))
                } else {
                    Err(LoxError::TypeError(format!(
                        "cannot add {} to array",
                        LoxValueType::from(rhs)
                    )))
                }
            }
            _ => Err(LoxError::TypeError(format!(
                "cannot add {} to {}",
                LoxValueType::from(rhs),
                LoxValueType::from(self)
            ))),
        }
    }
}

impl Add<&str> for LoxValue {
    type Output = LoxResult<Self>;

    fn add(self, rhs: &str) -> Self::Output {
        let other: LoxValue = rhs.into();
        self + other
    }
}

impl Sub for LoxValue {
    type Output = LoxResult<Self>;

    fn sub(self, rhs: Self) -> Self::Output {
        if let Self::Num(num1) = self {
            if let Self::Num(num2) = rhs {
                Ok(LoxValue::from(num1 - num2))
            } else {
                Err(LoxError::TypeError(format!(
                    "cannot subtract {} from number",
                    LoxValueType::from(rhs)
                )))
            }
        } else {
            Err(LoxError::TypeError(format!(
                "cannot subtract {} from {}",
                LoxValueType::from(rhs),
                LoxValueType::from(self)
            )))
        }
    }
}

impl Mul for LoxValue {
    type Output = LoxResult<Self>;

    fn mul(self, rhs: Self) -> Self::Output {
        if let Self::Num(num1) = self {
            if let Self::Num(num2) = rhs {
                Ok(LoxValue::from(num1 * num2))
            } else {
                Err(LoxError::TypeError(format!(
                    "cannot multiply number by {}",
                    LoxValueType::from(rhs)
                )))
            }
        } else {
            Err(LoxError::TypeError(format!(
                "cannot multiply {} by {}",
                LoxValueType::from(self),
                LoxValueType::from(rhs),
            )))
        }
    }
}

impl Div for LoxValue {
    type Output = LoxResult<Self>;

    fn div(self, rhs: Self) -> Self::Output {
        if let Self::Num(num1) = self {
            if let Self::Num(num2) = rhs {
                Ok(LoxValue::from(num1 / num2))
            } else {
                Err(LoxError::TypeError(format!(
                    "cannot divide number by {}",
                    LoxValueType::from(rhs)
                )))
            }
        } else {
            Err(LoxError::TypeError(format!(
                "cannot divide {} by {}",
                LoxValueType::from(self),
                LoxValueType::from(rhs)
            )))
        }
    }
}

impl Neg for LoxValue {
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

impl Not for LoxValue {
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

impl BitOr for LoxValue {
    type Output = LoxResult<Self>;

    fn bitor(self, rhs: Self) -> Self::Output {
        if let Self::Bool(b1) = self {
            if let Self::Bool(b2) = rhs {
                Ok(LoxValue::from(b1 || b2))
            } else {
                Err(LoxError::TypeError(format!(
                    "invalid operands for 'or': bool and {}",
                    LoxValueType::from(rhs)
                )))
            }
        } else {
            Err(LoxError::TypeError(format!(
                "invalid operands for 'or': {} and {}",
                LoxValueType::from(self),
                LoxValueType::from(rhs)
            )))
        }
    }
}

impl BitOr<bool> for LoxValue {
    type Output = LoxResult<Self>;

    fn bitor(self, rhs: bool) -> Self::Output {
        self | Self::Bool(rhs)
    }
}

impl BitAnd for LoxValue {
    type Output = LoxResult<Self>;

    fn bitand(self, rhs: Self) -> Self::Output {
        if let Self::Bool(b1) = self {
            if let Self::Bool(b2) = rhs {
                Ok(LoxValue::from(b1 && b2))
            } else {
                Err(LoxError::TypeError(format!(
                    "invalid operands for 'and': bool and {}",
                    LoxValueType::from(rhs)
                )))
            }
        } else {
            Err(LoxError::TypeError(format!(
                "invalid operands for 'and': {} and {}",
                LoxValueType::from(self),
                LoxValueType::from(rhs)
            )))
        }
    }
}

impl BitAnd<bool> for LoxValue {
    type Output = LoxResult<Self>;

    fn bitand(self, rhs: bool) -> Self::Output {
        self | Self::Bool(rhs)
    }
}

impl Index<usize> for LoxValue {
    type Output = Self;

    fn index(&self, index: usize) -> &Self::Output {
        match self {
            Self::Arr(arr) => &arr[index],
            Self::Str(s) => &(s[index..index + 1][0]),
            _ => {
                panic!("cannot index {} with number", LoxValueType::from(self))
            }
        }
    }
}

impl Index<LoxValue> for LoxValue {
    type Output = Self;

    fn index(&self, value: LoxValue) -> &Self::Output {
        if let Self::Arr(arr) = self {
            if let Self::Num(num) = value {
                let index = num as usize;

                if index as f64 == num {
                    &arr[index]
                } else {
                    panic!("invalid base for index: {}", num);
                }
            } else {
                panic!("cannot index array with {}", LoxValueType::from(value));
            }
        } else {
            panic!(
                "cannot index {} with {}",
                LoxValueType::from(self),
                LoxValueType::from(value)
            );
        }
    }
}

impl<T> PartialOrd<T> for LoxValue
where
    T: Into<LoxValue> + Clone,
{
    fn partial_cmp(&self, other: &T) -> Option<std::cmp::Ordering> {
        let other: Self = other.clone().into();
        match self {
            Self::Bool(b1) => {
                if let Self::Bool(b2) = other {
                    b1.partial_cmp(&b2)
                } else {
                    None
                }
            }
            Self::Char(c1) => {
                if let Self::Char(c2) = other {
                    c1.partial_cmp(&c2)
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
            Self::Function(..) => None,
            Self::Instance(_) => None,
            Self::Nil => None,
        }
    }
}

impl IntoIterator for LoxValue {
    type Item = Self;
    type IntoIter = LoxIterator;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            Self::Arr(arr) => LoxIterator {
                values: arr,
                position: 0,
            },
            Self::Str(chars) => LoxIterator {
                values: chars,
                position: 0,
            },
            _ => panic!("cannot convert {} into iterator", LoxValueType::from(self)),
        }
    }
}

pub struct LoxIterator {
    values: Vec<LoxValue>,
    position: usize,
}

impl Iterator for LoxIterator {
    type Item = LoxValue;

    fn next(&mut self) -> Option<Self::Item> {
        if self.position >= self.values.len() {
            return None;
        }

        let value = self.values[self.position].clone();

        self.position += 1;

        Some(value)
    }
}

impl FnOnce<(Vec<LoxValue>,)> for LoxValue {
    type Output = LoxValue;

    extern "rust-call" fn call_once(self, args: (Vec<LoxValue>,)) -> Self::Output {
        match self {
            Self::Function(func) => (func.ptr)(args.0),
            _ => panic!("cannot call value of type {}", LoxValueType::from(self)),
        }
    }
}

impl FnMut<(Vec<LoxValue>,)> for LoxValue {
    extern "rust-call" fn call_mut(&mut self, args: (Vec<LoxValue>,)) -> Self::Output {
        match self {
            Self::Function(func) => (func.ptr)(args.0),
            _ => panic!("cannot call value of type {}", LoxValueType::from(self)),
        }
    }
}

impl Fn<(Vec<LoxValue>,)> for LoxValue {
    extern "rust-call" fn call(&self, args: (Vec<LoxValue>,)) -> Self::Output {
        match self {
            Self::Function(func) => (func.ptr)(args.0),
            _ => panic!("cannot call value of type {}", LoxValueType::from(self)),
        }
    }
}

impl ToTokens for LoxValue {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Bool(b) => tokens.append_all(quote! { LoxValue::from(#b) }),
            Self::Char(c) => tokens.append_all(quote! { LoxValue::from(#c) }),
            Self::Str(_) => {
                let s = self.to_string();

                tokens.append_all(quote! { LoxValue::from(#s) });
            }
            Self::Num(n) => tokens.append_all(quote! { LoxValue::from(#n) }),
            Self::Arr(arr) => {
                tokens.append_all(quote! { LoxValue::from });
                tokens.append(Punct::new('(', Spacing::Alone));
                tokens.append_all(quote! { vec! });
                tokens.append(Punct::new('[', Spacing::Alone));
                for i in 0..arr.len() {
                    let value = arr[i].clone();
                    tokens.append_all(quote! { #value, });
                }
                tokens.append(Punct::new(']', Spacing::Joint));
                tokens.append(Punct::new(')', Spacing::Alone));
            }
            Self::Function(_) => unimplemented!(
                "tokens produced by Lox functions differ for statements and expressions, and so must be converted manually"
            ),
            Self::Instance(_) => todo!(),
            Self::Nil => tokens.append_all(quote! { LoxValue::Nil }),
        }
    }
}

#[cfg(test)]
mod tests;
