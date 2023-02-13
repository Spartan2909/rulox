//! # rulox types
//!
//! `rulox_types` is a collection of types used by the `rulox` crate
//! to represent dynamically typed values.

use std::{
    collections::HashMap,
    error::Error,
    fmt,
    mem::size_of,
    ops::{Add, Div, Mul, Sub},
};

/// An error that occurred when attempting to use a LoxValue in an invalid location.
#[derive(Debug)]
pub enum LoxError {
    /// An error that occurred when attempting to use a LoxValue with an invalid type.
    TypeError {
        expected: Vec<LoxValueType>,
        found: LoxValueType,
    },
    /// An error that occurred when attempting to convert a LoxValue into a Rust type that is too small.
    SizeError { found: usize },
}

impl fmt::Display for LoxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::TypeError { expected, found } => {
                write!(f, "expected one of {:?}, found {:?}", expected, found)
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

impl LoxError {
    fn new_type_single(expected: LoxValueType, found: LoxValueType) -> Self {
        Self::TypeError {
            expected: vec![expected],
            found,
        }
    }
}

/// A potential error returned by Lox code.
type LoxResult<T> = Result<T, LoxError>;

/// An enum used for error reporting.
#[derive(Debug)]
pub enum LoxValueType {
    Bool,
    Str,
    Num,
    Arr,
    Instance,
    Nil,
}

impl fmt::Display for LoxValueType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Bool => write!(f, "bool"),
            Self::Str => write!(f, "string"),
            Self::Num => write!(f, "number"),
            Self::Arr => write!(f, "array"),
            Self::Instance => write!(f, "instance"),
            Self::Nil => write!(f, "nil"),
        }
    }
}

impl From<LoxValue> for LoxValueType {
    fn from(value: LoxValue) -> Self {
        match value {
            LoxValue::Bool(_) => Self::Bool,
            LoxValue::Str(_) => Self::Str,
            LoxValue::Num(_) => Self::Num,
            LoxValue::Arr(_) => Self::Arr,
            LoxValue::Instance(_) => Self::Instance,
            LoxValue::Nil => Self::Nil,
        }
    }
}

/// A dynamically typed value used by Lox programs.
#[derive(Debug, Clone)]
pub enum LoxValue {
    Bool(bool),
    Str(String),
    Num(f64),
    Arr(Vec<LoxValue>),
    Instance(LoxInstance),
    Nil,
}

/// An instance of a Lox class.
#[derive(Debug, Clone, PartialEq)]
pub struct LoxInstance {
    class: String,
    attributes: HashMap<String, LoxValue>,
    methods: Vec<String>,
}

impl<T> PartialEq<T> for LoxValue
    where T: Into<LoxValue> + Clone
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
            Self::Str(value) => {
                write!(f, "\"{}\"", value)
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
            Self::Instance(instance) => {
                write!(f, "<Instance of {}>", instance.class)
            }
            Self::Nil => {
                write!(f, "nil")
            }
        }
    }
}

impl From<&LoxValue> for LoxValue {
    fn from(value: &LoxValue) -> Self {
        value.clone()
    }
}

impl From<bool> for LoxValue {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}

impl From<String> for LoxValue {
    fn from(value: String) -> Self {
        Self::Str(value)
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

impl TryFrom<LoxValue> for bool {
    type Error = LoxError;

    fn try_from(value: LoxValue) -> Result<Self, Self::Error> {
        if let LoxValue::Bool(b) = value {
            Ok(b)
        } else {
            Err(LoxError::new_type_single(
                LoxValueType::Bool,
                LoxValueType::from(value),
            ))
        }
    }
}

impl TryFrom<&LoxValue> for bool {
    type Error = LoxError;

    fn try_from(value: &LoxValue) -> Result<Self, Self::Error> {
        Self::try_from(value.clone())
    }
}

impl TryFrom<LoxValue> for String {
    type Error = LoxError;

    fn try_from(value: LoxValue) -> Result<Self, Self::Error> {
        if let LoxValue::Str(s) = value {
            Ok(s)
        } else {
            Err(LoxError::new_type_single(
                LoxValueType::Str,
                LoxValueType::from(value),
            ))
        }
    }
}

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
                    _ => Err(LoxError::new_type_single(
                            LoxValueType::Num,
                            LoxValueType::from(value),
                        ))
                }
            }
        }

        impl Add<$t> for LoxValue {
            type Output = LoxResult<Self>;

            fn add(self, rhs: $t) -> Self::Output {
                match self {
                    Self::Num(num) => Ok(Self::from(num + rhs as f64)),
                    _ => Err(LoxError::new_type_single(LoxValueType::Num, LoxValueType::from(self))),
                }
            }
        }

        impl Sub<$t> for LoxValue {
            type Output = LoxResult<Self>;

            fn sub(self, rhs: $t) -> Self::Output {
                match self {
                    Self::Num(num) => Ok(Self::from(num - rhs as f64)),
                    _ => Err(LoxError::new_type_single(LoxValueType::Num, LoxValueType::from(self))),
                }
            }
        }

        impl Mul<$t> for LoxValue {
            type Output = LoxResult<Self>;

            fn mul(self, rhs: $t) -> Self::Output {
                match self {
                    Self::Num(num) => Ok(Self::from(num * rhs as f64)),
                    _ => Err(LoxError::new_type_single(LoxValueType::Num, LoxValueType::from(self))),
                }
            }
        }

        impl Div<$t> for LoxValue {
            type Output = LoxResult<Self>;

            fn div(self, rhs: $t) -> Self::Output {
                match self {
                    Self::Num(num) => Ok(Self::from(num / rhs as f64)),
                    _ => Err(LoxError::new_type_single(LoxValueType::Num, LoxValueType::from(self))),
                }
            }
        }
    ) *
    };
}

impl_numeric! { f32, f64, u8, u16, u32, u64, u128, i8, i16, i32, i64, i128 }

impl Add for LoxValue {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match self {
            LoxValue::Str(s) => {
                if let LoxValue::Str(other) = rhs {
                    LoxValue::from(s + &other)
                } else {
                    panic!("cannot add {} to string", LoxValueType::from(rhs))
                }
            }
            LoxValue::Num(n) => {
                if let LoxValue::Num(other) = rhs {
                    LoxValue::from(n + other)
                } else {
                    panic!("cannot add {} to number", LoxValueType::from(rhs))
                }
            }
            LoxValue::Arr(values) => {
                if let LoxValue::Arr(other) = rhs {
                    let mut result = values.clone();
                    result.extend(other);
                    LoxValue::from(result)
                } else {
                    panic!("cannot add {} to array", LoxValueType::from(rhs))
                }
            }
            _ => panic!(
                "cannot add {} to {}",
                LoxValueType::from(rhs),
                LoxValueType::from(self)
            ),
        }
    }
}

impl Sub for LoxValue {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        if let Self::Num(num1) = self {
            if let Self::Num(num2) = rhs {
                LoxValue::from(num1 - num2)
            } else {
                panic!("cannot subtract {} from number", LoxValueType::from(rhs))
            }
        } else {
            panic!(
                "cannot subtract {} from {}",
                LoxValueType::from(rhs),
                LoxValueType::from(self)
            )
        }
    }
}

impl Mul for LoxValue {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        if let Self::Num(num1) = self {
            if let Self::Num(num2) = rhs {
                LoxValue::from(num1 * num2)
            } else {
                panic!("cannot multiply number by {}", LoxValueType::from(rhs))
            }
        } else {
            panic!(
                "cannot multiply {} by {}",
                LoxValueType::from(self),
                LoxValueType::from(rhs),
            )
        }
    }
}

impl Div for LoxValue {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        if let Self::Num(num1) = self {
            if let Self::Num(num2) = rhs {
                LoxValue::from(num1 / num2)
            } else {
                panic!("cannot divide number by {}", LoxValueType::from(rhs))
            }
        } else {
            panic!(
                "cannot divide {} by {}",
                LoxValueType::from(self),
                LoxValueType::from(rhs)
            )
        }
    }
}

#[cfg(test)]
mod tests;
