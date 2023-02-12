//! # rulox types
//!
//! `rulox_types` is a collection of types used by the `rulox` crate
//! to represent dynamically typed values.

use std::{collections::HashMap, error::Error, fmt, mem::size_of};

/// An error that occurred when attempting to use a LoxValue in an invalid location.
#[derive(Debug)]
pub enum LoxError {
    /// An error that occurred when attempting to use a LoxValue with an invalid type.
    TypeError {
        expected: Vec<LoxValueType>,
        found: LoxValueType,
    },
    /// An error that occurred when attempting to convert a LoxValue into a Rust type that is too small.
    SizeError { required: usize, found: usize },
}

impl fmt::Display for LoxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::TypeError { expected, found } => {
                write!(f, "expected one of {:?}, found {:?}", expected, found)
            }
            Self::SizeError { required, found } => {
                write!(
                    f,
                    "could not convert from a LoxValue of size {:?} to a value of size {:?}",
                    required, found
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

/// An enum used for error reporting.
#[derive(Debug)]
pub enum LoxValueType {
    Str,
    Num,
    Arr,
    Instance,
}

impl From<LoxValue> for LoxValueType {
    fn from(value: LoxValue) -> Self {
        match value {
            LoxValue::Str(_) => Self::Str,
            LoxValue::Num(_) => Self::Num,
            LoxValue::Arr(_) => Self::Arr,
            LoxValue::Instance(_) => Self::Instance,
        }
    }
}

/// A dynamically typed value used by Lox programs.
#[derive(Debug, Clone)]
pub enum LoxValue {
    Str(LoxStr),
    Num(LoxNum),
    Arr(LoxArr),
    Instance(LoxInstance),
}

/// A Lox string.
#[derive(Debug, Clone)]
pub struct LoxStr {
    value: String,
}

/// A Lox number.
#[derive(Debug, Clone)]
pub struct LoxNum {
    value: f64,
}

/// A Lox array.
#[derive(Debug, Clone)]
pub struct LoxArr {
    value: Vec<LoxValue>,
}

/// An instance of a Lox class.
#[derive(Debug, Clone)]
pub struct LoxInstance {
    attributes: HashMap<String, LoxValue>,
    methods: Vec<String>,
}

impl From<String> for LoxValue {
    fn from(value: String) -> Self {
        Self::Str(LoxStr { value })
    }
}

impl From<&str> for LoxValue {
    fn from(value: &str) -> Self {
        Self::from(value.to_string())
    }
}

macro_rules! impl_convert_num_to_loxvalue {
    ( $($t:ty),* ) => {
    $( impl From<$t> for LoxValue {
        fn from(value: $t) -> Self {
            Self::Num(LoxNum { value: value as f64 })
        }
    }) *
    };
}

impl_convert_num_to_loxvalue! { f32, f64, u8, u16, u32, u64, u128, i8, i16, i32, i64, i128 }

impl TryInto<String> for LoxValue {
    type Error = LoxError;

    fn try_into(self) -> Result<String, Self::Error> {
        if let LoxValue::Str(value) = self {
            return Ok(value.value);
        }

        Err(LoxError::new_type_single(
            LoxValueType::Str,
            LoxValueType::from(self),
        ))
    }
}

macro_rules! impl_try_convert_loxvalue_to_num {
    ( $($t:ty),* ) => {
    $( impl TryFrom<LoxValue> for $t {
        type Error = LoxError;

        fn try_from(value: LoxValue) -> Result<Self, Self::Error> {
            match value {
                LoxValue::Num(ref num) => {
                    let converted = num.value as Self;
                    if converted as f64 == num.value {
                        Ok(converted)
                    } else {
                        Err(LoxError::SizeError{
                            required: 8,
                            found: size_of::<Self>()
                        })
                    }
                },
                _ => Err(LoxError::new_type_single(
                        LoxValueType::Num,
                        LoxValueType::from(value),
                    ))
            }
        }
    }) *
    };
}

impl_try_convert_loxvalue_to_num! { f32, f64, u8, u16, u32, u64, u128, i8, i16, i32, i64, i128 }
