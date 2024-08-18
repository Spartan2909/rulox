#![allow(clippy::implicit_hasher)]

use crate::error::LoxError;
use crate::interop::LoxObject;
use crate::shared::Shared;
use crate::LoxClass;
use crate::LoxFn;
use crate::LoxInstance;
use crate::LoxValue;
use crate::LoxValueType;
use crate::MapKey;

use crate::async_types;

use std::collections::HashMap;
use std::sync::Arc;

use bytes::Bytes;

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
        Self::Str((value.chars()).collect::<String>().into())
    }
}

impl From<&str> for LoxValue {
    fn from(value: &str) -> Self {
        Self::from(value.to_string())
    }
}

impl From<Vec<LoxValue>> for LoxValue {
    fn from(values: Vec<LoxValue>) -> Self {
        Self::Arr(Shared::new(values))
    }
}

impl<const N: usize> From<[LoxValue; N]> for LoxValue {
    fn from(values: [LoxValue; N]) -> Self {
        Self::from(Vec::from(values))
    }
}

impl From<Box<LoxError>> for LoxValue {
    fn from(value: Box<LoxError>) -> Self {
        LoxValue::Error(value)
    }
}

impl From<LoxError> for LoxValue {
    fn from(value: LoxError) -> Self {
        LoxValue::Error(Box::new(value))
    }
}

impl From<()> for LoxValue {
    fn from(_value: ()) -> Self {
        LoxValue::Nil
    }
}

impl From<Arc<LoxClass>> for LoxValue {
    fn from(value: Arc<LoxClass>) -> Self {
        LoxValue::Class(value)
    }
}

impl From<HashMap<MapKey, LoxValue>> for LoxValue {
    fn from(value: HashMap<MapKey, LoxValue>) -> Self {
        LoxValue::Map(Shared::new(value))
    }
}

impl From<Bytes> for LoxValue {
    fn from(value: Bytes) -> Self {
        LoxValue::Bytes(value)
    }
}

impl<T: LoxObject + Send + Sync> From<T> for LoxValue {
    fn from(value: T) -> Self {
        LoxValue::external(value)
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

impl TryFrom<LoxValue> for Arc<str> {
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

impl TryFrom<LoxValue> for Arc<LoxFn> {
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

impl TryFrom<LoxValue> for Shared<HashMap<MapKey, LoxValue>> {
    type Error = LoxError;

    fn try_from(value: LoxValue) -> Result<Self, Self::Error> {
        value.expect_map().cloned()
    }
}

impl TryFrom<LoxValue> for HashMap<MapKey, LoxValue> {
    type Error = LoxError;

    fn try_from(value: LoxValue) -> Result<Self, Self::Error> {
        value.expect_map().map(|value| value.read().clone())
    }
}

impl TryFrom<LoxValue> for Bytes {
    type Error = LoxError;

    fn try_from(value: LoxValue) -> Result<Self, Self::Error> {
        value.expect_bytes().cloned()
    }
}

impl TryFrom<LoxValue> for Arc<async_types::Coroutine> {
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

macro_rules! numeric_conversions {
    ( $( $ty:ty ),* ) => {
        $(
            impl From<$ty> for LoxValue {
                #[allow(clippy::cast_lossless)]
                fn from(value: $ty) -> Self {
                    Self::Num(value as f64)
                }
            }

            impl TryFrom<LoxValue> for $ty {
                type Error = LoxError;

                #[allow(clippy::cast_lossless)]
                fn try_from(value: LoxValue) -> Result<Self, LoxError> {
                    match &value {
                        LoxValue::Num(num) => {
                            if *num > Self::MAX as f64 {
                                Err(LoxError::overflow_error(MapKey::verify_key(value).unwrap(), stringify!($t)))
                            } else {
                                Ok(*num as Self)
                            }
                        },
                        _ => Err(LoxError::type_error(format!("expected number, found {}", LoxValueType::from(value))))
                    }
                }
            }
        )*
    };
}

numeric_conversions! { f32, f64, u8, u16, u32, u64, u128, usize, i8, i16, i32, i64, i128, isize }
