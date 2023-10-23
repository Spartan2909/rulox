use crate::error::LoxError;
use crate::shared::write;
use crate::LoxRc;
use crate::LoxResult;
use crate::LoxValue;
use crate::LoxValueType;

use std::ops::Add;
use std::ops::Div;
use std::ops::Mul;
use std::ops::Neg;
use std::ops::Not;
use std::ops::Rem;
use std::ops::Sub;

macro_rules! numeric_operations {
    ( $($t:ty),* ) => {
        $(
            impl Add<$t> for LoxValue {
                type Output = LoxResult;

                fn add(self, rhs: $t) -> Self::Output {
                    match self {
                        Self::Num(num) => Ok(Self::Num(num + rhs as f64)),
                        _ => Err(LoxError::type_error(format!("cannot add number to {}", LoxValueType::from(self)))),
                    }
                }
            }

            impl Sub<$t> for LoxValue {
                type Output = LoxResult;

                fn sub(self, rhs: $t) -> Self::Output {
                    match self {
                        Self::Num(num) => Ok(Self::Num(num - rhs as f64)),
                        _ => Err(LoxError::type_error(format!("cannot subtract number from {}", LoxValueType::from(self)))),
                    }
                }
            }

            impl Mul<$t> for LoxValue {
                type Output = LoxResult;

                fn mul(self, rhs: $t) -> Self::Output {
                    match self {
                        Self::Num(num) => Ok(Self::Num(num * rhs as f64)),
                        _ => Err(LoxError::type_error(format!("cannot multiply {} by number", LoxValueType::from(self)))),
                    }
                }
            }

            impl Div<$t> for LoxValue {
                type Output = LoxResult;

                fn div(self, rhs: $t) -> Self::Output {
                    match self {
                        Self::Num(num) => Ok(Self::Num(num / rhs as f64)),
                        _ => Err(LoxError::type_error(format!("cannot divide {} by number", LoxValueType::from(self)))),
                    }
                }
            }

            impl Rem<$t> for LoxValue {
                type Output = LoxResult;

                fn rem(self, rhs: $t) -> Self::Output {
                    match self {
                        Self::Num(num) => Ok(Self::Num(num / rhs as f64)),
                        _ => Err(LoxError::type_error(format!("cannot take the remainder of {} and a number", LoxValueType::from(self)))),
                    }
                }
            }
        )*
    };
}

numeric_operations! { f32, f64, u8, u16, u32, u64, u128, usize, i8, i16, i32, i64, i128, isize }

impl Add for LoxValue {
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

impl Add<&str> for LoxValue {
    type Output = LoxResult;

    fn add(self, rhs: &str) -> Self::Output {
        let other: LoxValue = rhs.into();
        self + other
    }
}

impl Sub for LoxValue {
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

impl Mul for LoxValue {
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

impl Div for LoxValue {
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

impl Rem for LoxValue {
    type Output = LoxResult;

    fn rem(self, rhs: Self) -> Self::Output {
        if let (&Self::Num(num1), &Self::Num(num2)) = (&self, &rhs) {
            Ok(LoxValue::Num(num1 % num2))
        } else {
            Err(LoxError::type_error(format!(
                "cannot take remainder of {} and {}",
                LoxValueType::from(self),
                LoxValueType::from(rhs),
            )))
        }
    }
}

impl Neg for LoxValue {
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

impl Not for LoxValue {
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
