use crate::error::LoxError;
use crate::shared::Shared;
use crate::LoxArgs;
use crate::LoxResult;
use crate::LoxValue;

use std::any::Any;
use std::error::Error;
use std::fmt::Debug;

use castaway::cast;

#[doc(hidden)]
pub trait ToLoxResult {
    fn to_lox_result(self) -> LoxResult;
}

impl<T: Into<LoxValue>> ToLoxResult for T {
    fn to_lox_result(self) -> LoxResult {
        let value: LoxValue = self.into();
        if let LoxValue::Error(err) = value {
            Err(*err)
        } else {
            Ok(value)
        }
    }
}

impl<T: Into<LoxValue>, E: Error + Send + Sync + 'static> ToLoxResult for Result<T, E> {
    fn to_lox_result(self) -> LoxResult {
        self.map(Into::into).map_err(|err| {
            if cast!(&err, &LoxError).is_ok() {
                cast!(err, LoxError).unwrap()
            } else {
                LoxError::external(err)
            }
        })
    }
}

/// A trait for getting the name of a type from a reference.
pub trait Named {
    /// A programmer-friendly name for the type.
    ///
    /// Where an implementation of [`LoxObject`] exists, this function should be
    /// consistent with [`LoxObject::type_name`].
    fn type_name_of_val(&self) -> String;
}

impl<T: LoxObject> Named for T {
    fn type_name_of_val(&self) -> String {
        Self::type_name()
    }
}

/// A trait for foreign objects that can be used in Lox.
pub trait LoxObject: Any + Debug + Named + Send + Sync {
    /// A programmer-friendly name for the type.
    fn type_name() -> String
    where
        Self: Sized;

    /// A programmer-friendly representation of `self` for debugging and error
    /// messages.
    fn representation(&self) -> String {
        format!("{self:#?}")
    }

    /// Gets the field of `self` corresponding to `key`.
    ///
    /// ## Errors
    /// Returns an error if this functionality is not implemented for this type.
    /// This method should return `Err(None)` if the value is not found.
    fn get(&self, this: Shared<dyn LoxObject>, key: &str) -> Result<LoxValue, Option<LoxError>> {
        let (_, _) = (this, key);
        Err(None)
    }

    /// Sets the field of `self` corresponding to `key` to the given value.
    ///
    /// ## Errors
    ///
    /// Returns an error if this functionality is not implemented for this type.
    /// If the key does not exist, `Err(None)` should be returned.
    fn set(
        &mut self,
        this: Shared<dyn LoxObject>,
        key: &str,
        value: LoxValue,
    ) -> Result<(), Option<LoxError>> {
        let (_, _, _) = (this, key, value);
        Err(None)
    }

    /// Gets the element of `self` corresponding to `key`.
    ///
    /// Intended for objects that function as arrays or maps.
    ///
    /// ## Errors
    ///
    /// Returns an error if this functionality is not implemented for this type.
    fn index(&self, key: LoxValue) -> Result<LoxValue, LoxError> {
        drop(key);
        Err(LoxError::type_error(
            format!("cannot index into '{}'", self.type_name_of_val()).into(),
        ))
    }

    /// Sets the element of `self` corresponding to `key` to `value`.
    ///
    /// Intended for objects that function as arrays or maps.
    ///
    /// ## Errors
    ///
    /// Returns an error if this functionality is not implemented for this type.
    fn index_set(&mut self, key: LoxValue, value: LoxValue) -> Result<(), LoxError> {
        let (_, _) = (key, value);
        Err(LoxError::type_error(
            format!("cannot index into '{}'", self.type_name_of_val()).into(),
        ))
    }

    /// Add `self` to `rhs`.
    ///
    /// ## Errors
    ///
    /// Returns an error if this functionality is not implemented for this type.
    fn add(&self, rhs: LoxValue) -> LoxResult {
        let _ = rhs;
        Err(LoxError::not_implemented("add", &self.type_name_of_val()))
    }

    /// Subtract `rhs` from `self`.
    ///
    /// ## Errors
    ///
    /// Returns an error if this functionality is not implemented for this type.
    fn sub(&self, rhs: LoxValue) -> LoxResult {
        let _ = rhs;
        Err(LoxError::not_implemented("sub", &self.type_name_of_val()))
    }

    /// Multiply `self` by `rhs`.
    ///
    /// ## Errors
    ///
    /// Returns an error if this functionality is not implemented for this type.
    fn mul(&self, rhs: LoxValue) -> LoxResult {
        let _ = rhs;
        Err(LoxError::not_implemented("mul", &self.type_name_of_val()))
    }

    /// Divide `self` by `rhs`.
    ///
    /// ## Errors
    ///
    /// Returns an error if this functionality is not implemented for this type.
    fn div(&self, rhs: LoxValue) -> LoxResult {
        let _ = rhs;
        Err(LoxError::not_implemented("div", &self.type_name_of_val()))
    }

    /// Take the remainder when `self` is divided by `rhs`.
    ///
    /// ## Errors
    ///
    /// Returns an error if this functionality is not implemented for this type.
    fn rem(&self, rhs: LoxValue) -> LoxResult {
        let _ = rhs;
        Err(LoxError::not_implemented("rem", &self.type_name_of_val()))
    }

    /// Negate `self`.
    ///
    /// ## Errors
    ///
    /// Returns an error if this functionality is not implemented for this type.
    fn neg(&self) -> LoxResult {
        Err(LoxError::not_implemented("neg", &self.type_name_of_val()))
    }

    /// Call `self` like a method with the given arguments.
    ///
    /// ## Errors
    ///
    /// Returns an error if this functionality is not implemented for this type.
    fn call(&self, args: LoxArgs) -> LoxResult {
        let _ = args;
        Err(LoxError::not_implemented("call", &self.type_name_of_val()))
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
        obj_from_value(&value).ok_or_else(|| {
            LoxError::type_error(format!("expected {}, found {value}", T::type_name()).into())
        })
    }
}
