use crate::error::LoxError;
use crate::shared;
use crate::shared::read;
use crate::shared::Shared;
use crate::LoxRc;
use crate::LoxResult;
use crate::LoxValue;

use std::any::Any;
use std::any::TypeId;
use std::error::Error;
use std::fmt::Debug;
use std::ops::Deref;

use castaway::cast;

#[doc(hidden)]
pub trait ToLoxResult {
    fn to_lox_result(self) -> LoxResult;
}

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
pub trait LoxObject: Any + Debug {
    /// A programmer-friendly name for the type.
    fn name() -> String
    where
        Self: Sized;

    /// A programmer-friendly representation of `self` for debugging and error
    /// messages.
    fn representation(&self) -> String {
        format!("{:#?}", self)
    }

    /// Gets the field of `self` corresponding to `key`.
    ///
    /// This method should return `Err(None)` if the value is not found.
    fn get(
        &self,
        this: Shared<DynLoxObject>,
        key: &'static str,
    ) -> Result<LoxValue, Option<LoxError>> {
        let (_, _) = (this, key);
        Err(None)
    }

    /// Sets the field of `self` corresponding to `key` to the given value.
    ///
    /// If the key does not exist, `Err(None)` should be returned.
    fn set(
        &mut self,
        this: Shared<DynLoxObject>,
        key: &'static str,
        value: LoxValue,
    ) -> Result<(), Option<LoxError>> {
        let (_, _, _) = (this, key, value);
        Err(None)
    }

    /// Gets the element of `self` corresponding to `key`.
    ///
    /// Intended for objects that function as arrays or maps.
    fn index(&self, key: LoxValue) -> Result<LoxValue, LoxError> {
        drop(key);
        Err(LoxError::type_error(format!(
            "cannot index into '{}'",
            self.representation()
        )))
    }

    /// Sets the element of `self` corresponding to `key` to `value`.
    ///
    /// Intended for objects that function as arrays or maps.
    fn index_set(&mut self, key: LoxValue, value: LoxValue) -> Result<(), LoxError> {
        let (_, _) = (key, value);
        Err(LoxError::type_error(format!(
            "cannot index into '{}'",
            self.representation()
        )))
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
