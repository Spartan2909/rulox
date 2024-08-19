use crate::LoxArgs;
use crate::LoxError;
use crate::LoxObject;
use crate::LoxResult;
use crate::LoxValue;
use crate::MapKey;
use crate::Shared;

use std::collections::HashMap;

use serde::Serialize;
use serde::Serializer;

use serde_json::Map;
use serde_json::Number;
use serde_json::Value;

/// A [`LoxObject`] that can be serialised with `serde`.
pub trait SerializableLoxObject<S: Serializer>: LoxObject {
    /// Serialise this object.
    ///
    /// ## Errors
    ///
    /// Returns an error if serialisation fails.
    fn serialize(&self, serializer: S) -> Result<S::Ok, S::Error> {
        "<external object>".serialize(serializer)
    }
}

impl<T: LoxObject + ?Sized, S: Serializer> SerializableLoxObject<S> for T {}

#[allow(clippy::trivially_copy_pass_by_ref)]
pub(super) fn primitive_method<S: Serializer>(
    _func: &fn(LoxArgs) -> LoxResult,
    _value: &LoxValue,
    serializer: S,
) -> Result<S::Ok, S::Error> {
    "<bound method>".serialize(serializer)
}

pub(super) fn external<S: Serializer>(
    external: &Shared<dyn LoxObject>,
    serializer: S,
) -> Result<S::Ok, S::Error> {
    external.read().serialize(serializer)
}

impl TryFrom<&Value> for LoxValue {
    type Error = LoxError;

    fn try_from(value: &Value) -> Result<Self, LoxError> {
        match value {
            Value::Null => Ok(LoxValue::Nil),
            Value::Bool(b) => Ok(LoxValue::Bool(*b)),
            Value::Number(num) => Ok(LoxValue::Num(num.as_f64().unwrap())),
            Value::String(string) => Ok(LoxValue::Str(string.as_str().into())),
            Value::Array(arr) => Ok(LoxValue::Arr(Shared::new(
                arr.iter()
                    .map(TryInto::<LoxValue>::try_into)
                    .collect::<Result<Vec<_>, _>>()?,
            ))),
            Value::Object(obj) => Ok(LoxValue::Map(Shared::new(
                obj.into_iter()
                    .map(|(key, value)| {
                        Ok((
                            MapKey::verify_key(key.to_string().into()).unwrap(),
                            value.try_into()?,
                        ))
                    })
                    .collect::<Result<HashMap<MapKey, LoxValue>, LoxError>>()?,
            ))),
        }
    }
}

impl TryFrom<Value> for LoxValue {
    type Error = LoxError;

    fn try_from(value: Value) -> Result<Self, LoxError> {
        (&value).try_into()
    }
}

/// Converts a Lox hashmap to a JSON map.
///
/// ## Errors
///
/// Returns an error if any of the keys are not strings or if the values are not
/// valid JSON values.
pub fn hashmap_to_json_map<S>(
    map: &HashMap<MapKey, LoxValue, S>,
) -> Result<Map<String, Value>, LoxError> {
    map.iter()
        .map(|(key, value)| Ok((key.as_inner().expect_str()?.to_string(), value.try_into()?)))
        .collect()
}

impl TryFrom<&LoxValue> for Value {
    type Error = LoxError;

    fn try_from(value: &LoxValue) -> Result<Self, Self::Error> {
        match value {
            LoxValue::Bool(b) => Ok(Value::Bool(*b)),
            LoxValue::Str(s) => Ok(Value::String(s.to_string())),
            LoxValue::Num(n) => Ok(Value::Number(Number::from_f64(*n).ok_or_else(|| {
                LoxError::type_error(format!("invalid json value: '{n}'").into())
            })?)),
            LoxValue::Arr(arr) => {
                let arr: Result<Vec<Value>, LoxError> =
                    arr.read().iter().map(TryInto::try_into).collect();
                Ok(Value::Array(arr?))
            }
            LoxValue::Map(map) => Ok(Value::Object(hashmap_to_json_map(&map.read())?)),
            LoxValue::Instance(instance) => {
                let map: Result<Map<String, Value>, LoxError> = instance
                    .read()
                    .attributes
                    .iter()
                    .map(|(key, value)| Ok((key.to_owned(), value.try_into()?)))
                    .collect();
                Ok(Value::Object(map?))
            }
            LoxValue::Bytes(bytes) => Ok(Value::String(
                String::from_utf8(bytes.to_vec()).map_err(LoxError::external)?,
            )),
            LoxValue::Nil => Ok(Value::Null),
            _ => Err(LoxError::type_error(
                format!("invalid json value: '{value}'").into(),
            )),
        }
    }
}

impl TryFrom<LoxValue> for Value {
    type Error = LoxError;

    fn try_from(value: LoxValue) -> Result<Self, Self::Error> {
        (&value).try_into()
    }
}
