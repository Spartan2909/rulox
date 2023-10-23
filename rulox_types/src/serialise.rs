use crate::read;
use crate::shared::Inner;
use crate::DynLoxObject;
use crate::LoxArgs;
use crate::LoxError;
use crate::LoxRc;
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

pub(super) fn primitive_method<S: Serializer>(
    _func: &fn(LoxArgs) -> LoxResult,
    _value: &LoxValue,
    serializer: S,
) -> Result<S::Ok, S::Error> {
    LoxValue::Str(LoxRc::new("<bound method>".to_string())).serialize(serializer)
}

pub(super) fn external<S: Serializer>(
    _external: &Shared<DynLoxObject>,
    serializer: S,
) -> Result<S::Ok, S::Error> {
    LoxValue::Str(LoxRc::new("<external object>".to_string())).serialize(serializer)
}

impl From<&Value> for LoxValue {
    fn from(value: &Value) -> Self {
        match value {
            Value::Null => LoxValue::Nil,
            Value::Bool(b) => LoxValue::Bool(*b),
            Value::Number(num) => LoxValue::Num(num.as_f64().unwrap()),
            Value::String(string) => LoxValue::Str(LoxRc::new(string.clone())),
            Value::Array(arr) => LoxValue::Arr(LoxRc::new(Inner::new(
                arr.into_iter().map(|value| value.into()).collect(),
            ))),
            Value::Object(obj) => LoxValue::Map(LoxRc::new(
                HashMap::from_iter(
                    obj.into_iter()
                        .map(|(key, value)| (MapKey::verify_key(key.to_string().into()).unwrap(), value.into())),
                )
                .into(),
            )),
        }
    }
}

impl From<Value> for LoxValue {
    fn from(value: Value) -> Self {
        (&value).into()
    }
}

/// Attempts to convert a Lox hashmap to a JSON map.
pub fn hashmap_to_json_map(
    map: &HashMap<MapKey, LoxValue>,
) -> Result<Map<String, Value>, LoxError> {
    map.iter()
        .map(|(key, value)| Ok((key.as_inner().as_str()?.to_string(), value.try_into()?)))
        .collect()
}

impl TryFrom<&LoxValue> for Value {
    type Error = LoxError;

    fn try_from(value: &LoxValue) -> Result<Self, Self::Error> {
        match value {
            LoxValue::Bool(b) => Ok(Value::Bool(*b)),
            LoxValue::Str(s) => Ok(Value::String(s.to_string())),
            LoxValue::Num(n) => Ok(Value::Number(
                Number::from_f64(*n)
                    .ok_or(LoxError::type_error(format!("invalid json value: '{n}'")))?,
            )),
            LoxValue::Arr(arr) => {
                let arr: Result<Vec<Value>, LoxError> =
                    read(arr).iter().map(|value| value.try_into()).collect();
                Ok(Value::Array(arr?))
            }
            LoxValue::Map(map) => Ok(Value::Object(hashmap_to_json_map(&read(map))?)),
            LoxValue::Instance(instance) => {
                let map: Result<Map<String, Value>, LoxError> = read(instance)
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
            _ => Err(LoxError::type_error(format!(
                "invalid json value: '{value}'"
            ))),
        }
    }
}

impl TryFrom<LoxValue> for Value {
    type Error = LoxError;

    fn try_from(value: LoxValue) -> Result<Self, Self::Error> {
        (&value).try_into()
    }
}
