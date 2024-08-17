#![allow(clippy::redundant_pub_crate)]
#![allow(clippy::unnecessary_wraps)]

use crate::functions::LoxArgs;
use crate::DynLoxObject;
use crate::LoxError;
use crate::LoxObject;
use crate::LoxResult;
use crate::LoxValue;
use crate::MapKey;
use crate::Shared;

use std::collections::HashMap;

pub(super) fn is_bool(args: LoxArgs) -> LoxResult {
    Ok(LoxValue::Bool(matches!(
        args.head.unwrap(),
        LoxValue::Bool(_)
    )))
}

pub(super) fn is_str(args: LoxArgs) -> LoxResult {
    Ok(LoxValue::Bool(matches!(
        args.head.unwrap(),
        LoxValue::Str(_)
    )))
}

pub(super) fn is_num(args: LoxArgs) -> LoxResult {
    Ok(LoxValue::Bool(matches!(
        args.head.unwrap(),
        LoxValue::Num(_)
    )))
}

pub(super) fn is_arr(args: LoxArgs) -> LoxResult {
    Ok(LoxValue::Bool(matches!(
        args.head.unwrap(),
        LoxValue::Arr(_)
    )))
}

pub(super) fn is_function(args: LoxArgs) -> LoxResult {
    Ok(LoxValue::Bool(matches!(
        args.head.unwrap(),
        LoxValue::BoundMethod(_, _) | LoxValue::Coroutine(_) | LoxValue::PrimitiveMethod(_, _)
    )))
}

pub(super) fn is_class(args: LoxArgs) -> LoxResult {
    Ok(LoxValue::Bool(matches!(
        args.head.unwrap(),
        LoxValue::Class(_)
    )))
}

pub(super) fn is_map(args: LoxArgs) -> LoxResult {
    Ok(LoxValue::Bool(matches!(
        args.head.unwrap(),
        LoxValue::Map(_)
    )))
}

pub(super) fn is_bytes(args: LoxArgs) -> LoxResult {
    Ok(LoxValue::Bool(matches!(
        args.head.unwrap(),
        LoxValue::Bytes(_)
    )))
}

pub(super) fn is_error(args: LoxArgs) -> LoxResult {
    Ok(LoxValue::Bool(matches!(
        args.head.unwrap(),
        LoxValue::Error(_)
    )))
}

pub(super) fn is_nil(args: LoxArgs) -> LoxResult {
    Ok(LoxValue::Bool(matches!(args.head.unwrap(), LoxValue::Nil)))
}

macro_rules! default_collection {
    ( $name:ident, $inner:ty, $loxvalue_variant:ident, $( $index:tt )+ ) => {
        #[derive(Debug, Clone)]
        struct $name($inner, LoxValue);

        impl LoxObject for $name {
            fn type_name() -> String
            where
                Self: Sized,
            {
                stringify!($name).to_string()
            }

            fn get(
                &self,
                _this: Shared<DynLoxObject>,
                key: &str,
            ) -> Result<LoxValue, Option<LoxError>> {
                Ok(LoxValue::$loxvalue_variant(self.0.clone()).get(key)?)
            }

            fn set(
                &mut self,
                _this: Shared<DynLoxObject>,
                key: &str,
                value: LoxValue,
            ) -> Result<(), Option<LoxError>> {
                LoxValue::$loxvalue_variant(self.0.clone()).set(key, value)?;
                Ok(())
            }

            $( $index )+

            fn index_set(
                &mut self,
                key: LoxValue,
                value: LoxValue,
            ) -> Result<(), LoxError> {
                LoxValue::$loxvalue_variant(self.0.clone()).index_set(key, value)
            }
        }
    };
}

pub(super) fn set_default(mut args: LoxArgs) -> LoxResult {
    match args.head.clone().unwrap() {
        LoxValue::Arr(arr) => {
            default_collection!(
                DefaultArray,
                Shared<Vec<LoxValue>>,
                Arr,
                fn index(&self, key: LoxValue) -> Result<LoxValue, LoxError> {
                    self.0
                        .read()
                        .get(usize::try_from(key)?)
                        .map_or_else(|| self.1.call([].into()), |value| Ok(value.clone()))
                }
            );

            Ok(LoxValue::external(DefaultArray(
                arr,
                args.drain().next().unwrap(),
            )))
        }
        LoxValue::Map(map) => {
            default_collection!(
                DefaultMap,
                Shared<HashMap<MapKey, LoxValue>>,
                Map,
                fn index(&self, key: LoxValue) -> Result<LoxValue, LoxError> {
                    self.0
                        .read()
                        .get(&MapKey::verify_key(key)?)
                        .map_or_else(|| self.1.call([].into()), |value| Ok(value.clone()))
                }
            );

            Ok(LoxValue::external(DefaultMap(
                map,
                args.drain().next().unwrap(),
            )))
        }
        _ => Err(LoxError::type_error(format!(
            "'{}' has no attribute 'set_default'",
            args.head.unwrap()
        ))),
    }
}
