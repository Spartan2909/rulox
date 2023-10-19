use crate::ExternalError;
use crate::LoxErrorInner;
use crate::LoxResult;
use crate::Shared;
use crate::DynLoxObject;
use crate::LoxValue;
use crate::LoxRc;

use serde::Serialize;
use serde::Serializer;

pub(super) fn external_error<S: Serializer>(value: &ExternalError, serializer: S) -> Result<S::Ok, S::Error> {
    LoxErrorInner::Arbitrary(value.0.to_string()).serialize(serializer)
}

pub(super) fn primitive_method<S: Serializer>(_func: &fn(LoxValue) -> LoxResult, _value: &LoxValue, serializer: S) -> Result<S::Ok, S::Error> {
    LoxValue::Str(LoxRc::new("<bound method>".to_string())).serialize(serializer)
}

pub(super) fn external<S: Serializer>(_external: &Shared<DynLoxObject>, serializer: S) -> Result<S::Ok, S::Error> {
    LoxValue::Str(LoxRc::new("<external object>".to_string())).serialize(serializer)
}
