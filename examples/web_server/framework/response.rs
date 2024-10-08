use super::Error;
use super::LoxHeaders;

use std::collections::HashMap;

use hyper::HeaderMap;
use hyper::StatusCode;

use rulox::hashmap_to_json_map;
use rulox::prelude::*;
use rulox::LoxError;
use rulox::LoxObject;
use rulox::MapKey;
use rulox::Shared;
use rulox::TryFromLoxValue;

use serde_json::Value;

#[derive(Debug, Clone, TryFromLoxValue)]
pub struct LoxResponse {
    pub(super) status_code: StatusCode,
    pub(super) body: String,
    pub(super) headers: Shared<LoxHeaders>,
}

fn f64_to_status_code(num: f64) -> Result<StatusCode, LoxError> {
    if num.fract() != 0.0 {
        return Err(Error::InvalidStatusCode(StatusCode::from_u16(0).unwrap_err()).into());
    }

    if num > u16::MAX as f64 {
        return Err(Error::InvalidStatusCode(StatusCode::from_u16(0).unwrap_err()).into());
    }

    Ok(StatusCode::from_u16(0).map_err(Error::InvalidStatusCode)?)
}

impl LoxObject for LoxResponse {
    fn type_name() -> String
    where
        Self: Sized,
    {
        "Response".to_string()
    }

    fn get(&self, _this: Shared<dyn LoxObject>, key: &str) -> Result<LoxValue, Option<LoxError>> {
        match key {
            "status_code" => Ok(self.status_code.as_u16().into()),
            "body" => Ok(self.body.clone().into()),
            "headers" => Ok(LoxValue::External(self.headers.clone().upcast())),
            _ => Err(None),
        }
    }

    fn set(
        &mut self,
        _this: Shared<dyn LoxObject>,
        key: &str,
        value: LoxValue,
    ) -> Result<(), Option<LoxError>> {
        match key {
            "status_code" => {
                self.status_code = f64_to_status_code(value.expect_num()?)?;
                Ok(())
            }
            "body" => {
                self.body = value.expect_str()?.to_string();
                Ok(())
            }
            "headers" => {
                self.headers = Shared::new(value.try_into()?);
                Ok(())
            }
            _ => Err(None),
        }
    }
}

fn response_from_body_headers<const N: usize>(
    body: String,
    headers: [(&str, &str); N],
) -> Result<LoxResponse, LoxError> {
    let headers: Result<HeaderMap<_>, LoxError> = headers
        .into_iter()
        .map(|(key, value)| {
            Ok((
                key.try_into().map_err(LoxError::external)?,
                value.try_into().map_err(LoxError::external)?,
            ))
        })
        .collect();
    Ok(LoxResponse {
        status_code: StatusCode::OK,
        body,
        headers: Shared::new(LoxHeaders(headers?)),
    })
}

pub fn new_response(body: String) -> LoxResponse {
    response_from_body_headers(body, [("Content-Type", "text/html")]).unwrap()
}

pub fn new_json_response(body: Shared<HashMap<MapKey, LoxValue>>) -> Result<LoxResponse, LoxError> {
    response_from_body_headers(
        Value::Object(hashmap_to_json_map(&body.read())?).to_string(),
        [("Content-Type", "application/json")],
    )
}
