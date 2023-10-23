use super::Error;
use super::LoxHeaders;
use super::ValidatedRequest;

use std::collections::HashMap;
use std::fmt;
use std::fmt::Debug;
use std::sync::Arc;
use std::sync::RwLock;

use hyper::body::Bytes;
use hyper::Method;

use rulox::lox_bindgen;
use rulox::prelude::*;
use rulox::Downcast;
use rulox::DynLoxObject;
use rulox::LoxError;
use rulox::LoxObject;
use rulox::LoxValue;
use rulox::TryFromLoxValue;

#[derive(Clone, Default, TryFromLoxValue)]
struct LoxParamDict {
    dict: HashMap<Arc<String>, Arc<Vec<Arc<String>>>>,
}

impl Debug for LoxParamDict {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(&self.dict, f)
    }
}

impl LoxParamDict {
    fn parse(value: &str) -> Option<LoxParamDict> {
        let mut map: HashMap<String, Vec<Arc<String>>> = HashMap::new();

        for param in value.split('&') {
            let mut iter = param.split('=');
            let name = iter.next()?;
            let value = iter.next()?;
            if iter.next().is_some() {
                return None;
            }
            if let Some(entry) = map.get_mut(name) {
                entry.push(Arc::new(value.to_string()));
            } else {
                map.insert(name.to_string(), vec![Arc::new(value.to_string())]);
            }
        }

        let mut params = HashMap::with_capacity(map.len());
        for (key, value) in map {
            params.insert(Arc::new(key), Arc::new(value));
        }

        Some(LoxParamDict { dict: params })
    }
}

impl LoxObject for LoxParamDict {
    fn name() -> String
    where
        Self: Sized,
    {
        "ParamDict".to_string()
    }

    fn get(
        &self,
        this: rulox::Shared<DynLoxObject>,
        key: &'static str,
    ) -> Result<LoxValue, Option<LoxError>> {
        if key == "get_or" {
            let get_or = move |key: LoxValue, default: LoxValue| -> Result<LoxValue, LoxError> {
                let this: Arc<RwLock<LoxParamDict>> = this.clone().downcast().unwrap();
                let this = this.read().unwrap();
                let value = this.dict.get(&key.as_str()?);
                if let Some(values) = value {
                    Ok(LoxValue::Str(values.first().unwrap().clone()))
                } else {
                    Ok(default)
                }
            };
            lox_bindgen!(fn get_or(key, default));
            Ok(get_or.get()?)
        } else {
            Err(None)
        }
    }

    fn index(&self, key: LoxValue) -> Result<LoxValue, LoxError> {
        let param_view = if let Some(param_view) = self.dict.get(&key.as_str()?) {
            LoxParamView(param_view.clone())
        } else {
            return Err(LoxError::invalid_key(key));
        };

        Ok(LoxValue::external(param_view))
    }
}

#[derive(Clone)]
struct LoxParamView(Arc<Vec<Arc<String>>>);

impl Debug for LoxParamView {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(&self.0, f)
    }
}

impl LoxObject for LoxParamView {
    fn name() -> String
    where
        Self: Sized,
    {
        "ParamView".to_string()
    }

    fn index(&self, key: LoxValue) -> Result<LoxValue, LoxError> {
        self.0
            .get(usize::try_from(&key)?)
            .map(|value| LoxValue::Str(value.clone()))
            .ok_or(LoxError::invalid_key(key))
    }
}

struct Uri {
    path: String,
    query: Option<LoxParamDict>,
}

impl TryFrom<hyper::Uri> for Uri {
    type Error = Error;

    fn try_from(value: hyper::Uri) -> Result<Self, Self::Error> {
        let parts = value.into_parts();
        let (path, query) = if let Some(path_and_query) = parts.path_and_query {
            (
                path_and_query.path().to_string(),
                path_and_query
                    .query()
                    .map(|query| {
                        LoxParamDict::parse(query).ok_or(Error::InvalidGetParam(query.to_string()))
                    })
                    .transpose()?,
            )
        } else {
            (String::new(), None)
        };
        Ok(Uri { path, query })
    }
}

pub struct LoxRequest {
    headers: Arc<RwLock<LoxHeaders>>,
    body: Bytes,
    uri: Uri,
    method: Method,
}

impl Debug for LoxRequest {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Request")
            .field("headers", &self.headers.read().unwrap())
            .field("body", &self.body)
            .field("path", &self.uri.path)
            .field("query", &self.uri.query)
            .field("method", &self.method)
            .finish()
    }
}

impl LoxObject for LoxRequest {
    fn name() -> String
    where
        Self: Sized,
    {
        "Request".to_string()
    }

    fn get(
        &self,
        _this: Arc<RwLock<DynLoxObject>>,
        key: &'static str,
    ) -> Result<LoxValue, Option<LoxError>> {
        match key {
            "headers" => Ok(LoxValue::External(self.headers.clone())),
            "body" => Ok(self.body.clone().into()),
            "path" => Ok(self.uri.path.to_owned().into()),
            "GET" => Ok(LoxValue::external(
                self.uri.query.clone().unwrap_or_default(),
            )),
            "method" => Ok(LoxValue::Str(Arc::new(self.method.to_string()))),
            _ => Err(None),
        }
    }

    fn set(
        &mut self,
        _this: rulox::Shared<DynLoxObject>,
        key: &'static str,
        value: LoxValue,
    ) -> Result<(), Option<LoxError>> {
        match key {
            "headers" => {
                self.headers = Arc::new(RwLock::new(value.try_into()?));
                Ok(())
            }
            "body" => {
                self.body = value.try_into()?;
                Ok(())
            }
            "path" => {
                self.uri.path = value.try_into()?;
                Ok(())
            }
            "GET" => match value {
                LoxValue::External(_) => {
                    let query: LoxParamDict = value.try_into()?;
                    self.uri.query = Some(query);
                    Ok(())
                }
                LoxValue::Str(query) => {
                    self.uri.query = Some(LoxParamDict::parse(&query).ok_or(
                        LoxError::external(Error::InvalidGetParam(query.to_string())),
                    )?);
                    Ok(())
                }
                value => Err(Some(LoxError::external(Error::InvalidGetParams(value)))),
            },
            "method" => {
                self.method =
                    Method::from_bytes(value.as_str()?.as_bytes()).map_err(LoxError::external)?;
                Ok(())
            }
            _ => Err(None),
        }
    }
}

pub(super) async fn new_request(value: ValidatedRequest) -> Result<LoxValue, LoxError> {
    let (parts, body) = value.0.into_parts();

    let request = LoxRequest {
        headers: Arc::new(RwLock::new(LoxHeaders(parts.headers))),
        body: hyper::body::to_bytes(body)
            .await
            .map_err(LoxError::external)?,
        uri: parts.uri.try_into().map_err(LoxError::external)?,
        method: parts.method,
    };

    Ok(LoxValue::external(request))
}
