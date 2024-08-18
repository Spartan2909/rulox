use super::response;
use super::Error;
use super::LoxHeaders;

use std::collections::HashMap;
use std::fmt;
use std::sync::Arc;
use std::sync::Mutex;

use http_body_util::BodyExt;

use hyper::body::Body;
use hyper::body::Bytes;
use hyper::body::Incoming;
use hyper::Method;
use hyper::Request;
use hyper::StatusCode;

use rulox::lox_bindgen;
use rulox::prelude::*;
use rulox::DynLoxObject;
use rulox::LoxError;
use rulox::LoxObject;
use rulox::LoxValue;
use rulox::Shared;
use rulox::TryFromLoxValue;
use rulox::Upcast;

use tokio::runtime::Handle;

#[derive(Clone, Default, TryFromLoxValue)]
struct LoxParamDict {
    dict: HashMap<Arc<str>, Arc<[Arc<str>]>>,
}

impl fmt::Debug for LoxParamDict {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.dict, f)
    }
}

impl LoxParamDict {
    fn parse(value: &str) -> Option<LoxParamDict> {
        let mut map: HashMap<Arc<str>, Vec<Arc<str>>> = HashMap::new();

        for param in value.split('&') {
            let mut iter = param.split('=');
            let name = iter.next()?;
            let value = iter.next()?;
            if iter.next().is_some() {
                return None;
            }
            if let Some(entry) = map.get_mut(name) {
                entry.push(value.into());
            } else {
                map.insert(name.into(), vec![value.into()]);
            }
        }

        Some(LoxParamDict {
            dict: map.into_iter().map(|(k, v)| (k, v.into())).collect(),
        })
    }
}

impl LoxObject for LoxParamDict {
    fn type_name() -> String
    where
        Self: Sized,
    {
        "ParamDict".to_string()
    }

    fn get(&self, this: Shared<DynLoxObject>, key: &str) -> Result<LoxValue, Option<LoxError>> {
        if key == "get_or" {
            let get_or = move |key: LoxValue, default: LoxValue| -> Result<LoxValue, LoxError> {
                let this: Shared<LoxParamDict> = this.clone().downcast().unwrap();
                let this = this.read();
                let value = this.dict.get(&**key.expect_str()?);
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
        let param_view = if let Some(param_view) = self.dict.get(&**key.expect_str()?) {
            LoxParamView(param_view.clone())
        } else {
            return Err(LoxError::invalid_key(key));
        };

        Ok(LoxValue::external(param_view))
    }
}

#[derive(Clone)]
struct LoxParamView(Arc<[Arc<str>]>);

impl fmt::Debug for LoxParamView {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}

impl LoxObject for LoxParamView {
    fn type_name() -> String
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

enum RequestBody {
    Bytes(Bytes),
    Incoming(Option<Incoming>),
}

impl fmt::Debug for RequestBody {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let RequestBody::Bytes(bytes) = self {
            write!(f, "{bytes:?}")
        } else {
            f.write_str("<incoming>")
        }
    }
}

pub struct LoxRequest {
    headers: Shared<LoxHeaders>,
    body: Mutex<RequestBody>,
    uri: Uri,
    method: Method,
}

impl fmt::Debug for LoxRequest {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Request")
            .field("headers", &*self.headers.read())
            .field("body", &self.body)
            .field("path", &self.uri.path)
            .field("query", &self.uri.query)
            .field("method", &self.method)
            .finish()
    }
}

impl LoxObject for LoxRequest {
    fn type_name() -> String
    where
        Self: Sized,
    {
        "Request".to_string()
    }

    fn get(&self, _this: Shared<DynLoxObject>, key: &str) -> Result<LoxValue, Option<LoxError>> {
        match key {
            "headers" => Ok(LoxValue::External(self.headers.clone().upcast())),
            "body" => {
                let mut body = self.body.lock().unwrap();
                match &mut *body {
                    RequestBody::Bytes(bytes) => Ok(bytes.clone().into()),
                    RequestBody::Incoming(incoming) => {
                        let incoming = incoming.as_mut().unwrap();
                        let upper = incoming.size_hint().upper().unwrap_or(u64::MAX);
                        if upper > 1024 * 64 {
                            let mut resp = response::new_response("Body too big".to_string());
                            resp.status_code = StatusCode::PAYLOAD_TOO_LARGE;
                            Err(Some(LoxError::value(LoxValue::external(resp))))
                        } else {
                            let handle = Handle::current();
                            let bytes = handle
                                .block_on(incoming.collect())
                                .map_err(LoxError::external)?
                                .to_bytes();
                            *body = RequestBody::Bytes(bytes.clone());
                            Ok(bytes.into())
                        }
                    }
                }
            }
            "path" => Ok(self.uri.path.to_owned().into()),
            "GET" => Ok(LoxValue::external(
                self.uri.query.clone().unwrap_or_default(),
            )),
            "method" => Ok(LoxValue::Str(self.method.to_string().into())),
            _ => Err(None),
        }
    }

    fn set(
        &mut self,
        _this: rulox::Shared<DynLoxObject>,
        key: &str,
        value: LoxValue,
    ) -> Result<(), Option<LoxError>> {
        match key {
            "headers" => {
                self.headers = Shared::new(value.try_into()?);
                Ok(())
            }
            "body" => {
                self.body = RequestBody::Bytes(value.try_into()?).into();
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
                self.method = Method::from_bytes(value.expect_str()?.as_bytes())
                    .map_err(LoxError::external)?;
                Ok(())
            }
            _ => Err(None),
        }
    }
}

pub(super) async fn new_request(value: Request<Incoming>) -> Result<LoxValue, LoxError> {
    let (parts, body) = value.into_parts();

    let request = LoxRequest {
        headers: Shared::new(LoxHeaders(parts.headers)),
        body: Mutex::new(RequestBody::Incoming(Some(body))),
        uri: parts.uri.try_into().map_err(LoxError::external)?,
        method: parts.method,
    };

    Ok(LoxValue::external(request))
}
