use super::request::LoxRequest;
use super::response::new_response;
use super::Error;
use super::TEMPLATES;

use std::collections::HashMap;
use std::fmt;
use std::fmt::Debug;
use std::mem;
use std::sync::Arc;
use std::sync::RwLock;

use rulox::hashmap_to_json_map;
use rulox::lox_bindgen;
use rulox::prelude::*;
use rulox::Downcast;
use rulox::DynLoxObject;
use rulox::LoxError;
use rulox::LoxObject;
use rulox::LoxValue;
use rulox::MapKey;

use serde_json::Map;
use serde_json::Value;

use tera::Context;
use tera::Filter;
use tera::Function;
use tera::Tera;
use tera::Test;

struct LoxFilter(Box<dyn Filter>);

impl Filter for LoxFilter {
    fn filter(&self, value: &Value, args: &HashMap<String, Value>) -> tera::Result<Value> {
        self.0.filter(value, args)
    }

    fn is_safe(&self) -> bool {
        self.0.is_safe()
    }
}

struct LoxTest(Box<dyn Test>);

impl Test for LoxTest {
    fn test(&self, value: Option<&Value>, args: &[Value]) -> tera::Result<bool> {
        self.0.test(value, args)
    }
}

struct LoxFunction(Box<dyn Function>);

impl Function for LoxFunction {
    fn call(&self, args: &HashMap<String, Value>) -> tera::Result<Value> {
        self.0.call(args)
    }

    fn is_safe(&self) -> bool {
        self.0.is_safe()
    }
}

pub struct LoxContext {
    context: Map<String, Value>,
    filters: HashMap<String, LoxFilter>,
    tests: HashMap<String, LoxTest>,
    functions: HashMap<String, LoxFunction>,
}

impl LoxContext {
    fn apply(&mut self, templates: &Tera) -> Result<(Tera, Context), LoxError> {
        let mut tera = Tera::default();
        tera.extend(templates).map_err(LoxError::external)?;
        for (name, filter) in mem::take(&mut self.filters) {
            tera.register_filter(&name, filter);
        }
        for (name, tester) in mem::take(&mut self.tests) {
            tera.register_tester(&name, tester)
        }
        for (name, function) in mem::take(&mut self.functions) {
            tera.register_function(&name, function);
        }
        Ok((
            tera,
            Context::from_serialize(&self.context).map_err(LoxError::external)?,
        ))
    }
}

impl Debug for LoxContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Context")
            .field("context", &self.context)
            .finish()
    }
}

impl LoxObject for LoxContext {
    fn type_name() -> String
    where
        Self: Sized,
    {
        "Context".to_string()
    }

    fn get(
        &self,
        this: Arc<RwLock<DynLoxObject>>,
        key: &'static str,
    ) -> Result<LoxValue, Option<LoxError>> {
        let this: Arc<RwLock<LoxContext>> =
            this.clone().downcast().unwrap_or_else(|_| unreachable!());
        match key {
            "filter" => {
                let filter = move |name: String, fun: LoxValue| {
                    this.write().unwrap().filters.insert(
                        name,
                        LoxFilter(Box::new(
                            move |input: &Value,
                                  args: &HashMap<String, Value>|
                                  -> Result<Value, tera::Error> {
                                let args: Result<HashMap<MapKey, LoxValue>, LoxError> = args
                                    .iter()
                                    .map(|(key, value)| {
                                        Ok((
                                            MapKey::verify_key(key.to_owned().into())?,
                                            value.try_into()?,
                                        ))
                                    })
                                    .collect();

                                let value = fun
                                    .call(
                                        [
                                            input.try_into().map_err(tera::Error::msg)?,
                                            LoxValue::Map(Arc::new(
                                                args.map_err(tera::Error::msg)?.into(),
                                            )),
                                        ]
                                        .into(),
                                    )
                                    .map_err(tera::Error::msg)?;

                                value.try_into().map_err(tera::Error::msg)
                            },
                        )),
                    );
                };
                lox_bindgen!(fn filter(name, value));
                Ok(filter.get()?)
            }
            "test" => {
                let test = move |name: String, function: LoxValue| {
                    this.write().unwrap().tests.insert(
                        name,
                        LoxTest(Box::new(
                            move |input: Option<&Value>,
                                  args: &[Value]|
                                  -> Result<bool, tera::Error> {
                                let input: Option<LoxValue> = input
                                    .map(TryInto::try_into)
                                    .transpose()
                                    .map_err(tera::Error::msg)?;
                                let args = [
                                    input.unwrap_or(LoxValue::Nil),
                                    LoxValue::arr(
                                        args.iter()
                                            .map(|value| {
                                                LoxValue::try_from(value).map_err(tera::Error::msg)
                                            })
                                            .collect::<Result<Vec<_>, _>>()?,
                                    ),
                                ]
                                .into();
                                function
                                    .call(args)
                                    .map(|value| value.is_truthy())
                                    .map_err(tera::Error::msg)
                            },
                        )),
                    );
                };
                lox_bindgen!(fn test(name, function));
                Ok(test.get()?)
            }
            "function" => {
                let function = move |name: String, function: LoxValue| {
                    this.write().unwrap().functions.insert(
                        name,
                        LoxFunction(Box::new(
                            move |args: &HashMap<String, Value>| -> Result<Value, tera::Error> {
                                let args = [LoxValue::Map(Arc::new(RwLock::new(
                                    args.iter()
                                        .map(|(key, value)| {
                                            Ok((
                                                MapKey::verify_key(key.to_owned().into()).unwrap(),
                                                LoxValue::try_from(value)
                                                    .map_err(tera::Error::msg)?,
                                            ))
                                        })
                                        .collect::<Result<HashMap<MapKey, LoxValue>, tera::Error>>(
                                        )?,
                                )))]
                                .into();
                                function
                                    .call(args)
                                    .and_then(|value| value.try_into())
                                    .map_err(tera::Error::msg)
                            },
                        )),
                    );
                };
                lox_bindgen!(fn function(name, func));
                Ok(function.get()?)
            }
            _ => Err(None),
        }
    }
}

fn into_context(value: LoxValue) -> Result<Arc<RwLock<LoxContext>>, LoxError> {
    if let LoxValue::External(external) = value {
        external.downcast::<LoxContext>().map_err(|external| {
            Error::IncorrectType {
                expected: LoxContext::type_name(),
                found: external.read().unwrap().representation(),
            }
            .into()
        })
    } else {
        Ok(Arc::new(
            LoxContext {
                context: hashmap_to_json_map(&value.expect_map()?.read().unwrap())?,
                filters: HashMap::new(),
                tests: HashMap::new(),
                functions: HashMap::new(),
            }
            .into(),
        ))
    }
}

pub(super) fn new_context(value: LoxValue) -> Result<LoxValue, LoxError> {
    let context: Arc<RwLock<DynLoxObject>> = into_context(value)?;
    Ok(LoxValue::External(context))
}

pub(super) fn render_template(
    name: Arc<String>,
    _request: Arc<RwLock<LoxRequest>>,
    context: LoxValue,
) -> Result<LoxValue, LoxError> {
    let context = into_context(context)?;
    let (tera, context) = context.write().unwrap().apply(TEMPLATES.get().unwrap())?;
    let body = tera.render(&name, &context).map_err(LoxError::external)?;
    Ok(LoxValue::external(new_response(body)))
}
