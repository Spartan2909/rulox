mod request;
use request::new_request;

mod response;
use response::new_json_response;
use response::new_response;
use response::LoxResponse;

mod template;
use template::new_context;
use template::render_template;

use std::collections::HashMap;
use std::error;
use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;
use std::net::SocketAddr;
use std::sync::Arc;
use std::sync::Mutex;
use std::sync::OnceLock;
use std::sync::RwLock;

use http_body_util::Full;

use hyper::body::Bytes;
use hyper::body::Incoming;
use hyper::header::HeaderName;
use hyper::header::HeaderValue;
use hyper::http::status::InvalidStatusCode;
use hyper::server::conn::http1;
use hyper::service::service_fn;
use hyper::HeaderMap;
use hyper::Method;
use hyper::Request;
use hyper::Response;
use hyper::StatusCode;

use hyper_util::rt::TokioIo;

use rulox::lox_bindgen;
use rulox::prelude::*;
use rulox::Coroutine;
use rulox::Downcast;
use rulox::DynLoxObject;
use rulox::LoxError;
use rulox::LoxFn;
use rulox::LoxObject;
use rulox::LoxResult;
use rulox::LoxVariable;
use rulox::TryFromLoxValue;

use tera::Tera;

use tokio::net::TcpListener;

static TEMPLATES: OnceLock<Tera> = OnceLock::new();

#[derive(Clone)]
struct LoxHeaders(HeaderMap<HeaderValue>);

impl Debug for LoxHeaders {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(&self.0, f)
    }
}

impl TryFrom<LoxValue> for LoxHeaders {
    type Error = LoxError;

    fn try_from(value: LoxValue) -> Result<Self, Self::Error> {
        if value.clone().as_external().is_ok() {
            let this: Arc<RwLock<LoxHeaders>> = value.try_into()?;
            let this = this.read().unwrap().clone();
            Ok(this)
        } else {
            let headers: Result<_, LoxError> = value
                .as_map()?
                .read()
                .unwrap()
                .iter()
                .map(|(name, value)| {
                    Ok((
                        HeaderName::try_from(name.clone().into_inner().as_str()?.to_string())
                            .map_err(LoxError::external)?,
                        value
                            .as_str()?
                            .to_string()
                            .try_into()
                            .map_err(LoxError::external)?,
                    ))
                })
                .collect();
            Ok(LoxHeaders(headers?))
        }
    }
}

impl LoxObject for LoxHeaders {
    fn type_name() -> String
    where
        Self: Sized,
    {
        "Headers".to_string()
    }

    fn index(&self, key: LoxValue) -> Result<LoxValue, LoxError> {
        Ok(LoxValue::Str(Arc::new(
            self.0
                .get(&key.as_str()?.to_lowercase())
                .ok_or(LoxError::invalid_key(key))?
                .to_str()
                .map_err(LoxError::external)?
                .to_string(),
        )))
    }

    fn index_set(&mut self, key: LoxValue, value: LoxValue) -> Result<(), LoxError> {
        self.0.insert(
            HeaderName::try_from(key.as_str()?.as_str()).map_err(LoxError::external)?,
            value
                .as_str()?
                .to_string()
                .try_into()
                .map_err(LoxError::external)?,
        );
        Ok(())
    }
}

#[derive(Debug)]
enum Error {
    InvalidStatusCode(InvalidStatusCode),
    MethodAlreadySet(&'static str),
    InvalidGetParam(String),
    InvalidGetParams(LoxValue),
    TypeError { expected: String, found: String },
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::InvalidStatusCode(err) => Display::fmt(err, f),
            Error::MethodAlreadySet(method) => write!(f, "request method '{method}' already set"),
            Error::InvalidGetParam(param) => write!(f, "invalid GET parameter: '{param}'"),
            Error::InvalidGetParams(params) => write!(f, "invalid GET parameters: {params}"),
            Error::TypeError { expected, found } => {
                write!(f, "type error: expected '{expected}', found '{found}'")
            }
        }
    }
}

impl error::Error for Error {}

impl From<Error> for LoxError {
    fn from(value: Error) -> Self {
        LoxError::external(value)
    }
}

#[derive(Debug, Clone, TryFromLoxValue)]
struct Routes {
    route_handlers: HashMap<String, RouteHandler>,
    handler_404: Option<Arc<Coroutine>>,
}

impl Routes {
    fn add_route(routes: Arc<RwLock<Routes>>, path: String, handler: RouteHandler) {
        routes.write().unwrap().route_handlers.insert(path, handler);
    }

    fn set_handler_404(routes: Arc<RwLock<Routes>>, handler: Arc<Coroutine>) {
        routes.write().unwrap().handler_404 = Some(handler);
    }
}

impl LoxObject for Routes {
    fn type_name() -> String
    where
        Self: Sized,
    {
        "Routes".to_string()
    }
}

fn create_routes() -> LoxValue {
    LoxValue::external(Routes {
        route_handlers: HashMap::new(),
        handler_404: None,
    })
}

#[derive(Debug, Clone, TryFromLoxValue)]
struct RouteHandler {
    get: Option<Arc<Coroutine>>,
    post: Option<Arc<Coroutine>>,
}

fn get_handler(handler: Arc<RwLock<DynLoxObject>>) -> Result<Arc<RwLock<RouteHandler>>, LoxError> {
    Ok(handler
        .downcast()
        .map_err(|_| "expected a handler object")
        .unwrap())
}

macro_rules! handler {
    ($this:ident, $method:ident) => {
        LoxValue::Function(Arc::new(LoxFn::new(
            move |args| {
                let handler_fn: Arc<Coroutine> = args.get(0).unwrap().clone().try_into()?;
                let handler = get_handler(Arc::clone(&$this))?;
                if handler.read().unwrap().$method.is_some() {
                    Err(LoxError::external(Error::MethodAlreadySet(stringify!(
                        $method
                    ))))
                } else {
                    handler.write().unwrap().$method = Some(handler_fn);
                    Ok(LoxValue::Nil)
                }
            },
            vec!["handler"],
        )))
    };
}

impl LoxObject for RouteHandler {
    fn type_name() -> String
    where
        Self: Sized,
    {
        "Handler".to_string()
    }

    fn get(
        &self,
        this: Arc<RwLock<DynLoxObject>>,
        key: &'static str,
    ) -> Result<LoxValue, Option<LoxError>> {
        match key {
            "get" => Ok(handler!(this, get)),
            "post" => Ok(handler!(this, post)),
            _ => Err(None),
        }
    }
}

fn handler_get(handler: Arc<Coroutine>) -> LoxValue {
    LoxValue::external(RouteHandler {
        get: Some(handler),
        post: None,
    })
}

fn handler_post(handler: Arc<Coroutine>) -> LoxValue {
    LoxValue::external(RouteHandler {
        get: None,
        post: Some(handler),
    })
}

#[derive(Debug, Clone, TryFromLoxValue)]
struct Addr(SocketAddr);

impl LoxObject for Addr {
    fn type_name() -> String
    where
        Self: Sized,
    {
        "Addr".to_string()
    }
}

fn parse_addr(addr: String) -> LoxResult {
    Ok(LoxValue::external(Addr(
        addr.parse().map_err(LoxError::external)?,
    )))
}

async fn handle_method(
    handler: Option<&Arc<Coroutine>>,
    request: LoxValue,
    response: &mut Response<Full<Bytes>>,
) -> Result<(), LoxError> {
    match handler {
        Some(handler) => match handler.start([request].into()).await {
            Ok(value) => match value {
                LoxValue::Str(string) => {
                    *response.body_mut() = Full::new(Bytes::from(string.to_string()))
                }
                LoxValue::External(external)
                    if external.clone().downcast::<LoxResponse>().is_ok() =>
                {
                    let lox_response = external
                        .downcast::<LoxResponse>()
                        .unwrap_or_else(|_| unreachable!())
                        .read()
                        .unwrap()
                        .clone();

                    *response.body_mut() = Bytes::from(lox_response.body).into();
                    *response.status_mut() = lox_response.status_code;
                    for (header_name, header) in &lox_response.headers.read().unwrap().0 {
                        response.headers_mut().insert(
                            HeaderName::try_from(header_name).map_err(LoxError::external)?,
                            header.try_into().map_err(LoxError::external)?,
                        );
                    }
                }
                LoxValue::Nil => {}
                _ => *response.status_mut() = StatusCode::INTERNAL_SERVER_ERROR,
            },
            #[cfg(debug_assertions)]
            Err(mut err) => {
                *response.status_mut() = StatusCode::INTERNAL_SERVER_ERROR;
                err.push_trace("handle_request");
                let err = err.to_string();
                eprintln!("{err}");
                *response.body_mut() = Bytes::from(err).into();
            }
            #[cfg(not(debug_assertions))]
            Err(_) => {
                *response.status_mut() = StatusCode::INTERNAL_SERVER_ERROR;
            }
        },
        None => *response.status_mut() = StatusCode::METHOD_NOT_ALLOWED,
    }

    Ok(())
}

async fn handle_request(
    request: Request<Incoming>,
    routes: &Routes,
) -> Result<Response<Full<Bytes>>, LoxError> {
    let handler = match routes.route_handlers.get(request.uri().path()) {
        Some(route) => route,
        None => {
            if let Some(handler) = &routes.handler_404 {
                let mut response = Response::new(Bytes::new().into());
                *response.status_mut() = StatusCode::NOT_FOUND;
                let request = new_request(request).await?;
                handle_method(Some(handler), request, &mut response).await?;
                return Ok(response);
            } else {
                let mut response = Response::new("Page not found".into());
                *response.status_mut() = StatusCode::NOT_FOUND;
                return Ok(response);
            }
        }
    };

    let method = request.method().clone();
    let request = new_request(request).await?;

    let mut response = Response::new(Bytes::new().into());
    match method {
        Method::GET => handle_method(handler.get.as_ref(), request, &mut response).await?,
        Method::POST => handle_method(handler.post.as_ref(), request, &mut response).await?,
        _ => *response.status_mut() = StatusCode::METHOD_NOT_ALLOWED,
    }
    Ok(response)
}

async fn shutdown_signal() {
    tokio::signal::ctrl_c()
        .await
        .expect("failed to listen for ctrl-c");
}

async fn start_server(addr: Addr, routes: Routes, shutdown_signal: Arc<Coroutine>) -> LoxResult {
    let listener = TcpListener::bind(addr.0)
        .await
        .map_err(LoxError::external)?;
    let routes = Arc::new(routes);
    let shutdown_signal = shutdown_signal.start([].into());

    let (tx, rx) = tokio::sync::oneshot::channel();
    let tx = Arc::new(Mutex::new(Some(tx)));

    let server = async {
        loop {
            let routes = Arc::clone(&routes);
            let (stream, _) = listener.accept().await.map_err(LoxError::external)?;
            let tx = Arc::clone(&tx);

            let io = TokioIo::new(stream);

            tokio::task::spawn(async move {
                if let Err(err) = http1::Builder::new()
                    .serve_connection(
                        io,
                        service_fn(|request| async { handle_request(request, &routes).await }),
                    )
                    .await
                {
                    if let Some(tx) = tx.lock().unwrap().take() {
                        let _ = tx.send(LoxError::external(err));
                    }
                }
            });
        }
    };

    tokio::select! {
        r = server => r,
        r = shutdown_signal => r,
        e = rx => Err(e.unwrap()),
    }
}

/// Returns a module containing the following objects:
///
/// - `Router`: A class representing the routes of a server.
/// - `Server`: A class representing a server bound to a particular IP address
///     and port.
/// - `get`, `post`: Functions for turning async functions into request handlers
///     for the relevant request method.
/// - `Response`: A class representing an HTTP response from a handler.
/// - `JsonResponse`: A class representing an HTTP response with a body of JSON.
/// - `render`: A function that accepts a template path, the incoming request,
///     and a map or a `Context` object, and returns a response with a rendered
///     template.
/// - `Context`: A class created from a map of strings to values.
pub fn get_module(templates_dir: &str) -> Result<LoxVariable, LoxError> {
    TEMPLATES
        .set(Tera::new(templates_dir).map_err(LoxError::external)?)
        .ok();

    lox_bindgen! {
        fn create_routes();
        fn handler_get(handler);
        fn handler_post(handler);
        fn Routes::add_route(routes, path, handler) as add_route;
        fn Routes::set_handler_404(routes, handler) as set_handler_404;
        fn parse_addr(addr);
        fn new_response(body);
        fn new_json_response(body);
        fn render_template(name, request, context);
        fn new_context(value);
        async fn shutdown_signal();
        async fn start_server(addr, routes, shutdown_signal);
    }

    lox! {
        class Module {}

        var module = Module();

        class Router {
            init() {
                this.__routes = create_routes();
            }

            route(path, handler) {
                add_route(this.__routes, path, handler);
                return this;
            }

            handler_404(handler) {
                set_handler_404(this.__routes, handler);
                return this;
            }
        }

        class Server {
            init(addr) {
                this.__addr = parse_addr(addr);
                this.__shutdown = shutdown_signal;
            }

            set_shutdown_signal(signal) {
                this.__shutdown = signal;
            }

            async serve(router) {
                start_server(this.__addr, router.__routes, this.__shutdown).await;
            }
        }

        module.Router = Router;
        module.Server = Server;
        module.get = handler_get;
        module.post = handler_post;
        module.Response = new_response;
        module.JsonResponse = new_json_response;
        module.render = render_template;
        module.Context = new_context;
    }

    Ok(module)
}
