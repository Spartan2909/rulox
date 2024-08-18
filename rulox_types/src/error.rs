use crate::LoxValue;
use crate::MapKey;

use std::collections::VecDeque;
use std::convert::Infallible;
use std::error::Error;
use std::fmt;
use std::fmt::Display;
use std::hash::Hash;
use std::sync::Arc;

#[cfg(feature = "serde")]
use serde::Serialize;
#[cfg(feature = "serde")]
use serde::Serializer;

#[cfg(feature = "serde")]
fn serialise_external_error<S: Serializer>(
    value: &ExternalError,
    serializer: S,
) -> Result<S::Ok, S::Error> {
    LoxErrorInner::Arbitrary(value.0.to_string()).serialize(serializer)
}

#[derive(Debug, Clone)]
struct ExternalError(Arc<dyn Error + Send + Sync>);

impl Hash for ExternalError {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Arc::as_ptr(&self.0).hash(state);
    }
}

impl PartialEq for ExternalError {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

/// An error raised during compilation or execution.
#[derive(Debug, Clone, PartialEq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct LoxError {
    inner: LoxErrorInner,
    trace: VecDeque<&'static str>,
}

impl LoxError {
    pub(crate) fn type_error(message: String) -> LoxError {
        LoxError {
            inner: LoxErrorInner::TypeError(message),
            trace: VecDeque::new(),
        }
    }

    pub(crate) fn undefined_variable(kind: &'static str) -> LoxError {
        LoxError {
            inner: LoxErrorInner::UndefinedVariable(kind),
            trace: VecDeque::new(),
        }
    }

    pub(crate) fn invalid_property(property: String, object: String) -> LoxError {
        LoxError {
            inner: LoxErrorInner::InvalidProperty { property, object },
            trace: VecDeque::new(),
        }
    }

    pub(crate) fn non_existent_super(name: String) -> LoxError {
        LoxError {
            inner: LoxErrorInner::NonExistentSuper(name),
            trace: VecDeque::new(),
        }
    }

    pub(crate) fn index_out_of_range(index: usize) -> LoxError {
        LoxError {
            inner: LoxErrorInner::IndexOutOfRange(index),
            trace: VecDeque::new(),
        }
    }

    /// Returns an error corresponding to an invalid key in a map or array.
    pub fn invalid_key(key: LoxValue) -> LoxError {
        LoxError {
            inner: LoxErrorInner::InvalidKey(Box::new(key)),
            trace: VecDeque::new(),
        }
    }

    #[doc(hidden)]
    pub fn incorrect_arity(expected: usize, found: usize) -> LoxError {
        LoxError {
            inner: LoxErrorInner::IncorrectArity { expected, found },
            trace: VecDeque::new(),
        }
    }

    pub(crate) fn overflow_error(value: MapKey, target_type: &'static str) -> LoxError {
        LoxError {
            inner: LoxErrorInner::OverflowError {
                value: Box::new(value),
                target_type,
            },
            trace: VecDeque::new(),
        }
    }

    /// Creates an error out of a [`LoxValue`], which can be recovered in an
    /// `except` block.
    pub fn value(value: LoxValue) -> LoxError {
        LoxError {
            inner: LoxErrorInner::Value(Box::new(value)),
            trace: VecDeque::new(),
        }
    }

    pub(super) fn not_implemented<T: ToString>(method_name: &str, kind: &T) -> LoxError {
        LoxError::type_error(format!(
            "The method '{method_name}' is not implemented for '{}'",
            kind.to_string()
        ))
    }

    pub(crate) fn finished_coroutine() -> LoxError {
        LoxError {
            inner: LoxErrorInner::FinishedCoroutine,
            trace: VecDeque::new(),
        }
    }

    #[doc(hidden)]
    pub fn pass() -> LoxError {
        LoxError {
            inner: LoxErrorInner::ControlFlow,
            trace: VecDeque::new(),
        }
    }

    fn filter_pass(self) -> Option<LoxError> {
        if matches!(&self.inner, LoxErrorInner::ControlFlow) {
            None
        } else {
            Some(self)
        }
    }

    #[doc(hidden)]
    pub const fn is_pass(&self) -> bool {
        matches!(&self.inner, LoxErrorInner::ControlFlow)
    }

    #[doc(hidden)]
    pub fn result_filter_pass(r: Result<LoxValue, LoxError>) -> Option<Result<LoxValue, LoxError>> {
        match r {
            Ok(v) => Some(Ok(v)),
            Err(e) => e.filter_pass().map(Err),
        }
    }

    /// Pushes a new function to this error's stack trace.
    #[cold]
    pub fn push_trace(&mut self, value: &'static str) {
        self.trace.push_front(value);
    }

    #[doc(hidden)] // Not public API.
    pub fn push_trace_front(&mut self, value: &'static str) {
        self.trace.push_front(value);
    }

    #[doc(hidden)] // Not public API.
    #[must_use]
    pub fn with_trace(mut self, trace: Vec<&'static str>) -> LoxError {
        self.trace = trace.into();
        self
    }

    #[doc(hidden)] // Not public API.
    pub fn into_value(self) -> LoxValue {
        if let LoxErrorInner::Value(value) = self.inner {
            *value
        } else {
            LoxValue::Error(self)
        }
    }

    /// Creates a [`LoxError`] from another error.
    pub fn external<E: Error + Send + Sync + 'static>(value: E) -> LoxError {
        LoxError {
            inner: LoxErrorInner::External(ExternalError(Arc::new(value))),
            trace: VecDeque::new(),
        }
    }
}

impl From<Infallible> for LoxError {
    fn from(value: Infallible) -> Self {
        match value {}
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
enum LoxErrorInner {
    /// An error that occurs when attempting to use a `LoxValue` with an invalid type.
    TypeError(String),
    /// An error that occurs when attempting to convert a `LoxValue` into a Rust type that is too small.
    OverflowError {
        value: Box<MapKey>,
        target_type: &'static str,
    },
    /// An error that occurs when variables are accessed without first being defined.
    UndefinedVariable(&'static str),
    InvalidProperty {
        property: String,
        object: String,
    },
    NonExistentSuper(String),
    IndexOutOfRange(usize),
    InvalidKey(Box<LoxValue>),
    IncorrectArity {
        expected: usize,
        found: usize,
    },
    Value(Box<LoxValue>),
    #[cfg_attr(feature = "serde", serde(serialize_with = "serialise_external_error"))]
    External(ExternalError),
    #[cfg(feature = "serde")]
    #[doc(hidden)]
    Arbitrary(String),
    FinishedCoroutine,
    ControlFlow,
}

impl Display for LoxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "traceback (most recent call last):")?;
        for &fun in &self.trace {
            writeln!(f, "in '{fun}'")?;
        }
        writeln!(f)?;

        match &self.inner {
            LoxErrorInner::TypeError(message) => {
                write!(f, "{message}")
            }
            LoxErrorInner::OverflowError { value, target_type } => {
                write!(
                    f,
                    "could not convert '{}' to {target_type} (value too large)",
                    value.as_inner()
                )
            }
            LoxErrorInner::UndefinedVariable(name) => write!(f, "undefined variable '{name}'"),
            LoxErrorInner::InvalidProperty { property, object } => {
                write!(f, "invalid property '{property}' on {object}")
            }
            LoxErrorInner::NonExistentSuper(name) => {
                write!(f, "function '{name}' has no super function")
            }
            LoxErrorInner::IndexOutOfRange(index) => write!(f, "index out of range: {index}"),
            LoxErrorInner::InvalidKey(key) => write!(f, "invalid key: {key}"),
            LoxErrorInner::IncorrectArity { expected, found } => {
                write!(f, "expected {expected} arguments, found {found}")
            }
            LoxErrorInner::Value(value) => {
                write!(f, "error: {value}")
            }
            LoxErrorInner::External(err) => fmt::Display::fmt(&err.0, f),
            #[cfg(feature = "serde")]
            LoxErrorInner::Arbitrary(string) => f.write_str(string),
            LoxErrorInner::FinishedCoroutine => write!(f, "cannot await a finished coroutine"),
            LoxErrorInner::ControlFlow => unreachable!(),
        }
    }
}

impl Error for LoxError {}
