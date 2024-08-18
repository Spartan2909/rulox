use crate::LoxClass;
use crate::LoxError;
use crate::LoxInstance;
use crate::LoxValue;

use std::fmt;
use std::hash::Hash;
use std::hash::Hasher;

#[cfg(feature = "serde")]
use serde::Serialize;

pub(crate) fn hash_ptr<T: ?Sized, H: Hasher>(ptr: *const T, state: &mut H) {
    (ptr.cast::<()>()).hash(state);
}

impl Hash for LoxValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Self::Bool(b) => b.hash(state),
            Self::Str(s) => s.hash(state),
            &Self::Num(mut n) => {
                if n.is_nan() {
                    n = fastrand::f64();
                }
                let bits = n.to_bits();
                let sign: i8 = if bits >> 63 == 0 { 1 } else { -1 };
                let mut exponent = ((bits >> 52) & 0x7ff) as i16;
                let mantissa = if exponent == 0 {
                    (bits & 0x000f_ffff_ffff_ffff) << 1
                } else {
                    (bits & 0x000f_ffff_ffff_ffff) | 0x0010_0000_0000_0000
                };
                exponent -= 1023 + 52;
                (mantissa, exponent, sign).hash(state);
            }
            Self::Arr(arr) => arr.read().hash(state),
            Self::Function(func) => func.hash(state),
            Self::BoundMethod(func, _) => func.hash(state),
            Self::PrimitiveMethod(func, _) => func.hash(state),
            Self::Class(class) => class.hash(state),
            Self::Instance(instance) => instance.read().hash(state),
            Self::Map(_) => panic!("cannot hash a hashmap"),
            Self::Bytes(bytes) => bytes.hash(state),
            Self::Error(err) => err.hash(state),
            Self::Coroutine(func) => func.hash(state),
            Self::Future(fut) => fut.0.read().hash(state),
            Self::Nil => {}
            Self::External(external) => external.as_ptr().hash(state),
        }
    }
}

impl Hash for LoxClass {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.initialiser.hash(state);
        self.superclass.hash(state);
        let mut methods: Vec<_> = self
            .methods
            .iter()
            .map(|(&name, method)| (name, method))
            .collect();
        methods.sort_unstable_by(|(name1, _), (name2, _)| name1.cmp(name2));
        for method in methods {
            method.hash(state);
        }
    }
}

impl Hash for LoxInstance {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.class.hash(state);
        let mut attributes: Vec<_> = self.attributes.iter().collect();
        attributes.sort_unstable_by(|(name1, _), (name2, _)| name1.cmp(name2));
        for attributes in attributes {
            attributes.hash(state);
        }
    }
}

/// A hashmap key.
#[derive(Clone, PartialEq, PartialOrd, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
#[repr(transparent)]
pub struct MapKey(LoxValue);

impl MapKey {
    /// Creates a [`MapKey`] from a [`LoxValue`] if it is a valid key.
    ///
    /// ## Errors
    ///
    /// Returns an error if `self` is not a boolean, number, or string, or if
    /// `self` is `NaN`.
    pub fn verify_key(key: LoxValue) -> Result<MapKey, LoxError> {
        match key {
            LoxValue::Num(n) if n.is_nan() => Err(LoxError::invalid_key(key)),
            LoxValue::Bool(_) | LoxValue::Num(_) | LoxValue::Str(_) => Ok(MapKey(key)),
            _ => Err(LoxError::invalid_key(key)),
        }
    }

    /// Extracts the [`LoxValue`] from `self`.
    pub fn into_inner(self) -> LoxValue {
        self.0
    }

    /// Borrows the [`LoxValue`] in `self`.
    pub const fn as_inner(&self) -> &LoxValue {
        &self.0
    }
}

impl TryFrom<LoxValue> for MapKey {
    type Error = LoxError;

    fn try_from(value: LoxValue) -> Result<Self, Self::Error> {
        MapKey::verify_key(value)
    }
}

impl From<MapKey> for LoxValue {
    fn from(value: MapKey) -> Self {
        value.into_inner()
    }
}

impl fmt::Debug for MapKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}

impl Eq for MapKey {}
