use std::{collections::HashMap, error::Error, fmt};

#[derive(Debug)]
pub struct TypeError {
    expected: Vec<LoxValueType>,
    found: LoxValueType,
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "expected one of {:?}, found {:?}",
            self.expected, self.found
        )
    }
}

impl Error for TypeError {}

impl TypeError {
    fn new_single(expected: LoxValueType, found: LoxValueType) -> Self {
        Self {
            expected: vec![expected],
            found,
        }
    }
}

#[derive(Debug)]
pub enum LoxValueType {
    Str,
    Num,
    Arr,
    Instance,
}

impl From<LoxValue> for LoxValueType {
    fn from(value: LoxValue) -> Self {
        match value {
            LoxValue::Str { .. } => Self::Str,
            LoxValue::Num { .. } => Self::Num,
            LoxValue::Arr { .. } => Self::Arr,
            LoxValue::Instance { .. } => Self::Instance,
        }
    }
}

#[derive(Debug)]
pub enum LoxValue {
    Str {
        value: String,
    },
    Num {
        value: f64,
    },
    Arr {
        value: Vec<LoxValue>,
    },
    Instance {
        values: HashMap<String, LoxValue>,
        functions: Vec<String>,
    },
}

impl From<String> for LoxValue {
    fn from(value: String) -> Self {
        Self::Str { value }
    }
}

impl From<&str> for LoxValue {
    fn from(value: &str) -> Self {
        Self::from(value.to_string())
    }
}

macro_rules! impl_convert_num_to_loxvalue {
    ( $($t:ty),* ) => {
    $( impl From<$t> for LoxValue {
        fn from(value: $t) -> Self {
            Self::Num { value: value as f64 }
        }
    }) *
    };
}

impl_convert_num_to_loxvalue! { f32, f64, u8, u16, u32, u64, u128, i8, i16, i32, i64, i128 }

impl TryInto<String> for LoxValue {
    type Error = TypeError;

    fn try_into(self) -> Result<String, Self::Error> {
        if let LoxValue::Str { value } = self {
            return Ok(value);
        }

        Err(TypeError::new_single(
            LoxValueType::Str,
            LoxValueType::from(self),
        ))
    }
}
