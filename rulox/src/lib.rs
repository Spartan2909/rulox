//! # rulox
//!
//! [![crates.io](https://img.shields.io/badge/crates.io-fc8d62?style=for-the-badge&labelColor=555555&logo=rust)](https://crates.io/crates/rulox)
//! [![github](https://img.shields.io/badge/github-8da0cb?style=for-the-badge&labelColor=555555&logo=github)](https://github.com/Spartan2909/rulox)
//! [![docs.rs](https://img.shields.io/badge/docs.rs-66c2a5?style=for-the-badge&labelColor=555555&logo=docs.rs)](https://docs.rs/rulox/latest) <br>
//!
//! `rulox` is a lightweight scripting language embedded in Rust.
//! It is based on the Lox language from [Crafting Interpreters](https://craftinginterpreters.com/).
//!
//! # Examples
//! ```
//! use rulox::prelude::*;
//! # use rulox::LoxError;
//!
//! # fn main() -> Result<(), LoxError> {
//! lox! {
//!     var a = 5;
//!
//!     print a + 2;
//! }
//!
//! let b: f64 = a.get()?.try_into().unwrap();
//!
//! println!("{}", b);
//! # Ok(())
//! # }
//! ```
//!
//! ```
//! use rulox::prelude::*;
//! # use rulox::LoxError;
//!
//! # fn main() -> Result<(), LoxError> {
//! lox! {
//!     for (var i = 5; i > 0; i = i - 1) print i;
//! }
//! # Ok(())
//! # }
//! ```
//!
//! ```
//! use rulox::prelude::*;
//! # use rulox::LoxError;
//!
//! # fn main() -> Result<(), LoxError> {
//! lox! {
//!     fun hello(name) {
//!         print "Hello " + name + "! :)";
//!     }
//!
//!     fun add_one(num) {
//!         return num + 1;
//!     }
//!
//!     hello("Alice");
//! }
//!
//! hello.get()?.call([LoxValue::from("Bob")].into())?;
//!
//! assert_eq!(add_one.get()?.call([LoxValue::from(3)].into())?, LoxValue::from(4));
//! # Ok(())
//! # }
//! ```
//!
//! ```
//! use rulox::prelude::*;
//! # use rulox::LoxError;
//!
//! # fn main() -> Result<(), LoxError> {
//! lox! {
//!     var people = ["Bob", "Alice", "John"];
//!
//!     for (person in people) {
//!         print "Hello " + person + "!";
//!     }
//! }
//! # Ok(())
//! # }
//! ```

#![warn(missing_docs)]

/// Parses Lox code and converts it to Rust.
/// # Examples
/// ```
/// use rulox::prelude::*;
/// use rulox::LoxVariable;
/// # use rulox::LoxError;
///
/// # fn main() -> Result<(), LoxError> {
/// lox! {
///     var hello = "hello ";
/// }
///
/// let world = LoxVariable::new("world");
///
/// lox! {
///     print hello + world;
/// }
/// # Ok(())
/// # }
/// ```
pub use rulox_macro::lox;

/// Generates a rulox binding for a Rust function.
///
/// ## Examples
///
/// ```
/// use rulox::prelude::*;
/// use rulox::lox_bindgen;
/// # use rulox::LoxError;
///
/// fn hello(name: String) -> String {
///     "Hello ".to_string() + &name
/// }
///
/// # fn main() -> Result<(), LoxError> {
/// lox_bindgen!(fn hello(name) as lox_hello);
///
/// lox! {
///     print lox_hello("Alice");
/// }
/// # Ok(())
/// # }
/// ```
#[macro_export]
macro_rules! lox_bindgen {
    ( fn $rust_name:ident ( $( $arg:ident ),* ) as $lox_name:ident ) => {
        let $lox_name = __rulox_helpers::LoxVariable::new(LoxValue::function(__rulox_helpers::LoxFn::new(
            |mut args: $crate::LoxArgs| -> __rulox_helpers::LoxResult {
                let mut __drain = args.drain();
                $(
                    let $arg = __drain.next().unwrap();
                )*
                $crate::ToLoxResult::to_lox_result($rust_name( $( $arg.try_into()? ),* ))
            },
            vec![$( stringify!($arg) ),*]
        )));
    };
    ( async fn $rust_name:ident ( $( $arg:ident ),* ) as $lox_name:ident ) => {
        let $lox_name = __rulox_helpers::LoxVariable::new(LoxValue::coroutine(
            |mut args: $crate::LoxArgs| -> Box<dyn $crate::prelude::__rulox_helpers::Future<Output = $crate::LoxResult> + Send + Sync + 'static> {
                let mut __drain = args.drain();
                $(
                    let $arg = __drain.next().unwrap();
                )*
                Box::new(async {
                    $crate::ToLoxResult::to_lox_result($rust_name( $( $arg.try_into()? ),* ).await)
                })
            },
            vec![$( stringify!($arg) ),*]
        ));
    };
}

/// Generates a Rust binding for a rulox function.
///
/// ## Examples
///
/// ```
/// use rulox::prelude::*;
/// use rulox::rust_bindgen;
/// # use rulox::LoxError;
///
/// # fn main() -> Result<(), LoxError> {
/// lox! {
///     fun hello(name) {
///         return "Hello " + name + "!";
///     }
/// }
///
/// rust_bindgen!(fn hello(name: &str) -> String as rust_hello);
///
/// assert_eq!(rust_hello("Bob"), Ok("Hello Bob!".to_string()));
/// # Ok(())
/// # }
/// ```
#[macro_export]
macro_rules! rust_bindgen {
    ( fn $lox_name:ident ( $( $arg_name:ident : $arg_ty:ty ),* ) -> $ret_ty:ty as $rust_name:ident ) => {
        let $rust_name = {
            let $lox_name = $lox_name.close_over();
            move | $( $arg_name : $arg_ty ),* | -> $crate::prelude::__rulox_helpers::Result<$ret_ty, $crate::LoxError> {
                $lox_name.get()?.call([ $( $arg_name.into() ),* ].into())?.try_into()
            }
        };
    };
}

pub use rulox_macro::TryFromLoxValue;
pub use rulox_types::async_types::Coroutine;
pub use rulox_types::async_types::LoxFuture;
pub use rulox_types::Downcast;
pub use rulox_types::DynLoxObject;
pub use rulox_types::LoxArgs;
pub use rulox_types::LoxClass;
pub use rulox_types::LoxError;
pub use rulox_types::LoxFn;
pub use rulox_types::LoxObject;
pub use rulox_types::LoxResult;
pub use rulox_types::LoxValue;
pub use rulox_types::LoxVariable;

#[doc(hidden)]
pub use rulox_types::ToLoxResult;

/// Items that the [`lox`] macro expects to find in scope.
pub mod prelude {
    pub use crate::lox;
    pub use crate::LoxValue;

    #[doc(hidden)] // Not public API.
    pub mod __rulox_helpers {
        pub use crate::LoxArgs;
        pub use crate::LoxClass;
        pub use crate::LoxError;
        pub use crate::LoxFn;
        pub use crate::LoxObject;
        pub use crate::LoxResult;
        pub use crate::LoxValue;
        pub use crate::LoxVariable;
        pub use core::result::Result;
        pub use rulox_types::extract;
        pub use rulox_types::read;
        pub use rulox_types::write;
        pub use rulox_types::LoxRc;
        pub use rulox_types::Shared;
        pub use std::collections::HashMap;

        #[cfg(feature = "async")]
        pub use core::future::Future;
        #[cfg(feature = "async")]
        pub use rulox_types::async_types::Coroutine;
    }
}
