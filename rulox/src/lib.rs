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
//!
//! lox! {
//!     var a = 5;
//!
//!     print a + 2;
//! }
//!
//! let b: f64 = a.get().try_into().unwrap();
//!
//! println!("{}", b);
//! ```
//!
//! ```
//! use rulox::prelude::*;
//! lox! {
//!     for (var i = 5; i > 0; i = i - 1) print i;
//! }
//! ```
//!
//! ```
//! use rulox::prelude::*;
//!
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
//! hello.get().lox_call([LoxValue::from("Bob")].into());
//!
//! assert_eq!(add_one.get().lox_call([LoxValue::from(3)].into()), LoxValue::from(4));
//! ```
//!
//! ```
//! use rulox::prelude::*;
//!
//! lox! {
//!     var people = ["Bob", "Alice", "John"];
//!
//!     for (person in people) {
//!         print "Hello " + person + "!";
//!     }
//! }
//! ```

/// Parses Lox code and converts it to Rust.
/// # Examples
/// ```
/// use rulox::prelude::*;
/// use rulox::LoxVariable;
///
/// lox! {
///     var hello = "hello ";
/// }
///
/// let world = LoxVariable::new("world");
///
/// lox! {
///     print hello + world;
/// }
/// ```
///
/// # Panics
/// If an operation is attemped between two unsupported types.
pub use rulox_macro::lox;

/// Generates a rulox binding for a Rust function.
/// # Examples
/// ```
/// use rulox::prelude::*;
/// use rulox::lox_bindgen;
///
/// fn hello(name: String) -> String {
///     "Hello ".to_string() + &name
/// }
///
/// lox_bindgen!(fn hello(name) as lox_hello);
///
/// lox! {
///     print lox_hello("Alice");
/// }
/// ```
#[macro_export]
macro_rules! lox_bindgen {
    ( fn $rust_name:ident ( $( $arg:ident ),* ) as $lox_name:ident ) => {
        let $lox_name = __rulox_helpers::LoxVariable::new(
            LoxValue::function(__rulox_helpers::LoxFn::new(|mut args: $crate::LoxArgs| -> LoxValue {
                let mut _drain = args.drain();
                $(
                    let $arg = _drain.next().unwrap();
                )*
                $rust_name( $( $arg.try_into().unwrap() )* ).into()
                },
                vec![$( stringify!($arg) ),*]
            ))
        );
    };
}

pub use rulox_types::extract;
pub use rulox_types::LoxArgs;
pub use rulox_types::LoxCallable;
pub use rulox_types::LoxClass;
pub use rulox_types::LoxFn;
pub use rulox_types::LoxValue;
pub use rulox_types::LoxVariable;

pub mod prelude {
    pub use crate::lox;
    pub use crate::LoxCallable as _;
    pub use crate::LoxValue;

    #[doc(hidden)] // Not public API.
    pub mod __rulox_helpers {
        pub use crate::extract;
        pub use crate::LoxArgs;
        pub use crate::LoxClass;
        pub use crate::LoxFn;
        pub use crate::LoxVariable;
        pub use std::collections::HashMap;
        pub use std::rc::Rc;
    }
}
