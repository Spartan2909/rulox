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
//! use rulox::LoxError;
//!
//! fn main() -> Result<(), LoxError> {
//!     lox! {
//!         var a = 5;
//!
//!         print a + 2;
//!     }
//!
//!     let b: f64 = a.get()?.try_into().unwrap();
//!
//!     println!("{}", b);
//!
//!     Ok(())
//! }
//! ```
//!
//! ```
//! # use rulox::prelude::*;
//! # use rulox::LoxError;
//! # fn main() -> Result<(), LoxError> {
//! lox! {
//!     for (var i = 5; i > 0; i = i - 1) print i;
//! }
//! # Ok(())
//! # }
//! ```
//!
//! ```
//! # use rulox::prelude::*;
//! # use rulox::LoxError;
//! # fn main() -> Result<(), LoxError> {
//! lox! {
//!    fun hello(name) {
//!         print "Hello " + name + "! :)";
//!    }
//!
//!     fun add_one(num) {
//!         return num + 1;
//!     }
//! }
//!
//! hello.get()?.call([LoxValue::from("Alice")].into());
//!
//! assert_eq!(add_one.get()?.call([LoxValue::from(3)].into())?, 4);
//! # Ok(())
//! # }
//! ```
//!
//! ```
//! # use rulox::prelude::*;
//! # use rulox::LoxError;
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
//!
//! ```
//! # use rulox::prelude::*;
//! # use rulox::LoxError;
//! # fn main() -> Result<(), LoxError> {
//! lox! {
//!     class Person {
//!         init(name) {
//!             this.name = name;
//!         }
//!
//!         say_hello() {
//!             print "Hello, my name is " + this.name + "!";
//!         }
//!     }
//!
//!     var jane = Person("Jane");
//!     jane.say_hello();
//! }
//! # Ok(())
//! # }
//! ```
//!
//! ```
//! # use rulox::prelude::*;
//! # use rulox::LoxError;
//! # fn main() -> Result<(), LoxError> {
//! lox! {
//!     class Person {
//!         init(name) {
//!             this.name = name;
//!         }
//!
//!         say_hello() {
//!             print "Hello, my name is " + this.name + "!";
//!         }
//!     }
//!
//!     class Telepath > Person {
//!         init(name, power) {
//!             super(name);
//!             this.power = power;
//!         }
//!
//!         lift(weight) {
//!             if (this.power < weight) {
//!                 print "It's too heavy!";
//!             } else if (this.power == weight) {
//!                 print "I can't keep this up for long!";
//!             } else {
//!                 print "This is child's play.";
//!             }
//!         }
//!     }
//!
//!     var bob = Person("Bob");
//!     bob.say_hello();
//!
//!     print "";
//!
//!     var alice = Telepath("Alice", 4);
//!     alice.say_hello();
//!     alice.lift(1.5);
//!     alice.lift(4);
//!     alice.lift(10);
//! }
//! # Ok(())
//! # }
//! ```
//!
//! ```
//! # use rulox::prelude::*;
//! # use rulox::LoxError;
//! # fn main() -> Result<(), LoxError> {
//! lox! {
//!     var except_ran = false;
//!     var else_ran = false;
//!     var finally_ran = false;
//!     try {
//!         print "try";
//!         throw 1;
//!     } except {
//!         print "except";
//!         except_ran = true;
//!     } else {
//!         print "else";
//!         else_ran = true;
//!     } finally {
//!         print "finally";
//!         finally_ran = true;
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
    ( fn $rust_name:ident $( :: $segment:ident )* ( $( $arg:ident ),* ) ) => {
        $crate::lox_bindgen!( fn $rust_name ( $( $arg ),* ) as $rust_name )
    };
    ( fn $rust_name:ident $( :: $segment:ident )* ( $( $arg:ident ),* ) as $lox_name:ident ) => {
        let $lox_name = __rulox_helpers::LoxVariable::new(LoxValue::function(__rulox_helpers::LoxFn::new(
            move |mut args: $crate::LoxArgs| -> __rulox_helpers::LoxResult {
                let mut __drain = args.drain();
                $(
                    let $arg = __drain.next().unwrap();
                )*
                $crate::ToLoxResult::to_lox_result($rust_name $( :: $segment )* ( $( $arg.try_into()? ),* ))
            },
            vec![$( stringify!($arg) ),*]
        )));
    };
    ( async fn $rust_name:ident $( :: $segment:ident )* ( $( $arg:ident ),* ) ) => {
        $crate::lox_bindgen!( async fn $rust_name ( $( $arg ),* ) as $rust_name )
    };
    ( async fn $rust_name:ident $( :: $segment:ident )* ( $( $arg:ident ),* ) as $lox_name:ident ) => {
        let $lox_name = __rulox_helpers::LoxVariable::new(LoxValue::coroutine(
            move |mut args: $crate::LoxArgs| -> Box<dyn $crate::prelude::__rulox_helpers::Future<Output = $crate::LoxResult> + Send + Sync + 'static> {
                let mut __drain = args.drain();
                $(
                    let $arg = __drain.next().unwrap();
                )*
                Box::new(async move {
                    $crate::ToLoxResult::to_lox_result($rust_name $( :: $segment )* ( $( $arg.try_into()? ),* ).await)
                })
            },
            vec![$( stringify!($arg) ),*]
        ));
    };
    {
        $(
            fn $sync_rust_name:ident $( :: $sync_segment:ident )* ( $( $sync_arg:ident ),* ) $( as $sync_lox_name:ident )? ;
        )*
        $(
            async fn $async_rust_name:ident $( :: $async_segment:ident )* ( $( $async_arg:ident ),* ) $( as $async_lox_name:ident )? ;
        )*
    } => {
        $(
            $crate::lox_bindgen!( fn $sync_rust_name $( :: $sync_segment )* ( $( $sync_arg ),* ) $( as $sync_lox_name )? );
        )*
        $(
            $crate::lox_bindgen!( async fn $async_rust_name $( :: $async_segment )* ( $( $async_arg ),* ) $( as $async_lox_name )? );
        )*
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

/// Generates an implementation of [`TryFrom<LoxValue>`].
///
/// This implementation casts the input to an external object, attempts to
/// downcast it to this type, and then clones it.
///
/// If cloning is not desirable, [`Shared<T>`][rulox_types::Shared] where
/// [`T: LoxObject`][rulox_types::LoxObject] implements [`TryFrom<LoxValue>`],
/// and can be used instead.
///
/// This macro requires that the input type implement both
/// [`LoxObject`][rulox_types::LoxObject] and [`Clone`].
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
pub use rulox_types::MapKey;
pub use rulox_types::Shared;

#[cfg(feature = "serialise")]
pub use rulox_types::hashmap_to_json_map;

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
