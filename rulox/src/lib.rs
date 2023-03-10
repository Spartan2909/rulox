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
//! use rulox::transpiler::*;
//!
//! fn main() {
//!     lox! {
//!         var a = 5;
//!
//!         print a + 2;
//!     }
//!
//!     let b: f64 = a.try_into().unwrap();
//!
//!     println!("{}", b);
//! }
//! ```
//!
//! ```
//! use rulox::transpiler::*;
//!
//! fn main() {
//!     lox! {
//!         for (var i = 5; i > 0; i = i - 1) print i;
//!     }
//! }
//! ```
//!
//! ```
//! use rulox::transpiler::*;
//!
//! fn main() {
//!     lox! {
//!         fun hello(name) {
//!             print "Hello " + name + "! :)";
//!         }
//!
//!         fun add_one(num) {
//!             return num + 1;
//!         }
//!
//!         hello("Alice");
//!     }
//!
//!     rust_hello(LoxValue::from("Bob"));
//!
//!     assert_eq!(rust_add_one(LoxValue::from(3)), LoxValue::from(4));
//! }
//! ```
//!
//! ```
//! use rulox::transpiler::*;
//!
//! fn main() {
//!     lox! {
//!         var people = ["Bob", "Alice", "John"];
//!
//!         for (person in people) {
//!             print "Hello " + person + "!";
//!         }
//!     }
//! }
//! ```

pub mod transpiler {
    /// Parses Lox code and converts it to Rust.
    /// # Examples
    /// ```
    /// use rulox::*;
    ///
    /// fn main () {
    ///     lox! {
    ///         var hello = "hello ";
    ///     }
    ///
    ///     let world = "world";
    ///
    ///     lox! {
    ///         print hello + world;
    ///     }
    /// }
    /// ```
    ///
    /// # Panics
    /// If an operation is attemped between two unsupported types.
    pub use rulox_macro::lox;

    /// Generates a rulox binding for a Rust function.
    /// # Examples
    /// ```
    /// use rulox::*;
    ///
    /// fn hello(name: String) -> String {
    ///     "Hello ".to_string() + &name
    /// }
    ///
    /// fn main() {
    ///     lox_bindgen!(fn hello(name));
    ///
    ///     lox! {
    ///         print lox_hello("Alice");
    ///     }
    /// }
    /// ```
    pub use rulox_macro::lox_bindgen;

    pub use rulox_types::{extract, LoxFn, LoxValue};
}
