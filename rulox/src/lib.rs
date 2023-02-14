#![doc = include_str!("../../README.md")]

/// Parses Lox code and converts it to Rust.
/// # Examples
/// ```
/// use rulox::*;
/// lox! {
///     var hello = "hello ";
/// }
///
/// let world = "world";
///
/// lox! {
///     print hello + world;
/// }
/// ```
///
/// # Panics
/// If an operation is attemped between two unsupported types.
pub use rulox_macro::lox;

pub use rulox_types::*;
