//! A lightweight, dynamically typed scripting language embedded in Rust.

extern crate proc_macro;
use proc_macro2::TokenStream;

use syn::parse_macro_input;

use quote::ToTokens;

mod ast;

/// Parses Lox code and converts it to Rust.
/// # Examples
/// ```
/// use rulox::lox;
/// use rulox_types::prelude::*;
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
#[proc_macro]
pub fn lox(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(tokens as ast::LoxProgram);

    let mut output = TokenStream::new();

    for stmt in input.statements {
        stmt.to_tokens(&mut output);
    }

    output.into()
}
