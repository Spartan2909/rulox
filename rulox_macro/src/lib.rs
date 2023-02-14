//! A lightweight, dynamically typed scripting language embedded in Rust.

#![doc(html_playground_url = "https://play.rust-lang.org/")]

extern crate proc_macro;
use proc_macro2::TokenStream;

use syn::parse_macro_input;

use quote::ToTokens;

mod ast;

#[proc_macro]
pub fn lox(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(tokens as ast::LoxProgram);

    let mut output = TokenStream::new();

    for stmt in input.statements {
        stmt.to_tokens(&mut output);
    }

    output.into()
}
