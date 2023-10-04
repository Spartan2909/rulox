//! A lightweight, dynamically typed scripting language embedded in Rust.

#![doc(html_playground_url = "https://play.rust-lang.org/")]

extern crate proc_macro;

use syn::parse_macro_input;

use quote::ToTokens;

mod ast;
mod ir;

#[proc_macro]
pub fn lox(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(tokens as ast::LoxProgram);

    let program: ir::Ir = input.into();

    program.to_token_stream().into()
}
