//! A lightweight, dynamically typed scripting language embedded in Rust.

#![doc(html_playground_url = "https://play.rust-lang.org/")]

extern crate proc_macro;
use proc_macro2::TokenStream;

use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::{parenthesized, parse_macro_input, token, Ident, Token};

use quote::{quote, ToTokens, TokenStreamExt};

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

struct Signature {
    unsafe_specifier: Option<Token![unsafe]>,
    _fn: Token![fn],
    name: Ident,
    _paren: token::Paren,
    params: Punctuated<Ident, Token![,]>,
}

impl Parse for Signature {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        Ok(Self {
            unsafe_specifier: input.parse()?,
            _fn: input.parse()?,
            name: input.parse()?,
            _paren: parenthesized!(content in input),
            params: content.parse_terminated(Ident::parse)?,
        })
    }
}

#[proc_macro]
pub fn lox_bindgen(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(tokens as Signature);

    let mut output = TokenStream::new();

    let name = "lox_".to_string() + &input.name.to_string();
    let name = Ident::new(&name, input.name.span());

    output.append_all(quote! { fn #name(_args: Vec<LoxValue>) -> LoxValue });

    let mut params = TokenStream::new();
    for (i, _) in input.params.into_iter().enumerate() {
        params.append_all(quote! { _args[#i].clone().try_into().unwrap(), });
    }

    let rust_fn_name = input.name;

    let mut body = TokenStream::new();
    body.append_all(quote! { LoxValue::from(#rust_fn_name(#params)) });

    if input.unsafe_specifier.is_some() {
        body = quote! { unsafe { #body } };
    }

    output.append_all(quote! { { #body}  });

    output.into()
}
