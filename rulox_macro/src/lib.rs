//! A lightweight, dynamically typed scripting language embedded in Rust.
//!
//! This crate should be used through [`rulox`](https://docs.rs/rulox/latest).

extern crate proc_macro;

use proc_macro2::TokenStream;
use quote::quote;
use quote::ToTokens;
use quote::TokenStreamExt;

use syn::parse_macro_input;
use syn::DeriveInput;
use syn::GenericParam;
use syn::Generics;

mod ast;
mod ir;

#[proc_macro]
pub fn lox(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(tokens as ast::LoxProgram);

    let program: ir::Ir = input.into();

    program.to_token_stream().into()
}

fn generic_params(generics: &Generics) -> TokenStream {
    let mut tokens = TokenStream::new();
    for generic in &generics.params {
        if let GenericParam::Lifetime(l) = generic {
            let lifetime = &l.lifetime;
            tokens.append_all(quote!(#lifetime,));
        }
    }
    for generic in &generics.params {
        match generic {
            GenericParam::Const(c) => {
                let ident = &c.ident;
                tokens.append_all(quote!(#ident,));
            }
            GenericParam::Type(ty) => {
                let ident = &ty.ident;
                tokens.append_all(quote!(#ident,));
            }
            GenericParam::Lifetime(_) => unreachable!(),
        }
    }
    tokens
}

#[proc_macro_derive(TryFromLoxValue)]
pub fn derive_try_from(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(item as DeriveInput);

    let name = input.ident;
    let generics = input.generics;
    let generic_params = generic_params(&generics);

    quote! {
        impl #generics ::core::convert::TryFrom<__rulox_helpers::LoxValue> for #name <#generic_params>
        where
            #name <#generic_params>: ::core::clone::Clone + __rulox_helpers::LoxObject
        {
            type Error = __rulox_helpers::LoxError;

            fn try_from(value: LoxValue) -> ::core::result::Result<Self, Self::Error> {
                let shared: __rulox_helpers::Shared<Self> = value.try_into()?;
                let reader = __rulox_helpers::read(&shared);
                Ok(reader.clone())
            }
        }
    }
    .into()
}
