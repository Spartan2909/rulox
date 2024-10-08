use crate::LoxValue;

use proc_macro2::Literal;
use proc_macro2::Punct;
use proc_macro2::Spacing;
use proc_macro2::TokenStream;

use quote::quote;
use quote::ToTokens;
use quote::TokenStreamExt;

impl ToTokens for LoxValue {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Bool(b) => tokens.append_all(quote! { LoxValue::from(#b) }),
            Self::Str(_) => {
                let s = self.to_string();

                tokens.append_all(quote! { LoxValue::from(#s) });
            }
            Self::Num(n) => tokens.append_all(quote! { LoxValue::from(#n) }),
            Self::Arr(arr) => {
                tokens.append_all(quote! { LoxValue::from });
                tokens.append(Punct::new('(', Spacing::Alone));
                tokens.append_all(quote! { vec! });
                tokens.append(Punct::new('[', Spacing::Alone));
                for value in arr.read().iter() {
                    tokens.append_all(quote! { #value, });
                }
                tokens.append(Punct::new(']', Spacing::Joint));
                tokens.append(Punct::new(')', Spacing::Alone));
            }
            Self::Function(_) | Self::BoundMethod(_, _) | Self::PrimitiveMethod(_, _) => unimplemented!(
                "tokens produced by Lox functions differ for statements and expressions, and so must be converted manually"
            ),
            Self::Class(_) => unimplemented!("classes contain functions, which cannot be converted without context"),
            Self::Instance(_) => unimplemented!("instances cannot directly occur in source code"),
            Self::Map(_) => unimplemented!("maps must be constructed with LoxValue::map"),
            Self::Bytes(bytes) => Literal::byte_string(bytes).to_tokens(tokens),
            Self::Error(_) => unimplemented!("errors cannot directly occur in source code"),
            Self::Coroutine(_) => unimplemented!("coroutine are functions, which cannot be converted without context"),
            Self::Future(_) => unimplemented!("futures cannot directly occur in source code"),
            Self::Nil => tokens.append_all(quote! { LoxValue::Nil }),
            Self::External(_) => unimplemented!("external objects cannot be represented in Lox code"),
        }
    }
}
