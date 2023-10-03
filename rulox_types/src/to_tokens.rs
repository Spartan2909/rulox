use crate::LoxValue;

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
            Self::Function(_) => unimplemented!(
                "tokens produced by Lox functions differ for statements and expressions, and so must be converted manually"
            ),
            Self::Instance(_) => todo!(),
            Self::Nil => tokens.append_all(quote! { LoxValue::Nil }),
        }
    }
}
