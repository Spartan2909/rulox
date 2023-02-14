use proc_macro2::{Punct, Spacing, TokenStream};

use quote::{quote, ToTokens, TokenStreamExt};

use rulox_types::LoxValue;

use syn::bracketed;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::{parenthesized, spanned::Spanned, token, BinOp, Ident, Token, UnOp};

mod kw {
    syn::custom_keyword!(nil);
    syn::custom_keyword!(print);
    syn::custom_keyword!(var);
    syn::custom_keyword!(and);
    syn::custom_keyword!(or);
}

pub struct LoxProgram {
    pub statements: Vec<Stmt>,
}

impl Parse for LoxProgram {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut statements: Vec<Stmt> = vec![];

        while !input.is_empty() {
            statements.push(input.parse()?);
        }

        Ok(Self { statements })
    }
}

pub enum Stmt {
    Expr(Expr),
    Print(Expr),
    Var { name: Ident, initializer: Expr },
}

impl ToTokens for Stmt {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Expr(expr) => {
                expr.to_tokens(tokens);
                tokens.append(Punct::new(';', Spacing::Alone));
            }
            Self::Print(expr) => {
                tokens.append_all(quote! { println!("{}", #expr); });
                tokens.append(Punct::new(';', Spacing::Alone));
            }
            Self::Var { name, initializer } => {
                tokens.append_all(quote! { let mut #name });
                tokens.append(Punct::new('=', Spacing::Alone));
                initializer.to_tokens(tokens);
                tokens.append(Punct::new(';', Spacing::Alone));
            }
        }
    }
}

impl Parse for Stmt {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Self::declaration(input)
    }
}

impl Stmt {
    fn declaration(input: ParseStream) -> syn::Result<Self> {
        if input.peek(kw::var) {
            Self::var_declaration(input)
        } else {
            Self::statement(input)
        }
    }

    fn var_declaration(input: ParseStream) -> syn::Result<Self> {
        input.parse::<kw::var>()?;

        let name: Ident = input.parse()?;

        let initializer: Expr = if input.peek(Token![=]) {
            input.parse::<Token![=]>()?;
            input.parse()?
        } else {
            Expr::Literal(LoxValue::Nil)
        };

        input.parse::<Token![;]>()?;

        Ok(Self::Var { name, initializer })
    }

    fn statement(input: ParseStream) -> syn::Result<Self> {
        if input.peek(kw::print) {
            input.parse::<kw::print>()?;
            let expr: Expr = input.parse()?;
            input.parse::<Token![;]>()?;
            Ok(Self::Print(expr))
        } else {
            let expr: Expr = input.parse()?;
            input.parse::<Token![;]>()?;
            Ok(Self::Expr(expr))
        }
    }
}

pub enum Expr {
    Literal(rulox_types::LoxValue),
    Array(Vec<Expr>),
    Variable(Ident),
    Grouping(Box<Expr>),
    Unary {
        operator: UnOp,
        expr: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        operator: BinOp,
        right: Box<Expr>,
    },
    Assign {
        name: Ident,
        value: Box<Expr>,
    },
}

impl ToTokens for Expr {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Literal(value) => {
                value.to_tokens(tokens);
            }
            Self::Array(arr) => {
                let mut inner = TokenStream::new();
                for expr in arr {
                    inner.append_all(quote! { #expr, })
                }

                tokens.append_all(quote! { LoxValue::from([#inner]) })
            }
            Self::Variable(var) => {
                tokens.append(var.clone());
            }
            Self::Grouping(expr) => {
                let expr: &Expr = &*expr;
                expr.to_tokens(tokens);
            }
            Self::Unary { operator, expr } => {
                operator.to_tokens(tokens);
                let expr: &Expr = &*expr;
                expr.to_tokens(tokens);
            }
            Self::Binary {
                left,
                operator,
                right,
            } => {
                let left: &Expr = &*left;
                let right: &Expr = &*right;
                tokens.append_all(
                    quote! { extract(LoxValue::from(#left) #operator LoxValue::from(#right)) },
                );
            }
            Self::Assign { name, value } => {
                let value: &Expr = &*value;
                tokens.append_all(quote! { #name = #value })
            }
        }
    }
}

impl Parse for Expr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Self::assignment(input)
    }
}

impl Expr {
    fn assignment(input: ParseStream) -> syn::Result<Self> {
        let expr = Self::or(input)?;

        if input.peek(Token![=]) {
            input.parse::<Token![=]>()?;
            let value = Self::assignment(input)?;

            match expr {
                Expr::Variable(name) => {
                    return Ok(Self::Assign {
                        name,
                        value: Box::new(value),
                    })
                }
                _ => Err(input.error("invalid assignment target"))?,
            }
        }

        Ok(expr)
    }

    fn or(input: ParseStream) -> syn::Result<Self> {
        let mut expr = Self::and(input)?;

        while input.peek(kw::or) {
            let span = input.parse::<kw::or>()?.span();
            let right = Self::and(input)?;
            let operator = BinOp::BitOr(Token![|](span));
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            }
        }

        Ok(expr)
    }

    fn and(input: ParseStream) -> syn::Result<Self> {
        let mut expr = Self::equality(input)?;

        while input.peek(kw::and) {
            let span = input.parse::<kw::and>()?.span();
            let right = Self::equality(input)?;
            let operator = BinOp::BitAnd(Token![&](span));
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            }
        }

        Ok(expr)
    }

    fn equality(input: ParseStream) -> syn::Result<Self> {
        let mut expr = Self::comparison(input)?;

        while input.peek(Token![!=]) || input.peek(Token![==]) {
            let operator: BinOp = input.parse()?;
            let right = Self::comparison(input)?;
            expr = Self::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn comparison(input: ParseStream) -> syn::Result<Self> {
        let mut expr = Self::term(input)?;

        while input.peek(Token![>])
            || input.peek(Token![>=])
            || input.peek(Token![<])
            || input.peek(Token![<=])
        {
            let operator: BinOp = input.parse()?;
            let right = Self::term(input)?;
            expr = Self::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn term(input: ParseStream) -> syn::Result<Self> {
        let mut expr = Self::factor(input)?;

        while input.peek(Token![-]) || input.peek(Token![+]) {
            let operator: BinOp = input.parse()?;
            let right = Self::factor(input)?;
            expr = Self::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            }
        }

        Ok(expr)
    }

    fn factor(input: ParseStream) -> syn::Result<Self> {
        let mut expr = Self::unary(input)?;

        while input.peek(Token![/]) || input.peek(Token![*]) {
            let operator: BinOp = input.parse()?;
            let right = Self::unary(input)?;
            expr = Self::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            }
        }

        Ok(expr)
    }

    fn unary(input: ParseStream) -> syn::Result<Self> {
        if input.peek(Token![!]) || input.peek(Token![-]) {
            let operator: UnOp = input.parse()?;
            let right = Self::unary(input)?;
            Ok(Self::Unary {
                operator,
                expr: Box::new(right),
            })
        } else {
            Self::primary(input)
        }
    }

    fn primary(input: ParseStream) -> syn::Result<Self> {
        if input.peek(syn::LitBool) {
            let value: syn::LitBool = input.parse()?;
            Ok(Self::Literal(LoxValue::from(value.value)))
        } else if input.peek(kw::nil) {
            Ok(Self::Literal(LoxValue::Nil))
        } else if input.peek(syn::LitStr) {
            let value: syn::LitStr = input.parse()?;
            Ok(Self::Literal(LoxValue::from(value.value())))
        } else if input.peek(syn::LitInt) {
            let value: syn::LitInt = input.parse()?;
            Ok(Self::Literal(LoxValue::from(value.base10_parse::<f64>()?)))
        } else if input.peek(syn::LitFloat) {
            let value: syn::LitFloat = input.parse()?;
            Ok(Self::Literal(LoxValue::from(value.base10_parse::<f64>()?)))
        } else if input.peek(token::Bracket) {
            let content;
            bracketed!(content in input);

            let items: Punctuated<Expr, Token![,]> = content.parse_terminated(Expr::parse)?;
            let items = Vec::from_iter(items.into_iter());

            Ok(Self::Array(items))
        } else if input.peek(token::Paren) {
            let content;
            parenthesized!(content in input);
            let expr = Self::parse(&content)?;
            Ok(Self::Grouping(Box::new(expr)))
        } else {
            Ok(Self::Variable(input.parse()?))
        }
    }
}
