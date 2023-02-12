use rulox_types::LoxValue;

use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::{parenthesized, token, BinOp, Token, UnOp};

mod kw {
    syn::custom_keyword!(nil);
    syn::custom_keyword!(print);
}

pub struct LoxProgram {
    statements: Punctuated<Stmt, Token![;]>,
}

impl Parse for LoxProgram {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            statements: input.parse_terminated(Stmt::parse)?,
        })
    }
}

enum Stmt {
    Expr(Expr),
    Print(Expr),
    Declaration,
}

impl Parse for Stmt {
    fn parse(input: ParseStream) -> syn::Result<Self> {
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

enum Expr {
    Literal(rulox_types::LoxValue),
    Grouping(Box<Expr>),
    Unary {
        operator: UnOp,
        expression: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        operator: BinOp,
        right: Box<Expr>,
    },
}

impl Parse for Expr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Self::equality(input)
    }
}

impl Expr {
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
                expression: Box::new(right),
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
            Ok(Self::Literal(LoxValue::from(value.base10_parse::<u128>()?)))
        } else if input.peek(syn::LitFloat) {
            let value: syn::LitFloat = input.parse()?;
            Ok(Self::Literal(LoxValue::from(value.base10_parse::<f64>()?)))
        } else if input.peek(token::Paren) {
            let content;
            parenthesized!(content in input);
            let expr = Self::parse(&content)?;
            Ok(Self::Grouping(Box::new(expr)))
        } else {
            Err(input.error("expected expression"))
        }
    }
}
