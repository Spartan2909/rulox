use proc_macro2::Punct;
use proc_macro2::Spacing;
use proc_macro2::TokenStream;

use quote::quote;
use quote::ToTokens;
use quote::TokenStreamExt;

use rulox_types::LoxValue;

use syn::braced;
use syn::bracketed;
use syn::parenthesized;
use syn::parse::Parse;
use syn::parse::ParseStream;
use syn::spanned::Spanned;
use syn::token;
use syn::BinOp;
use syn::Ident;
use syn::Token;
use syn::UnOp;

mod kw {
    syn::custom_keyword!(nil);
    syn::custom_keyword!(print);
    syn::custom_keyword!(var);
    syn::custom_keyword!(and);
    syn::custom_keyword!(or);
    syn::custom_keyword!(fun);
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
    Break,
    Var {
        name: Ident,
        initializer: Expr,
        implicit_nil: bool,
    },
    Return(Expr),
    Function {
        name: Ident,
        params: Vec<Ident>,
        body: Box<Stmt>,
    },
    Block(Vec<Stmt>),
    If {
        condition: Expr,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    },
    While {
        condition: Expr,
        body: Box<Stmt>,
    },
    For {
        name: Ident,
        iterable: Expr,
        body: Box<Stmt>,
    },
    Loop {
        body: Box<Stmt>,
    },
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
            Self::Break => {
                tokens.append_all(quote! { break; });
            }
            Self::Var {
                name,
                initializer,
                implicit_nil,
            } => {
                if *implicit_nil {
                    tokens.append_all(quote! { #[allow(unused_assignments, unused_mut)] });
                } else {
                    tokens.append_all(quote! { #[allow(unused_mut)] });
                }

                tokens.append_all(quote! { let mut #name = #initializer; });
            }
            Self::Return(expr) => {
                tokens.append_all(quote! { return #expr; });
            }
            Self::Function { name, params, body } => {
                tokens.append_all(quote! { fn #name(_args: Vec<LoxValue>) -> LoxValue });

                let mut expr_body = TokenStream::new();
                for (i, param) in params.iter().enumerate() {
                    expr_body.append_all(quote! { let #param = _args[#i].clone(); });
                }

                body.to_tokens(&mut expr_body);

                tokens
                    .append_all(quote! { { #expr_body #[allow(unreachable_code)] LoxValue::Nil } });

                let rust_name = String::from("rust_") + &name.to_string();
                let rust_name = Ident::new(&rust_name, proc_macro2::Span::call_site());

                let mut params_tokens = TokenStream::new();
                for param in params {
                    params_tokens.append_all(quote! { #param: LoxValue, });
                }

                let mut args = TokenStream::new();
                args.append_separated(params, Punct::new(',', Spacing::Alone));

                tokens.append_all(quote! {
                    fn #rust_name(#params_tokens) -> LoxValue {
                        #name(vec![#args])
                    }
                });

                let mut params_tokens = TokenStream::new();
                for param in params {
                    let name = param.to_string();
                    syn::LitStr::new(&name, param.span()).to_tokens(&mut params_tokens);
                    params_tokens.append_all(quote! { .to_string(), });
                }

                tokens.append_all(quote! {
                    #[allow(unused_mut, unused_variables)]
                    let mut #name = LoxValue::function(LoxFn::new(#name, vec![#params_tokens], false));
                });
            }
            Self::Block(block) => {
                let mut inner = TokenStream::new();

                for stmt in block {
                    stmt.to_tokens(&mut inner);
                }

                tokens.append_all(quote! { { #inner } })
            }
            Self::If {
                condition,
                then_branch,
                else_branch,
            } => {
                tokens.append_all(quote! { if extract(#condition.try_into()) { #then_branch } });
                if let Some(branch) = else_branch {
                    tokens.append_all(quote! { else { #branch } });
                }
            }
            Self::While { condition, body } => {
                tokens.append_all(quote! { while extract(#condition.try_into()) { #body } });
            }
            Self::For {
                name,
                iterable,
                body,
            } => {
                tokens.append_all(quote! { for #name in #iterable.into_iter() { #body } });
            }
            Self::Loop { body } => {
                tokens.append_all(quote! { loop { #body } });
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
        } else if input.peek(kw::fun) {
            Self::function(input)
        } else {
            Self::statement(input)
        }
    }

    fn var_declaration(input: ParseStream) -> syn::Result<Self> {
        input.parse::<kw::var>()?;

        let name: Ident = input.parse()?;

        let (initializer, implicit_nil) = if input.peek(Token![=]) {
            input.parse::<Token![=]>()?;
            (input.parse()?, false)
        } else {
            (Expr::Literal(LoxValue::Nil), true)
        };

        input.parse::<Token![;]>()?;

        Ok(Self::Var {
            name,
            initializer,
            implicit_nil,
        })
    }

    fn function(input: ParseStream) -> syn::Result<Self> {
        if !input.peek2(Ident) {
            return Ok(Self::Expr(Expr::function(input)?));
        }

        input.parse::<kw::fun>()?;

        let name: Ident = input.parse()?;

        let content;
        parenthesized!(content in input);
        let parameters = content.parse_terminated(Ident::parse, Token![,])?;

        let body = Self::block(input)?;

        Ok(Self::Function {
            name,
            params: Vec::from_iter(parameters),
            body: Box::new(body),
        })
    }

    fn statement(input: ParseStream) -> syn::Result<Self> {
        if input.peek(Token![if]) {
            Self::if_statement(input)
        } else if input.peek(kw::print) {
            Self::print_statement(input)
        } else if input.peek(Token![break]) {
            Self::break_statement(input)
        } else if input.peek(Token![return]) {
            Self::return_statement(input)
        } else if input.peek(Token![while]) {
            Self::while_statement(input)
        } else if input.peek(Token![for]) {
            Self::for_statement(input)
        } else if input.peek(Token![loop]) {
            Self::loop_statement(input)
        } else if input.peek(token::Brace) {
            Self::block(input)
        } else {
            Self::expression_statement(input)
        }
    }

    fn if_statement(input: ParseStream) -> syn::Result<Self> {
        input.parse::<Token![if]>()?;

        let content;
        parenthesized!(content in input);
        let condition: Expr = content.parse()?;

        let then_branch = Box::new(Self::statement(input)?);

        let else_branch = if input.peek(Token![else]) {
            input.parse::<Token![else]>()?;

            Some(Box::new(Self::statement(input)?))
        } else {
            None
        };

        Ok(Self::If {
            condition,
            then_branch,
            else_branch,
        })
    }

    fn print_statement(input: ParseStream) -> syn::Result<Self> {
        input.parse::<kw::print>()?;
        let expr: Expr = input.parse()?;
        input.parse::<Token![;]>()?;
        Ok(Self::Print(expr))
    }

    fn break_statement(input: ParseStream) -> syn::Result<Self> {
        input.parse::<Token![break]>()?;
        input.parse::<Token![;]>()?;
        Ok(Self::Break)
    }

    fn return_statement(input: ParseStream) -> syn::Result<Self> {
        input.parse::<Token![return]>()?;

        let value = if input.peek(Token![;]) {
            Expr::Literal(LoxValue::Nil)
        } else {
            input.parse()?
        };

        input.parse::<Token![;]>()?;

        Ok(Self::Return(value))
    }

    fn while_statement(input: ParseStream) -> syn::Result<Self> {
        input.parse::<Token![while]>()?;

        let content;
        parenthesized!(content in input);
        let condition: Expr = content.parse()?;

        let body = Box::new(Self::statement(input)?);

        Ok(Self::While { condition, body })
    }

    fn for_statement(input: ParseStream) -> syn::Result<Self> {
        input.parse::<Token![for]>()?;

        let content;
        parenthesized!(content in input);

        if content.peek2(Token![in]) {
            let name: Ident = content.parse()?;

            content.parse::<Token![in]>()?;

            let iterable: Expr = content.parse()?;

            let body: Stmt = input.parse()?;

            return Ok(Self::For {
                name,
                iterable,
                body: Box::new(body),
            });
        }

        let initializer = if content.peek(Token![;]) {
            Self::Expr(Expr::Literal(rulox_types::LoxValue::Nil))
        } else if content.peek(kw::var) {
            Self::var_declaration(&content)?
        } else {
            Self::expression_statement(&content)?
        };

        let condition = if content.peek(Token![;]) {
            Expr::Literal(rulox_types::LoxValue::Bool(true))
        } else {
            Expr::parse(&content)?
        };
        content.parse::<Token![;]>()?;

        let increment = if content.is_empty() {
            Expr::Literal(rulox_types::LoxValue::Nil)
        } else {
            Expr::parse(&content)?
        };

        let loop_contents = Self::Block(vec![Self::statement(input)?, Self::Expr(increment)]);

        let while_loop = Self::While {
            condition,
            body: Box::new(loop_contents),
        };

        let body = Self::Block(vec![initializer, while_loop]);

        Ok(body)
    }

    fn loop_statement(input: ParseStream) -> syn::Result<Self> {
        input.parse::<Token![loop]>()?;

        let body: Stmt = input.parse()?;

        Ok(Self::Loop {
            body: Box::new(body),
        })
    }

    fn block(input: ParseStream) -> syn::Result<Self> {
        let content;
        braced!(content in input);

        let mut statements: Vec<Stmt> = vec![];

        while !content.is_empty() {
            statements.push(content.parse()?);
        }

        Ok(Self::Block(statements))
    }

    fn expression_statement(input: ParseStream) -> syn::Result<Self> {
        let expr: Expr = input.parse()?;
        input.parse::<Token![;]>()?;
        Ok(Self::Expr(expr))
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryKind {
    Simple,
    Comparison,
    And,
    Or,
}

pub enum Expr {
    Literal(rulox_types::LoxValue),
    Array(Vec<Expr>),
    #[allow(dead_code)]
    Function {
        params: Vec<Ident>,
        body: Box<Stmt>,
    },
    Variable(Ident),
    Grouping(Box<Expr>),
    Call {
        callee: Box<Expr>,
        arguments: Vec<Expr>,
    },
    Unary {
        operator: UnOp,
        expr: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        operator: BinOp,
        right: Box<Expr>,
        kind: BinaryKind,
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
                if let LoxValue::Function(..) = value {
                    todo!()
                } else {
                    value.to_tokens(tokens);
                }
            }
            Self::Array(arr) => {
                let mut inner = TokenStream::new();
                inner.append_separated(arr, Punct::new(',', Spacing::Alone));

                tokens.append_all(quote! { LoxValue::from(vec![#inner]) })
            }
            Self::Function { params, body } => {
                let mut inner = TokenStream::new();
                inner.append_all(quote! { |_args: Vec<LoxValue>| -> LoxValue });

                let mut expr_body = TokenStream::new();
                for (i, param) in params.iter().enumerate() {
                    expr_body.append_all(quote! { let #param = _args[#i].clone(); });
                }

                body.to_tokens(&mut expr_body);

                inner
                    .append_all(quote! { { #expr_body #[allow(unreachable_code)] LoxValue::Nil } });

                let mut params_tokens = TokenStream::new();
                for param in params {
                    let name = param.to_string();
                    syn::LitStr::new(&name, param.span()).to_tokens(&mut params_tokens);
                    params_tokens.append_all(quote! { .to_string(), });
                }

                tokens.append_all(
                    quote! { LoxValue::function(LoxFn::new(#inner, vec![#params_tokens], false)) },
                );
            }
            Self::Variable(var) => {
                tokens.append_all(quote! { #var.clone() });
            }
            Self::Grouping(expr) => {
                expr.to_tokens(tokens);
            }
            Self::Call { callee, arguments } => {
                let mut inner = TokenStream::new();
                inner.append_separated(arguments, Punct::new(',', Spacing::Alone));

                tokens.append_all(quote! { #callee(vec![#inner]) });
            }
            Self::Unary { operator, expr } => {
                operator.to_tokens(tokens);
                expr.to_tokens(tokens);
            }
            Self::Binary {
                left,
                operator,
                right,
                kind,
            } => match kind {
                BinaryKind::Simple => {
                    tokens.append_all(quote! { extract(#left #operator #right) });
                }
                BinaryKind::Comparison => {
                    tokens.append_all(quote! { LoxValue::from(#left #operator #right) });
                }
                BinaryKind::And => {
                    tokens.append_all(quote! { {
                        let _left = #left;
                        if _left.is_truthy() {
                            #right
                        } else {
                            _left
                        }
                    } });
                }
                BinaryKind::Or => {
                    tokens.append_all(quote! { {
                        let _left = #left;
                        if _left.is_truthy() {
                            _left
                        } else {
                            #right
                        }
                    } });
                }
            },
            Self::Assign { name, value } => tokens.append_all(quote! { #name = #value }),
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
            let operator = BinOp::Or(Token![||](span));
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                kind: BinaryKind::Or,
            }
        }

        Ok(expr)
    }

    fn and(input: ParseStream) -> syn::Result<Self> {
        let mut expr = Self::equality(input)?;

        while input.peek(kw::and) {
            let span = input.parse::<kw::and>()?.span();
            let right = Self::equality(input)?;
            let operator = BinOp::And(Token![&&](span));
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                kind: BinaryKind::And,
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
                kind: BinaryKind::Comparison,
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
                kind: BinaryKind::Comparison,
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
                kind: BinaryKind::Simple,
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
                kind: BinaryKind::Simple,
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
            Self::call(input)
        }
    }

    fn call(input: ParseStream) -> syn::Result<Self> {
        let mut expr = Self::primary(input)?;

        while input.peek(token::Paren) {
            expr = Self::finish_call(input, expr)?
        }

        Ok(expr)
    }

    fn finish_call(input: ParseStream, callee: Expr) -> syn::Result<Self> {
        let content;
        parenthesized!(content in input);

        let arguments = content.parse_terminated(Expr::parse, Token![,])?;

        Ok(Self::Call {
            callee: Box::new(callee),
            arguments: Vec::from_iter(arguments),
        })
    }

    fn primary(input: ParseStream) -> syn::Result<Self> {
        if input.peek(syn::LitBool) {
            let value: syn::LitBool = input.parse()?;
            Ok(Self::Literal(LoxValue::from(value.value)))
        } else if input.peek(kw::nil) {
            Ok(Self::Literal(LoxValue::Nil))
        } else if input.peek(syn::LitStr) {
            let value: syn::LitStr = input.parse()?;
            Ok(Self::Literal(LoxValue::from(&value.value())))
        } else if input.peek(syn::LitInt) {
            let value: syn::LitInt = input.parse()?;
            Ok(Self::Literal(LoxValue::from(value.base10_parse::<f64>()?)))
        } else if input.peek(syn::LitFloat) {
            let value: syn::LitFloat = input.parse()?;
            Ok(Self::Literal(LoxValue::from(value.base10_parse::<f64>()?)))
        } else if input.peek(kw::fun) {
            Self::function(input)
        } else if input.peek(token::Bracket) {
            let content;
            bracketed!(content in input);

            let items = content.parse_terminated(Expr::parse, Token![,])?;
            let items = Vec::from_iter(items);

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

    fn function(input: ParseStream) -> syn::Result<Self> {
        input.parse::<kw::fun>()?;

        let content;
        parenthesized!(content in input);

        let params = content.parse_terminated(Ident::parse, Token![,])?;

        let body: Stmt = input.parse()?;

        Ok(Self::Function {
            params: Vec::from_iter(params),
            body: Box::new(body),
        })
    }
}
