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
    syn::custom_keyword!(class);
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

pub struct Function {
    pub name: Ident,
    pub params: Vec<Ident>,
    pub body: Box<Stmt>,
}

impl Parse for Function {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let name: Ident = input.parse()?;

        let content;
        parenthesized!(content in input);
        let parameters = content.parse_terminated(Ident::parse, Token![,])?;

        let body = Stmt::block(input)?;

        Ok(Function {
            name,
            params: parameters.into_iter().collect(),
            body: Box::new(body),
        })
    }
}

pub enum Stmt {
    Expr(Expr),
    Print(Expr),
    Break,
    Var {
        name: Ident,
        initialiser: Option<Expr>,
    },
    Return(Expr),
    Function(Function),
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
    Class {
        name: Ident,
        methods: Vec<Function>,
        superclass: Option<Ident>,
    },
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
        } else if input.peek(kw::class) {
            Self::class(input)
        } else {
            Self::statement(input)
        }
    }

    fn var_declaration(input: ParseStream) -> syn::Result<Self> {
        input.parse::<kw::var>()?;

        let name: Ident = input.parse()?;

        let initialiser = if input.peek(Token![=]) {
            input.parse::<Token![=]>()?;
            Some(input.parse()?)
        } else {
            None
        };

        input.parse::<Token![;]>()?;

        Ok(Self::Var { name, initialiser })
    }

    fn function(input: ParseStream) -> syn::Result<Self> {
        if !input.peek2(Ident) {
            return Ok(Self::Expr(Expr::function(input)?));
        }

        input.parse::<kw::fun>()?;

        Ok(Self::Function(input.parse()?))
    }

    fn class(input: ParseStream) -> syn::Result<Self> {
        input.parse::<kw::fun>()?;
        let name: Ident = input.parse()?;

        let mut methods = vec![];
        let content;
        braced!(content in input);
        while !content.is_empty() {
            methods.push(input.parse()?);
        }

        Ok(Self::Class {
            name,
            methods,
            superclass: None,
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

pub enum Expr {
    Literal(rulox_types::LoxValue),
    Array(Vec<Expr>),
    Function {
        params: Vec<Ident>,
        body: Box<Stmt>,
    },
    Variable(Ident),
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
    },
    Assign {
        name: Ident,
        value: Box<Expr>,
    },
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
            Self::parse(&content)
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
