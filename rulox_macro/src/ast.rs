use rulox_types::LoxValue;

use syn::braced;
use syn::bracketed;
use syn::parenthesized;
use syn::parse::Parse;
use syn::parse::ParseStream;
use syn::spanned::Spanned;
use syn::token;
use syn::token::Paren;
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
    syn::custom_keyword!(this);
    syn::custom_keyword!(throw);
    syn::custom_keyword!(except);
    syn::custom_keyword!(finally);
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

pub struct Except {
    pub binding: Option<Ident>,
    pub guard: Option<Expr>,
    pub body: Box<Stmt>,
}

pub enum Stmt {
    Expr(Expr),
    Print(Expr),
    Break,
    Var {
        name: Ident,
        initialiser: Option<Expr>,
    },
    Return(Option<Expr>),
    Function {
        is_async: bool,
        function: Function,
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
    Class {
        name: Ident,
        methods: Vec<(bool, Function)>,
        superclass: Option<Ident>,
    },
    Throw(Expr),
    Try {
        body: Box<Stmt>,
        excepts: Vec<Except>,
        else_block: Option<Box<Stmt>>,
        finally: Option<Box<Stmt>>,
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
            Self::function(input, false)
        } else if input.peek(Token![async]) {
            input.parse::<Token![async]>()?;
            Self::function(input, true)
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

    fn function(input: ParseStream, is_async: bool) -> syn::Result<Self> {
        if !input.peek2(Ident) {
            return Ok(Self::Expr(Expr::function(input, is_async)?));
        }

        input.parse::<kw::fun>()?;

        Ok(Self::Function {
            is_async,
            function: input.parse()?,
        })
    }

    fn class(input: ParseStream) -> syn::Result<Self> {
        input.parse::<kw::class>()?;
        let name: Ident = input.parse()?;

        let superclass = if input.peek(Token![>]) {
            input.parse::<Token![>]>()?;
            Some(input.parse()?)
        } else {
            None
        };

        let mut methods = vec![];
        let content;
        braced!(content in input);
        while !content.is_empty() {
            let is_async = if content.peek(Token![async]) {
                content.parse::<Token![async]>()?;
                true
            } else {
                false
            };
            methods.push((is_async, content.parse()?));
        }

        Ok(Self::Class {
            name,
            methods,
            superclass,
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
        } else if input.peek(kw::throw) {
            Self::throw(input)
        } else if input.peek(Token![try]) {
            Self::try_statement(input)
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
            None
        } else {
            Some(input.parse()?)
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

    fn throw(input: ParseStream) -> syn::Result<Self> {
        input.parse::<kw::throw>()?;

        let expr = input.parse()?;

        input.parse::<Token![;]>()?;

        Ok(Self::Throw(expr))
    }

    fn try_statement(input: ParseStream) -> syn::Result<Self> {
        input.parse::<Token![try]>()?;

        let body = input.parse()?;

        let mut excepts = vec![];
        while input.peek(kw::except) {
            input.parse::<kw::except>()?;

            let (binding, guard) = if input.peek(Paren) {
                let content;
                parenthesized!(content in input);
                let binding = content.parse()?;
                let guard = if content.peek(Token![if]) {
                    content.parse::<Token![if]>()?;
                    Some(content.parse()?)
                } else {
                    None
                };
                (binding, guard)
            } else {
                (None, None)
            };

            let body = input.parse()?;

            excepts.push(Except {
                binding,
                guard,
                body,
            })
        }

        let else_block = if input.peek(Token![else]) {
            input.parse::<Token![else]>()?;
            Some(input.parse()?)
        } else {
            None
        };

        let finally = if input.peek(kw::finally) {
            input.parse::<kw::finally>()?;
            Some(input.parse()?)
        } else {
            None
        };

        Ok(Self::Try {
            body: Box::new(body),
            excepts,
            else_block,
            finally,
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
    Literal(LoxValue),
    Array(Vec<Expr>),
    Function {
        is_async: bool,
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
    This,
    Get {
        object: Box<Expr>,
        name: Ident,
    },
    Set {
        object: Box<Expr>,
        name: Ident,
        value: Box<Expr>,
    },
    Super {
        arguments: Vec<Expr>,
    },
    Await {
        left: Box<Expr>,
    },
    Index {
        left: Box<Expr>,
        index: Box<Expr>,
    },
    IndexSet {
        left: Box<Expr>,
        index: Box<Expr>,
        value: Box<Expr>,
    },
    Map(Vec<(Expr, Expr)>),
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
                Expr::Get { object, name } => {
                    return Ok(Self::Set {
                        object,
                        name,
                        value: Box::new(value),
                    });
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

        loop {
            if input.peek(token::Paren) {
                expr = Self::finish_call(input, expr)?;
            } else if input.peek(Token![.]) {
                input.parse::<Token![.]>()?;
                if input.peek(Token![await]) {
                    input.parse::<Token![await]>()?;
                    expr = Self::Await {
                        left: Box::new(expr),
                    };
                } else {
                    expr = Self::Get {
                        object: Box::new(expr),
                        name: input.parse()?,
                    };
                }
            } else if input.peek(token::Bracket) {
                let content;
                bracketed!(content in input);
                let index = content.parse()?;
                if input.peek(Token![=]) {
                    input.parse::<Token![=]>()?;
                    expr = Expr::IndexSet {
                        left: Box::new(expr),
                        index: Box::new(index),
                        value: Box::new(input.parse()?),
                    };
                } else {
                    expr = Expr::Index {
                        left: Box::new(expr),
                        index: Box::new(index),
                    };
                }
            } else {
                break;
            }
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
        let lookahead = input.lookahead1();
        if lookahead.peek(syn::LitBool) {
            let value: syn::LitBool = input.parse()?;
            Ok(Self::Literal(LoxValue::from(value.value)))
        } else if lookahead.peek(kw::nil) {
            input.parse::<kw::nil>()?;
            Ok(Self::Literal(LoxValue::Nil))
        } else if lookahead.peek(syn::LitStr) {
            let value: syn::LitStr = input.parse()?;
            Ok(Self::Literal(LoxValue::from(&value.value())))
        } else if lookahead.peek(syn::LitInt) {
            let value: syn::LitInt = input.parse()?;
            Ok(Self::Literal(LoxValue::from(value.base10_parse::<f64>()?)))
        } else if lookahead.peek(syn::LitFloat) {
            let value: syn::LitFloat = input.parse()?;
            Ok(Self::Literal(LoxValue::from(value.base10_parse::<f64>()?)))
        } else if lookahead.peek(kw::fun) {
            Self::function(input, false)
        } else if lookahead.peek(Token![async]) {
            input.parse::<Token![async]>()?;
            Self::function(input, true)
        } else if lookahead.peek(kw::this) {
            input.parse::<kw::this>()?;
            Ok(Expr::This)
        } else if lookahead.peek(Token![super]) {
            input.parse::<Token![super]>()?;
            let content;

            parenthesized!(content in input);
            let arguments = content.parse_terminated(Expr::parse, Token![,])?;

            Ok(Self::Super {
                arguments: arguments.into_iter().collect(),
            })
        } else if lookahead.peek(token::Bracket) {
            let content;
            bracketed!(content in input);

            let items = content.parse_terminated(Expr::parse, Token![,])?;
            let items = Vec::from_iter(items);

            Ok(Self::Array(items))
        } else if lookahead.peek(token::Paren) {
            let content;
            parenthesized!(content in input);
            Self::parse(&content)
        } else if lookahead.peek(token::Brace) {
            Self::map(input)
        } else if lookahead.peek(Ident) {
            Ok(Self::Variable(input.parse()?))
        } else {
            Err(lookahead.error())
        }
    }

    fn function(input: ParseStream, is_async: bool) -> syn::Result<Self> {
        input.parse::<kw::fun>()?;

        let content;
        parenthesized!(content in input);

        let params = content.parse_terminated(Ident::parse, Token![,])?;

        let body: Stmt = input.parse()?;

        Ok(Self::Function {
            is_async,
            params: Vec::from_iter(params),
            body: Box::new(body),
        })
    }

    fn map(input: ParseStream) -> syn::Result<Self> {
        let content;
        braced!(content in input);
        let mut map = vec![];
        while !content.is_empty() {
            let key = content.parse()?;
            content.parse::<Token![:]>()?;
            map.push((key, content.parse()?));
            if !content.is_empty() {
                content.parse::<Token![,]>()?;
            }
        }
        Ok(Self::Map(map))
    }
}
