use flexi_parse::group;
use flexi_parse::Parse;
use flexi_parse::ParseStream;
use flexi_parse::group::Braces;
use flexi_parse::Punct;
use flexi_parse::punctuated::Punctuated;
use flexi_parse::token::Token;
use flexi_parse::token::Ident;
use flexi_parse::group::Parentheses;
use flexi_parse::token::LeftBrace;
use flexi_parse::token::LitInt;
use flexi_parse::token::LeftParen;
use flexi_parse::token::LeftBracket;
use flexi_parse::group::Brackets;
use flexi_parse::token::LitFloat;
use flexi_parse::token::LitStrDoubleQuote as LitStr;
use flexi_parse::Span;

use rulox_types::LoxValue;

mod kw {
    flexi_parse::keywords![
        and,
        async as kw_async,
        await as kw_await,
        break as kw_break,
        class,
        else as kw_else,
        except,
        false as kw_false,
        finally,
        for as kw_for,
        fun,
        if as kw_if,
        in as kw_in,
        loop as kw_loop,
        nil,
        or,
        print,
        return as kw_return,
        super as kw_super,
        this,
        throw,
        true as kw_true,
        try as kw_try,
        var,
        while as kw_while,
    ];
}

#[derive(Debug)]
pub struct LoxProgram {
    pub statements: Vec<Stmt>,
}

impl Parse for LoxProgram {
    fn parse(input: ParseStream) -> flexi_parse::Result<Self> {
        let mut statements: Vec<Stmt> = vec![];

        while !input.is_empty() {
            statements.push(input.parse()?);
        }

        Ok(Self { statements })
    }
}

#[derive(Debug)]
pub enum BinOp {
    Add(Punct!["+"]),
    Sub(Punct!["-"]),
    Mul(Punct!["*"]),
    Div(Punct!["/"]),
    Rem(Punct!["%"]),

    Eq(Punct!["=="]),
    Ne(Punct!["!="]),
    Lt(Punct!["<"]),
    Le(Punct!["<="]),
    Gt(Punct![">"]),
    Ge(Punct![">="]),

    And(Punct!["&&"]),
    Or(Punct!["||"]),
}

impl BinOp {
    pub(crate) fn span(&self) -> &Span {
        macro_rules! generate_match {
            ($($name:ident),+ $(,)?) => {
                match self {
                    $(
                        BinOp::$name(token) => token.span(),
                    )+
                }
            };
        }
        generate_match!(Add, Sub, Mul, Div, Rem, Eq, Ne, Lt, Le, Gt, Ge, And, Or)
    }
}

impl Parse for BinOp {
    fn parse(input: ParseStream) -> flexi_parse::Result<Self> {
        let lookahead = input.lookahead();
        if lookahead.peek(Punct!["+"]) {
            Ok(BinOp::Add(input.parse()?))
        } else if lookahead.peek(Punct!["-"]) {
            Ok(BinOp::Sub(input.parse()?))
        } else if lookahead.peek(Punct!["*"]) {
            Ok(BinOp::Mul(input.parse()?))
        } else if lookahead.peek(Punct!["/"]) {
            Ok(BinOp::Div(input.parse()?))
        } else if lookahead.peek(Punct!["%"]) {
            Ok(BinOp::Rem(input.parse()?))
        } else if lookahead.peek(Punct!["=="]) {
            Ok(BinOp::Eq(input.parse()?))
        } else if lookahead.peek(Punct!["!="]) {
            Ok(BinOp::Ne(input.parse()?))
        } else if lookahead.peek(Punct!["<"]) {
            Ok(BinOp::Lt(input.parse()?))
        } else if lookahead.peek(Punct!["<="]) {
            Ok(BinOp::Le(input.parse()?))
        } else if lookahead.peek(Punct![">"]) {
            Ok(BinOp::Gt(input.parse()?))
        } else if lookahead.peek(Punct![">="]) {
            Ok(BinOp::Ge(input.parse()?))
        } else if lookahead.peek(Punct!["&&"]) {
            Ok(BinOp::And(input.parse()?))
        } else if lookahead.peek(Punct!["||"]) {
            Ok(BinOp::Or(input.parse()?))
        } else {
            Err(lookahead.error())
        }
    }
}

#[derive(Debug)]
pub enum UnOp {
    Neg(Punct!["-"]),
    Not(Punct!["!"]),
}

impl Parse for UnOp {
    fn parse(input: ParseStream) -> flexi_parse::Result<Self> {
        let lookahead = input.lookahead();
        if lookahead.peek(Punct!["-"]) {
            Ok(UnOp::Neg(input.parse()?))
        } else if lookahead.peek(Punct!["!"]) {
            Ok(UnOp::Not(input.parse()?))
        } else {
            Err(lookahead.error())
        }
    }
}

#[derive(Debug)]
pub enum FunctionName {
    Ident(Ident),
    BinOp(BinOp),
    Negate(Punct!["-", "@"]),
    Call(Parentheses),
    Index(Brackets),
    IndexSet(Brackets, Punct!["="]),
}

impl Parse for FunctionName {
    fn parse(input: ParseStream) -> flexi_parse::Result<Self> {
        if input.peek(Ident) {
            Ok(FunctionName::Ident(input.parse()?))
        } else if input.peek(Punct!["-"]) && input.peek2(Punct!["@"]) {
            Ok(FunctionName::Negate(input.parse()?))
        } else if input.peek(LeftParen) {
            let content;
            let paren = group!(content in input);
            assert!(content.is_empty(), "invalid method name");
            Ok(FunctionName::Call(paren))
        } else if input.peek(LeftBracket) {
            let content;
            let bracket = group!(content in input);
            assert!(content.is_empty(), "invalid method name");
            if input.peek(Punct!["="]) {
                Ok(FunctionName::IndexSet(bracket, input.parse()?))
            } else {
                Ok(FunctionName::Index(bracket))
            }
        } else {
            Ok(FunctionName::BinOp(input.parse()?))
        }
    }
}

#[derive(Debug)]
pub struct Function {
    pub name: FunctionName,
    pub params: Vec<Ident>,
    pub body: Box<Stmt>,
}

impl Parse for Function {
    fn parse(input: ParseStream) -> flexi_parse::Result<Self> {
        let name = input.parse()?;

        let content;
        let _: Parentheses = group!(content in input);
        let parameters: Punctuated<Ident, Punct![","]> = Punctuated::parse_separated_trailing(&content)?;

        let body = Stmt::block(input)?;

        Ok(Function {
            name,
            params: parameters.into_iter().collect(),
            body: Box::new(body),
        })
    }
}

#[derive(Debug)]
pub struct Except {
    pub binding: Option<Ident>,
    pub guard: Option<Expr>,
    pub body: Box<Stmt>,
}

#[derive(Debug)]
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
    fn parse(input: ParseStream) -> flexi_parse::Result<Self> {
        Self::declaration(input)
    }
}

impl Stmt {
    fn declaration(input: ParseStream) -> flexi_parse::Result<Self> {
        if input.peek(kw::var) {
            Self::var_declaration(input)
        } else if input.peek(kw::fun) {
            Self::function(input, false)
        } else if input.peek(kw::kw_async) {
            input.parse::<kw::kw_async>()?;
            Self::function(input, true)
        } else if input.peek(kw::class) {
            Self::class(input)
        } else {
            Self::statement(input)
        }
    }

    fn var_declaration(input: ParseStream) -> flexi_parse::Result<Self> {
        input.parse::<kw::var>()?;

        let name: Ident = input.parse()?;

        let initialiser = if input.peek(Punct!["="]) {
            input.parse::<Punct!["="]>()?;
            Some(input.parse()?)
        } else {
            None
        };

        input.parse::<Punct![";"]>()?;

        Ok(Self::Var { name, initialiser })
    }

    fn function(input: ParseStream, is_async: bool) -> flexi_parse::Result<Self> {
        if !input.peek2(Ident) {
            return Ok(Self::Expr(Expr::function(input, is_async)?));
        }

        input.parse::<kw::fun>()?;

        Ok(Self::Function {
            is_async,
            function: input.parse()?,
        })
    }

    fn class(input: ParseStream) -> flexi_parse::Result<Self> {
        input.parse::<kw::class>()?;
        let name: Ident = input.parse()?;

        let superclass = if input.peek(Punct![">"]) {
            input.parse::<Punct![">"]>()?;
            Some(input.parse()?)
        } else {
            None
        };

        let mut methods = vec![];
        let content;
        let _: Braces = group!(content in input);
        while !content.is_empty() {
            let is_async = if content.peek(kw::kw_async) {
                content.parse::<kw::kw_async>()?;
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

    fn statement(input: ParseStream) -> flexi_parse::Result<Self> {
        if input.peek(kw::kw_if) {
            Self::if_statement(input)
        } else if input.peek(kw::print) {
            Self::print_statement(input)
        } else if input.peek(kw::kw_break) {
            Self::break_statement(input)
        } else if input.peek(kw::kw_return) {
            Self::return_statement(input)
        } else if input.peek(kw::kw_while) {
            Self::while_statement(input)
        } else if input.peek(kw::kw_for) {
            Self::for_statement(input)
        } else if input.peek(kw::kw_loop) {
            Self::loop_statement(input)
        } else if input.peek(kw::throw) {
            Self::throw(input)
        } else if input.peek(kw::kw_try) {
            Self::try_statement(input)
        } else if input.peek(LeftBrace) {
            Self::block(input)
        } else {
            Self::expression_statement(input)
        }
    }

    fn if_statement(input: ParseStream) -> flexi_parse::Result<Self> {
        input.parse::<kw::kw_if>()?;

        let content;
        let _: Parentheses = group!(content in input);
        let condition: Expr = content.parse()?;

        let then_branch = Box::new(Self::statement(input)?);

        let else_branch = if input.peek(kw::kw_else) {
            input.parse::<kw::kw_else>()?;

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

    fn print_statement(input: ParseStream) -> flexi_parse::Result<Self> {
        input.parse::<kw::print>()?;
        let expr: Expr = input.parse()?;
        input.parse::<Punct![";"]>()?;
        Ok(Self::Print(expr))
    }

    fn break_statement(input: ParseStream) -> flexi_parse::Result<Self> {
        input.parse::<kw::kw_break>()?;
        input.parse::<Punct![";"]>()?;
        Ok(Self::Break)
    }

    fn return_statement(input: ParseStream) -> flexi_parse::Result<Self> {
        input.parse::<kw::kw_return>()?;

        let value = if input.peek(Punct![";"]) {
            None
        } else {
            Some(input.parse()?)
        };

        input.parse::<Punct![";"]>()?;

        Ok(Self::Return(value))
    }

    fn while_statement(input: ParseStream) -> flexi_parse::Result<Self> {
        input.parse::<kw::kw_while>()?;

        let content;
        let _: Parentheses = group!(content in input);
        let condition: Expr = content.parse()?;

        let body = Box::new(Self::statement(input)?);

        Ok(Self::While { condition, body })
    }

    fn for_statement(input: ParseStream) -> flexi_parse::Result<Self> {
        input.parse::<kw::kw_for>()?;

        let content;
        let _: Parentheses = group!(content in input);

        if content.peek2(kw::kw_in) {
            let name: Ident = content.parse()?;

            content.parse::<kw::kw_in>()?;

            let iterable: Expr = content.parse()?;

            let body: Stmt = input.parse()?;

            return Ok(Self::For {
                name,
                iterable,
                body: Box::new(body),
            });
        }

        let initializer = if content.peek(Punct![";"]) {
            Self::Expr(Expr::Literal(rulox_types::LoxValue::Nil))
        } else if content.peek(kw::var) {
            Self::var_declaration(&content)?
        } else {
            Self::expression_statement(&content)?
        };

        let condition = if content.peek(Punct![";"]) {
            Expr::Literal(rulox_types::LoxValue::Bool(true))
        } else {
            Expr::parse(&content)?
        };
        content.parse::<Punct![";"]>()?;

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

    fn loop_statement(input: ParseStream) -> flexi_parse::Result<Self> {
        input.parse::<kw::kw_loop>()?;

        let body: Stmt = input.parse()?;

        Ok(Self::Loop {
            body: Box::new(body),
        })
    }

    fn throw(input: ParseStream) -> flexi_parse::Result<Self> {
        input.parse::<kw::throw>()?;

        let expr = input.parse()?;

        input.parse::<Punct![";"]>()?;

        Ok(Self::Throw(expr))
    }

    fn try_statement(input: ParseStream) -> flexi_parse::Result<Self> {
        input.parse::<kw::kw_try>()?;

        let body = input.parse()?;

        let mut excepts = vec![];
        while input.peek(kw::except) {
            input.parse::<kw::except>()?;

            let (binding, guard) = if input.peek(LeftParen) {
                let content;
                let _: Parentheses = group!(content in input);
                let binding = content.parse()?;
                let guard = if content.peek(kw::kw_if) {
                    content.parse::<kw::kw_if>()?;
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
            });
        }

        let else_block = if input.peek(kw::kw_else) {
            input.parse::<kw::kw_else>()?;
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

    fn block(input: ParseStream) -> flexi_parse::Result<Self> {
        let content;
        let _: Braces = group!(content in input);

        let mut statements: Vec<Stmt> = vec![];

        while !content.is_empty() {
            statements.push(content.parse()?);
        }

        Ok(Self::Block(statements))
    }

    fn expression_statement(input: ParseStream) -> flexi_parse::Result<Self> {
        let expr: Expr = input.parse()?;
        input.parse::<Punct![";"]>()?;
        Ok(Self::Expr(expr))
    }
}

#[derive(Debug)]
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
    fn parse(input: ParseStream) -> flexi_parse::Result<Self> {
        Self::assignment(input)
    }
}

impl Expr {
    fn assignment(input: ParseStream) -> flexi_parse::Result<Self> {
        let expr = Self::or(input)?;

        if input.peek(Punct!["="]) {
            let eq: Punct!["="] = input.parse()?;
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
                _ => Err(input.new_error("invalid assignment target".to_string(), &eq, 0))?,
            }
        }

        Ok(expr)
    }

    fn or(input: ParseStream) -> flexi_parse::Result<Self> {
        let mut expr = Self::and(input)?;

        while input.peek(kw::or) {
            let span = input.parse::<kw::or>()?.span().clone();
            let right = Self::and(input)?;
            let operator = BinOp::Or(<Punct!["||"]>::new(span));
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            }
        }

        Ok(expr)
    }

    fn and(input: ParseStream) -> flexi_parse::Result<Self> {
        let mut expr = Self::equality(input)?;

        while input.peek(kw::and) {
            let span = input.parse::<kw::and>()?.span().clone();
            let right = Self::equality(input)?;
            let operator = BinOp::And(<Punct!["&&"]>::new(span));
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            }
        }

        Ok(expr)
    }

    fn equality(input: ParseStream) -> flexi_parse::Result<Self> {
        let mut expr = Self::comparison(input)?;

        while input.peek(Punct!["!="]) || input.peek(Punct!["=="]) {
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

    fn comparison(input: ParseStream) -> flexi_parse::Result<Self> {
        let mut expr = Self::term(input)?;

        while input.peek(Punct![">"])
            || input.peek(Punct![">="])
            || input.peek(Punct!["<"])
            || input.peek(Punct!["<="])
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

    fn term(input: ParseStream) -> flexi_parse::Result<Self> {
        let mut expr = Self::factor(input)?;

        while input.peek(Punct!["-"]) || input.peek(Punct!["+"]) {
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

    fn factor(input: ParseStream) -> flexi_parse::Result<Self> {
        let mut expr = Self::unary(input)?;

        while input.peek(Punct!["/"]) || input.peek(Punct!["*"]) || input.peek(Punct!["%"]) {
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

    fn unary(input: ParseStream) -> flexi_parse::Result<Self> {
        if input.peek(Punct!["!"]) || input.peek(Punct!["-"]) {
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

    fn call(input: ParseStream) -> flexi_parse::Result<Self> {
        let mut expr = Self::primary(input)?;

        loop {
            if input.peek(LeftParen) {
                expr = Self::finish_call(input, expr)?;
            } else if input.peek(Punct!["."]) {
                input.parse::<Punct!["."]>()?;
                if input.peek(kw::kw_await) {
                    input.parse::<kw::kw_await>()?;
                    expr = Self::Await {
                        left: Box::new(expr),
                    };
                } else {
                    expr = Self::Get {
                        object: Box::new(expr),
                        name: input.parse()?,
                    };
                }
            } else if input.peek(LeftBracket) {
                let content;
                let _: Brackets = group!(content in input);
                let index = content.parse()?;
                if input.peek(Punct!["="]) {
                    input.parse::<Punct!["="]>()?;
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

    fn finish_call(input: ParseStream, callee: Expr) -> flexi_parse::Result<Self> {
        let content;
        let _: Parentheses = group!(content in input);

        let arguments: Punctuated<Expr, Punct![","]> = Punctuated::parse_separated_trailing(&content)?;

        Ok(Self::Call {
            callee: Box::new(callee),
            arguments: Vec::from_iter(arguments),
        })
    }

    fn primary(input: ParseStream) -> flexi_parse::Result<Self> {
        let lookahead = input.lookahead();
        if lookahead.peek(kw::kw_true) {
            let _: kw::kw_true = input.parse()?;
            Ok(Self::Literal(LoxValue::from(true)))
        } else if lookahead.peek(kw::kw_false) {
            let _: kw::kw_false = input.parse()?;
            Ok(Self::Literal(LoxValue::from(false)))
        } else if lookahead.peek(kw::nil) {
            input.parse::<kw::nil>()?;
            Ok(Self::Literal(LoxValue::Nil))
        } else if lookahead.peek(LitStr) {
            let value: LitStr = input.parse()?;
            Ok(Self::Literal(LoxValue::from(value.string().clone())))
        } else if lookahead.peek(LitFloat) {
            let value: LitFloat = input.parse()?;
            Ok(Self::Literal(LoxValue::from(value.value())))
        } else if lookahead.peek(LitInt) {
            let value: LitInt = input.parse()?;
            Ok(Self::Literal(LoxValue::from(value.value())))
        } else if lookahead.peek(kw::fun) {
            Self::function(input, false)
        } else if lookahead.peek(kw::kw_async) {
            input.parse::<kw::kw_async>()?;
            Self::function(input, true)
        } else if lookahead.peek(kw::this) {
            input.parse::<kw::this>()?;
            Ok(Expr::This)
        } else if lookahead.peek(kw::kw_super) {
            input.parse::<kw::kw_super>()?;
            let content;

            let _: Parentheses = group!(content in input);
            let arguments: Punctuated<Expr, Punct![","]> = Punctuated::parse_separated_trailing(&content)?;

            Ok(Self::Super {
                arguments: arguments.into_iter().collect(),
            })
        } else if lookahead.peek(LeftBracket) {
            let content;
            let _: Brackets = group!(content in input);

            let items: Punctuated<Expr, Punct![","]> = Punctuated::parse_separated_trailing(&content)?;
            let items = Vec::from_iter(items);

            Ok(Self::Array(items))
        } else if lookahead.peek(LeftParen) {
            let content;
            let _: Parentheses = group!(content in input);
            Self::parse(&content)
        } else if lookahead.peek(LeftBrace) {
            Self::map(input)
        } else if lookahead.peek(Ident) {
            Ok(Self::Variable(input.parse()?))
        } else {
            Err(lookahead.error())
        }
    }

    fn function(input: ParseStream, is_async: bool) -> flexi_parse::Result<Self> {
        input.parse::<kw::fun>()?;

        let content;
        let _: Parentheses = group!(content in input);

        let params: Punctuated<Ident, Punct![","]> = Punctuated::parse_separated_trailing(&content)?;

        let body: Stmt = input.parse()?;

        Ok(Self::Function {
            is_async,
            params: Vec::from_iter(params),
            body: Box::new(body),
        })
    }

    fn map(input: ParseStream) -> flexi_parse::Result<Self> {
        let content;
        let _: Braces = group!(content in input);
        let mut map = vec![];
        while !content.is_empty() {
            let key = content.parse()?;
            content.parse::<Punct![":"]>()?;
            map.push((key, content.parse()?));
            if !content.is_empty() {
                content.parse::<Punct![","]>()?;
            }
        }
        Ok(Self::Map(map))
    }
}
