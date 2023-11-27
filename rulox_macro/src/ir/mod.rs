mod codegen;

use crate::ast;
use ast::NegName;
use syn::Token;

use std::collections::HashSet;
use std::collections::VecDeque;

use proc_macro2::Punct;
use proc_macro2::Spacing;
use proc_macro2::Span;
use proc_macro2::TokenStream;

use quote::quote;
use quote::ToTokens;
use quote::TokenStreamExt;

use rulox_types::LoxValue;

use syn::spanned::Spanned;
use syn::token::Bracket;
use syn::token::Paren;
use syn::Ident;

#[derive(Debug, Clone, Copy)]
enum FunctionType {
    None,
    Function,
    Method,
    Initialiser,
}

impl FunctionType {
    const fn is_method(self) -> bool {
        matches!(self, FunctionType::Initialiser | FunctionType::Method)
    }
}

struct Resolver {
    function_type: FunctionType,
    in_loop: bool,
}

impl Resolver {
    const fn new() -> Resolver {
        Resolver {
            function_type: FunctionType::None,
            in_loop: false,
        }
    }
}

pub struct Ir {
    forward_declarations: HashSet<Ident>,
    statements: Vec<Stmt>,
}

impl From<ast::LoxProgram> for Ir {
    fn from(value: ast::LoxProgram) -> Self {
        let (forward_declarations, statements) = convert_block(value.statements);
        let mut ir = Ir {
            forward_declarations,
            statements,
        };
        let mut resolver = Resolver::new();
        for statement in &mut ir.statements {
            statement.resolve(&mut resolver);
        }
        ir
    }
}

fn insert_reference(references: &mut HashSet<Ident>, declared: &HashSet<Ident>, ident: Ident) {
    if !declared.contains(&ident) {
        references.insert(ident);
    }
}

enum FunctionName {
    Ident(Ident),
    BinOp(BinOp, Span),
    Neg(NegName),
    Call(Paren),
    Index(Bracket),
    IndexSet(Bracket, Token![=]),
}

impl FunctionName {
    const fn ident(&self) -> Option<&Ident> {
        if let FunctionName::Ident(ident) = &self {
            Some(ident)
        } else {
            None
        }
    }

    fn span(&self) -> Span {
        match self {
            FunctionName::Ident(ident) => ident.span(),
            FunctionName::BinOp(_, span) => *span,
            FunctionName::Neg(op) => op.span(),
            FunctionName::Call(paren) => paren.span.span(),
            FunctionName::Index(bracket) => bracket.span.span(),
            FunctionName::IndexSet(bracket, equals) => bracket
                .span
                .span()
                .join(equals.span)
                .unwrap_or(bracket.span.span()),
        }
    }
}

impl From<ast::FunctionName> for FunctionName {
    fn from(value: ast::FunctionName) -> Self {
        match value {
            ast::FunctionName::Ident(name) => FunctionName::Ident(name),
            ast::FunctionName::BinOp(op) => FunctionName::BinOp(
                (&op).try_into().expect("parsed invalid operator"),
                op.span(),
            ),
            ast::FunctionName::Negate(op) => FunctionName::Neg(op),
            ast::FunctionName::Call(paren) => FunctionName::Call(paren),
            ast::FunctionName::Index(bracket) => FunctionName::Index(bracket),
            ast::FunctionName::IndexSet(bracket, equals) => FunctionName::IndexSet(bracket, equals),
        }
    }
}

struct Function {
    name: FunctionName,
    params: VecDeque<Ident>,
    body: Box<Stmt>,
    upvalues: HashSet<Ident>,
    is_initialiser: bool,
}

fn get_upvalues(body: &Stmt, params: &[Ident]) -> HashSet<Ident> {
    let mut upvalues = HashSet::new();
    let mut declared = params.iter().cloned().collect();
    body.undeclared_references(&mut declared, &mut upvalues);
    upvalues
}

impl From<ast::Function> for Function {
    fn from(value: ast::Function) -> Self {
        let body: Stmt = value.body.into();
        let upvalues = get_upvalues(&body, &value.params);
        Function {
            name: value.name.into(),
            params: value.params.into(),
            body: Box::new(body),
            upvalues,
            is_initialiser: false,
        }
    }
}

fn convert_block(stmts: Vec<ast::Stmt>) -> (HashSet<Ident>, Vec<Stmt>) {
    let mut forward_declarations = HashSet::new();
    let mut body = vec![];

    for stmt in stmts {
        let stmt: Stmt = stmt.into();
        match &stmt {
            Stmt::Class { name, .. }
            | Stmt::Function {
                function:
                    Function {
                        name: FunctionName::Ident(name),
                        ..
                    },
                ..
            }
            | Stmt::Var { name, .. } => {
                forward_declarations.insert(name.clone());
            }
            _ => {}
        }
        body.push(stmt);
    }

    (forward_declarations, body)
}

struct Except {
    binding: Option<Ident>,
    guard: Option<Expr>,
    body: Box<Stmt>,
}

impl Except {
    fn undeclared_references(&self, declared: &HashSet<Ident>, references: &mut HashSet<Ident>) {
        let mut declared = declared.clone();
        if let Some(binding) = &self.binding {
            declared.insert(binding.clone());
        }
        if let Some(guard) = &self.guard {
            guard.undeclared_references(&mut declared, references);
        }
        self.body.undeclared_references(&mut declared, references);
    }

    fn resolve(&mut self, resolver: &mut Resolver) {
        if let Some(guard) = &mut self.guard {
            guard.resolve(resolver);
        }

        self.body.resolve(resolver);
    }
}

impl From<ast::Except> for Except {
    fn from(value: ast::Except) -> Self {
        Except {
            binding: value.binding,
            guard: value.guard.map(Into::into),
            body: Box::new(value.body.into()),
        }
    }
}

enum Stmt {
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
    Block {
        forward_declarations: HashSet<Ident>,
        body: Vec<Stmt>,
    },
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

impl Stmt {
    fn undeclared_references(
        &self,
        declared: &mut HashSet<Ident>,
        references: &mut HashSet<Ident>,
    ) {
        match self {
            Stmt::Expr(expr) | Stmt::Print(expr) | Stmt::Throw(expr) => {
                expr.undeclared_references(declared, references);
            }
            Stmt::Var { name, initialiser } => {
                if let Some(initialiser) = initialiser {
                    initialiser.undeclared_references(declared, references);
                }
                declared.insert(name.to_owned());
            }
            Stmt::Return(expr) => {
                if let Some(expr) = expr.as_ref() {
                    expr.undeclared_references(declared, references);
                }
            }
            Stmt::Function {
                is_async: _,
                function,
            } => {
                let mut declared = declared.clone();
                for param in &function.params {
                    declared.insert(param.clone());
                }
                function
                    .body
                    .undeclared_references(&mut declared, references);
                // No need to add the name - it's been forward declared.
            }
            Stmt::Block {
                forward_declarations,
                body,
            } => {
                let mut declared = declared.clone();
                declared.extend(forward_declarations.iter().cloned());
                for stmt in body {
                    stmt.undeclared_references(&mut declared, references);
                }
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                condition.undeclared_references(declared, references);
                then_branch.undeclared_references(declared, references);
                if let Some(else_branch) = else_branch {
                    else_branch.undeclared_references(declared, references);
                }
            }
            Stmt::While { condition, body } => {
                condition.undeclared_references(declared, references);
                body.undeclared_references(declared, references);
            }
            Stmt::For {
                name,
                iterable,
                body,
            } => {
                iterable.undeclared_references(declared, references);
                let mut declared = declared.clone();
                declared.insert(name.clone());
                body.undeclared_references(&mut declared, references);
            }
            Stmt::Loop { body } => body.undeclared_references(declared, references),
            Stmt::Class {
                name: _,
                methods,
                superclass,
            } => {
                if let Some(superclass) = superclass {
                    insert_reference(references, declared, superclass.to_owned());
                }
                for (_, method) in methods {
                    let mut declared = declared.clone();
                    for param in &method.params {
                        declared.insert(param.clone());
                    }
                    method.body.undeclared_references(&mut declared, references);
                }
            }
            Stmt::Try {
                body,
                excepts,
                else_block,
                finally,
            } => {
                let mut body_declared = declared.clone();
                body.undeclared_references(&mut body_declared, references);

                for except in excepts {
                    except.undeclared_references(declared, references);
                }

                if let Some(block) = else_block {
                    let mut declared = declared.clone();
                    block.undeclared_references(&mut declared, references);
                }

                if let Some(block) = finally {
                    let mut declared = declared.clone();
                    block.undeclared_references(&mut declared, references);
                }
            }
            Stmt::Break => {}
        }
    }

    fn resolve(&mut self, resolver: &mut Resolver) {
        match self {
            Stmt::Expr(expr)
            | Stmt::Print(expr)
            | Stmt::Throw(expr)
            | Stmt::Var {
                name: _,
                initialiser: Some(expr),
            } => expr.resolve(resolver),
            Stmt::Break => {
                assert!(resolver.in_loop, "cannot use 'break' outside of loop");
            }
            Stmt::Return(expr) => match resolver.function_type {
                FunctionType::None => panic!("cannot return from a top-level script"),
                FunctionType::Initialiser if expr.is_some() => {
                    panic!("cannot return with a value from an initialiser")
                }
                FunctionType::Initialiser => *expr = Some(Expr::This),
                _ => {}
            },
            Stmt::Function {
                is_async: _,
                function,
            } => {
                let current_function_type = resolver.function_type;
                resolver.function_type = FunctionType::Function;
                function.body.resolve(resolver);
                resolver.function_type = current_function_type;
            }
            Stmt::Block {
                forward_declarations: _,
                body,
            } => {
                for stmt in body {
                    stmt.resolve(resolver);
                }
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                condition.resolve(resolver);
                then_branch.resolve(resolver);
                if let Some(else_branch) = else_branch {
                    else_branch.resolve(resolver);
                }
            }
            Stmt::While { condition, body } => {
                let current_in_loop = resolver.in_loop;
                resolver.in_loop = true;
                condition.resolve(resolver);
                body.resolve(resolver);
                resolver.in_loop = current_in_loop;
            }
            Stmt::For {
                name: _,
                iterable,
                body,
            } => {
                let current_in_loop = resolver.in_loop;
                resolver.in_loop = true;
                iterable.resolve(resolver);
                body.resolve(resolver);
                resolver.in_loop = current_in_loop;
            }
            Stmt::Loop { body } => {
                let current_in_loop = resolver.in_loop;
                resolver.in_loop = true;
                body.resolve(resolver);
                resolver.in_loop = current_in_loop;
            }
            Stmt::Class {
                name: _,
                methods,
                superclass: _,
            } => {
                let current_function_type = resolver.function_type;
                for (_, method) in methods {
                    resolver.function_type = if method.is_initialiser {
                        FunctionType::Initialiser
                    } else {
                        FunctionType::Method
                    };
                    method.body.resolve(resolver);
                }
                resolver.function_type = current_function_type;
            }
            Stmt::Try {
                body,
                excepts,
                else_block,
                finally,
            } => {
                body.resolve(resolver);

                for except in excepts {
                    except.resolve(resolver);
                }

                if let Some(block) = else_block {
                    block.resolve(resolver);
                }

                if let Some(block) = finally {
                    block.resolve(resolver);
                }
            }
            Stmt::Var {
                name: _,
                initialiser: None,
            } => {}
        }
    }
}

impl From<ast::Stmt> for Stmt {
    fn from(value: ast::Stmt) -> Self {
        match value {
            ast::Stmt::Expr(expr) => Stmt::Expr(expr.into()),
            ast::Stmt::Print(expr) => Stmt::Print(expr.into()),
            ast::Stmt::Break => Stmt::Break,
            ast::Stmt::Var { name, initialiser } => Stmt::Var {
                name,
                initialiser: initialiser.map(Into::into),
            },
            ast::Stmt::Return(expr) => Stmt::Return(expr.map(Into::into)),
            ast::Stmt::Function { is_async, function } => Stmt::Function {
                is_async,
                function: function.into(),
            },
            ast::Stmt::Block(stmts) => {
                let (forward_declarations, body) = convert_block(stmts);

                Stmt::Block {
                    forward_declarations,
                    body,
                }
            }
            ast::Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => Stmt::If {
                condition: condition.into(),
                then_branch: Box::new(then_branch.into()),
                else_branch: else_branch.map(|stmt| Box::new(stmt.into())),
            },
            ast::Stmt::While { condition, body } => Stmt::While {
                condition: condition.into(),
                body: Box::new(body.into()),
            },
            ast::Stmt::For {
                name,
                iterable,
                body,
            } => Stmt::For {
                name,
                iterable: iterable.into(),
                body: Box::new(body.into()),
            },
            ast::Stmt::Loop { body } => Stmt::Loop {
                body: Box::new(body.into()),
            },
            ast::Stmt::Class {
                name,
                methods,
                superclass,
            } => Stmt::Class {
                name,
                methods: methods
                    .into_iter()
                    .map(|(is_async, method)| {
                        let mut method: Function = method.into();
                        method
                            .params
                            .push_front(Ident::new("this", method.name.span()));
                        if method.name.ident().is_some_and(|ident| ident == "init") {
                            method.is_initialiser = true;
                        }
                        (is_async, method)
                    })
                    .collect(),
                superclass,
            },
            ast::Stmt::Throw(expr) => Stmt::Throw(expr.into()),
            ast::Stmt::Try {
                body,
                excepts,
                else_block,
                finally,
            } => Stmt::Try {
                body: Box::new(body.into()),
                excepts: excepts.into_iter().map(Into::into).collect(),
                else_block: else_block.map(|block| Box::new(block.into())),
                finally: finally.map(|block| Box::new(block.into())),
            },
        }
    }
}

impl From<Box<ast::Stmt>> for Stmt {
    fn from(value: Box<ast::Stmt>) -> Self {
        (*value).into()
    }
}

enum UnOp {
    Neg,
    Not,
}

impl ToTokens for UnOp {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ch = match self {
            UnOp::Neg => '-',
            UnOp::Not => '!',
        };
        tokens.append(Punct::new(ch, Spacing::Alone));
    }
}

enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
}

impl TryFrom<&syn::BinOp> for BinOp {
    type Error = ();

    fn try_from(value: &syn::BinOp) -> Result<Self, Self::Error> {
        match value {
            syn::BinOp::Add(_) => Ok(BinOp::Add),
            syn::BinOp::Sub(_) => Ok(BinOp::Sub),
            syn::BinOp::Mul(_) => Ok(BinOp::Mul),
            syn::BinOp::Div(_) => Ok(BinOp::Div),
            syn::BinOp::Rem(_) => Ok(BinOp::Rem),
            _ => Err(()),
        }
    }
}

impl ToTokens for BinOp {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ch = match self {
            BinOp::Add => '+',
            BinOp::Sub => '-',
            BinOp::Mul => '*',
            BinOp::Div => '/',
            BinOp::Rem => '%',
        };
        tokens.append(Punct::new(ch, Spacing::Alone));
    }
}

enum Comparison {
    Eq,
    Ne,
    Gt,
    Ge,
    Lt,
    Le,
}

impl ToTokens for Comparison {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let punct = match self {
            Comparison::Eq => quote! { == },
            Comparison::Ne => quote! { != },
            Comparison::Gt => quote! { > },
            Comparison::Ge => quote! { >= },
            Comparison::Lt => quote! { < },
            Comparison::Le => quote! { <= },
        };
        tokens.append_all(punct);
    }
}

enum Expr {
    Literal(LoxValue),
    Array(Vec<Expr>),
    Function {
        upvalues: HashSet<Ident>,
        is_async: bool,
        params: VecDeque<Ident>,
        body: Box<Stmt>,
    },
    Variable(Ident),
    Call {
        callee: Box<Expr>,
        arguments: Vec<Expr>,
    },
    Unary {
        operator: UnOp,
        right: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        operator: BinOp,
        right: Box<Expr>,
    },
    Comparison {
        left: Box<Expr>,
        operator: Comparison,
        right: Box<Expr>,
    },
    And {
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Or {
        left: Box<Expr>,
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

impl Expr {
    fn undeclared_references(
        &self,
        declared: &mut HashSet<Ident>,
        references: &mut HashSet<Ident>,
    ) {
        match self {
            Expr::Array(array) => {
                for expr in array {
                    expr.undeclared_references(declared, references);
                }
            }
            Expr::Function {
                upvalues: _,
                is_async: _,
                params,
                body,
            } => {
                let mut declared = declared.clone();
                for param in params {
                    declared.insert(param.clone());
                }
                body.undeclared_references(&mut declared, references);
            }
            Expr::Variable(ident) => {
                insert_reference(references, declared, ident.clone());
            }
            Expr::Call { callee, arguments } => {
                callee.undeclared_references(declared, references);
                for expr in arguments {
                    expr.undeclared_references(declared, references);
                }
            }
            Expr::Unary { operator: _, right } => right.undeclared_references(declared, references),
            Expr::Binary {
                left,
                operator: _,
                right,
            }
            | Expr::Comparison {
                left,
                operator: _,
                right,
            }
            | Expr::And { left, right }
            | Expr::Or { left, right } => {
                left.undeclared_references(declared, references);
                right.undeclared_references(declared, references);
            }
            Expr::Assign { name, value } => {
                insert_reference(references, declared, name.clone());
                value.undeclared_references(declared, references);
            }
            Expr::Get { object, name: _ } => {
                object.undeclared_references(declared, references);
            }
            Expr::Set {
                object,
                name: _,
                value,
            } => {
                object.undeclared_references(declared, references);
                value.undeclared_references(declared, references);
            }
            Expr::Super { arguments } => {
                for expr in arguments {
                    expr.undeclared_references(declared, references);
                }
            }
            Expr::Await { left } => left.undeclared_references(declared, references),
            Expr::Index { left, index } => {
                left.undeclared_references(declared, references);
                index.undeclared_references(declared, references);
            }
            Expr::IndexSet { left, index, value } => {
                left.undeclared_references(declared, references);
                index.undeclared_references(declared, references);
                value.undeclared_references(declared, references);
            }
            Expr::Map(map) => {
                for (key, value) in map {
                    key.undeclared_references(declared, references);
                    value.undeclared_references(declared, references);
                }
            }
            Expr::Literal(_) | Expr::This => (),
        }
    }

    fn resolve(&mut self, resolver: &mut Resolver) {
        match self {
            Expr::Array(array) => {
                for expr in array {
                    expr.resolve(resolver);
                }
            }
            Expr::Function {
                upvalues: _,
                is_async: _,
                params: _,
                body,
            } => {
                let current_function_type = resolver.function_type;
                resolver.function_type = FunctionType::Function;
                body.resolve(resolver);
                resolver.function_type = current_function_type;
            }

            Expr::Call { callee, arguments } => {
                callee.resolve(resolver);
                for argument in arguments {
                    argument.resolve(resolver);
                }
            }
            Expr::Unary { operator: _, right } => right.resolve(resolver),
            Expr::Binary {
                left,
                operator: _,
                right,
            }
            | Expr::Comparison {
                left,
                operator: _,
                right,
            }
            | Expr::And { left, right }
            | Expr::Or { left, right } => {
                left.resolve(resolver);
                right.resolve(resolver);
            }
            Expr::Assign { name: _, value } => value.resolve(resolver),
            Expr::Get { object, name: _ } => object.resolve(resolver),
            Expr::Set {
                object,
                name: _,
                value,
            } => {
                object.resolve(resolver);
                value.resolve(resolver);
            }
            Expr::Super { arguments } => {
                assert!(
                    resolver.function_type.is_method(),
                    "cannot use 'super' outside of a method"
                );
                for argument in arguments {
                    argument.resolve(resolver);
                }
            }
            Expr::Await { left } => left.resolve(resolver),
            Expr::Index { left, index } => {
                left.resolve(resolver);
                index.resolve(resolver);
            }
            Expr::IndexSet { left, index, value } => {
                left.resolve(resolver);
                index.resolve(resolver);
                value.resolve(resolver);
            }
            Expr::Map(map) => {
                for (key, value) in map {
                    key.resolve(resolver);
                    value.resolve(resolver);
                }
            }
            Expr::Literal(_) | Expr::Variable(_) | Expr::This => {}
        }
    }
}

#[allow(clippy::fallible_impl_from)]
impl From<ast::Expr> for Expr {
    fn from(value: ast::Expr) -> Self {
        match value {
            ast::Expr::Literal(value) => Expr::Literal(value),
            ast::Expr::Array(array) => Expr::Array(array.into_iter().map(Into::into).collect()),
            ast::Expr::Function {
                is_async,
                params,
                body,
            } => {
                let body = body.into();
                Expr::Function {
                    upvalues: get_upvalues(&body, &params),
                    is_async,
                    params: params.into(),
                    body: Box::new(body),
                }
            }
            ast::Expr::Variable(name) => Expr::Variable(name),
            ast::Expr::Call { callee, arguments } => Expr::Call {
                callee: Box::new(callee.into()),
                arguments: arguments.into_iter().map(Into::into).collect(),
            },
            ast::Expr::Unary { operator, expr } => {
                let operator = match operator {
                    syn::UnOp::Neg(_) => UnOp::Neg,
                    syn::UnOp::Not(_) => UnOp::Not,
                    _ => panic!("invalid operator '{operator:?}' parsed"),
                };
                Expr::Unary {
                    operator,
                    right: Box::new(expr.into()),
                }
            }
            ast::Expr::Binary {
                left,
                operator,
                right,
            } => {
                let left = Box::new(left.into());
                let right = Box::new(right.into());
                match operator {
                    syn::BinOp::Add(_) => Expr::Binary {
                        left,
                        operator: BinOp::Add,
                        right,
                    },
                    syn::BinOp::Sub(_) => Expr::Binary {
                        left,
                        operator: BinOp::Sub,
                        right,
                    },
                    syn::BinOp::Mul(_) => Expr::Binary {
                        left,
                        operator: BinOp::Mul,
                        right,
                    },
                    syn::BinOp::Div(_) => Expr::Binary {
                        left,
                        operator: BinOp::Div,
                        right,
                    },
                    syn::BinOp::Rem(_) => Expr::Binary {
                        left,
                        operator: BinOp::Rem,
                        right,
                    },
                    syn::BinOp::Eq(_) => Expr::Comparison {
                        left,
                        operator: Comparison::Eq,
                        right,
                    },
                    syn::BinOp::Ne(_) => Expr::Comparison {
                        left,
                        operator: Comparison::Ne,
                        right,
                    },
                    syn::BinOp::Gt(_) => Expr::Comparison {
                        left,
                        operator: Comparison::Gt,
                        right,
                    },
                    syn::BinOp::Ge(_) => Expr::Comparison {
                        left,
                        operator: Comparison::Ge,
                        right,
                    },
                    syn::BinOp::Lt(_) => Expr::Comparison {
                        left,
                        operator: Comparison::Lt,
                        right,
                    },
                    syn::BinOp::Le(_) => Expr::Comparison {
                        left,
                        operator: Comparison::Le,
                        right,
                    },
                    syn::BinOp::And(_) => Expr::And { left, right },
                    syn::BinOp::Or(_) => Expr::Or { left, right },
                    _ => panic!("invalid operator '{operator:?}' parsed"),
                }
            }
            ast::Expr::Assign { name, value } => Expr::Assign {
                name,
                value: Box::new(value.into()),
            },
            ast::Expr::This => Expr::This,
            ast::Expr::Get { object, name } => Expr::Get {
                object: Box::new(object.into()),
                name,
            },
            ast::Expr::Set {
                object,
                name,
                value,
            } => Expr::Set {
                object: Box::new(object.into()),
                name,
                value: Box::new(value.into()),
            },
            ast::Expr::Super { arguments } => Expr::Super {
                arguments: arguments.into_iter().map(Into::into).collect(),
            },
            ast::Expr::Await { left } => Expr::Await {
                left: Box::new(left.into()),
            },
            ast::Expr::Index { left, index } => Expr::Index {
                left: Box::new(left.into()),
                index: Box::new(index.into()),
            },
            ast::Expr::IndexSet { left, index, value } => Expr::IndexSet {
                left: Box::new(left.into()),
                index: Box::new(index.into()),
                value: Box::new(value.into()),
            },
            ast::Expr::Map(map) => Expr::Map(
                map.into_iter()
                    .map(|(key, value)| (key.into(), value.into()))
                    .collect(),
            ),
        }
    }
}

impl From<Box<ast::Expr>> for Expr {
    fn from(value: Box<ast::Expr>) -> Self {
        (*value).into()
    }
}
