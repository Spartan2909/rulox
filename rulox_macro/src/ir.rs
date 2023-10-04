use crate::ast;

use std::collections::HashSet;

use proc_macro2::Punct;
use proc_macro2::Spacing;
use proc_macro2::TokenStream;

use quote::quote;
use quote::ToTokens;
use quote::TokenStreamExt;

use rulox_types::LoxValue;

use syn::Ident;

pub struct Ir {
    forward_declarations: HashSet<Ident>,
    statements: Vec<Stmt>,
}

impl From<ast::LoxProgram> for Ir {
    fn from(value: ast::LoxProgram) -> Self {
        let (forward_declarations, statements) = convert_block(value.statements);
        Ir {
            forward_declarations,
            statements,
        }
    }
}

impl ToTokens for Ir {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.append_all(block_to_token_stream(
            &self.forward_declarations,
            &self.statements,
        ));
    }
}

fn insert_reference(references: &mut HashSet<Ident>, declared: &HashSet<Ident>, ident: Ident) {
    if !declared.contains(&ident) {
        references.insert(ident);
    }
}

struct Function {
    name: Ident,
    params: Vec<Ident>,
    body: Box<Stmt>,
    upvalues: HashSet<Ident>,
}

fn get_upvalues(body: &Stmt, params: &[Ident]) -> HashSet<Ident> {
    let mut upvalues = HashSet::new();
    let mut declared = HashSet::from_iter(params.iter().cloned());
    body.undeclared_references(&mut declared, &mut upvalues);
    upvalues
}

impl From<ast::Function> for Function {
    fn from(value: ast::Function) -> Self {
        let body: Stmt = value.body.into();
        let upvalues = get_upvalues(&body, &value.params);
        Function {
            name: value.name,
            params: value.params,
            body: Box::new(body),
            upvalues,
        }
    }
}

fn convert_block(stmts: Vec<ast::Stmt>) -> (HashSet<Ident>, Vec<Stmt>) {
    let mut forward_declarations = HashSet::new();
    let mut body = vec![];

    for stmt in stmts {
        let stmt: Stmt = stmt.into();
        if let Stmt::Class { name, .. } = &stmt {
            forward_declarations.insert(name.clone());
        } else if let Stmt::Function(Function { name, .. }) = &stmt {
            forward_declarations.insert(name.clone());
        }
        body.push(stmt);
    }

    (forward_declarations, body)
}

fn block_to_token_stream(
    forward_declarations: &HashSet<Ident>,
    statements: &[Stmt],
) -> TokenStream {
    let mut tokens = TokenStream::new();

    for name in forward_declarations {
        tokens.append_all(quote! { let #name = LoxVariable::new(LoxValue::Nil); });
    }

    for stmt in statements {
        stmt.to_tokens(&mut tokens);
    }

    tokens
}

enum Stmt {
    Expr(Expr),
    Print(Expr),
    Break,
    Var {
        name: Ident,
        initialiser: Option<Expr>,
    },
    Return(Expr),
    Function(Function),
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
        forward_declarations: HashSet<Ident>,
        name: Ident,
        methods: Vec<Function>,
        superclass: Option<Ident>,
    },
}

impl Stmt {
    fn undeclared_references(
        &self,
        declared: &mut HashSet<Ident>,
        references: &mut HashSet<Ident>,
    ) {
        match self {
            Stmt::Expr(expr) => expr.undeclared_references(declared, references),
            Stmt::Print(expr) => expr.undeclared_references(declared, references),
            Stmt::Var { name, initialiser } => {
                if let Some(initialiser) = initialiser {
                    initialiser.undeclared_references(declared, references);
                }
                declared.insert(name.to_owned());
            }
            Stmt::Return(expr) => expr.undeclared_references(declared, references),
            Stmt::Function(function) => {
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
                forward_declarations,
                name: _,
                methods,
                superclass,
            } => {
                if let Some(superclass) = superclass {
                    insert_reference(references, declared, superclass.to_owned());
                }
                let mut declared = declared.clone();
            }
            Stmt::Break => {}
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
                initialiser: initialiser.map(|expr| expr.into()),
            },
            ast::Stmt::Return(expr) => Stmt::Return(expr.into()),
            ast::Stmt::Function(function) => Stmt::Function(function.into()),
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
                forward_declarations: todo!(),
                name,
                methods: methods.into_iter().map(|method| method.into()).collect(),
                superclass,
            },
        }
    }
}

impl From<Box<ast::Stmt>> for Stmt {
    fn from(value: Box<ast::Stmt>) -> Self {
        (*value).into()
    }
}

impl ToTokens for Stmt {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Stmt::Expr(expr) => {
                expr.to_tokens(tokens);
                tokens.append(Punct::new(';', Spacing::Alone));
            }
            Stmt::Print(expr) => {
                tokens.append_all(quote! { println!("{}", #expr); });
                tokens.append(Punct::new(';', Spacing::Alone));
            }
            Stmt::Break => {
                tokens.append_all(quote! { break; });
            }
            Stmt::Var { name, initialiser } => {
                if let Some(initialiser) = initialiser {
                    tokens.append_all(quote! {
                        let #name = LoxVariable::new(#initialiser);
                    });
                } else {
                    tokens.append_all(quote! {
                        let #name = LoxVariable::new(LoxValue::Nil);
                    });
                }
            }
            Stmt::Return(expr) => {
                tokens.append_all(quote! { return #expr; });
            }
            Stmt::Function(Function {
                name,
                params,
                body,
                upvalues,
            }) => {
                let function_object = function_expr_to_tokens(upvalues, params, body);

                let mut params_tokens = TokenStream::new();
                for param in params {
                    let name = param.to_string();
                    let name = syn::LitStr::new(&name, param.span());
                    params_tokens.append_all(quote! { #name, });
                }

                tokens.append_all(quote! {
                    #name.overwrite(#function_object);
                });
            }
            Stmt::Block {
                forward_declarations,
                body,
            } => {
                let inner = block_to_token_stream(forward_declarations, body);
                tokens.append_all(quote! { { #inner } });
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                tokens.append_all(quote! { if extract(#condition.try_into()) { #then_branch } });
                if let Some(branch) = else_branch {
                    tokens.append_all(quote! { else { #branch } });
                }
            }
            Stmt::While { condition, body } => {
                tokens.append_all(quote! { while extract(#condition.try_into()) { #body } });
            }
            Stmt::For {
                name,
                iterable,
                body,
            } => {
                tokens.append_all(quote! { for __tmp in #iterable.into_iter() {
                    let #name = LoxVariable::new(__tmp);
                    { #body }
                } });
            }
            Stmt::Loop { body } => {
                tokens.append_all(quote! { loop { #body } });
            }
            Stmt::Class {
                forward_declarations,
                name,
                methods,
                superclass,
            } => {}
        }
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
}

impl ToTokens for BinOp {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ch = match self {
            BinOp::Add => '+',
            BinOp::Sub => '-',
            BinOp::Mul => '*',
            BinOp::Div => '/',
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
            Expr::Literal(_) => (),
        }
    }
}

impl From<ast::Expr> for Expr {
    fn from(value: ast::Expr) -> Self {
        match value {
            ast::Expr::Literal(value) => Expr::Literal(value),
            ast::Expr::Array(array) => {
                Expr::Array(array.into_iter().map(|expr| expr.into()).collect())
            }
            ast::Expr::Function { params, body } => {
                let body = body.into();
                Expr::Function {
                    upvalues: get_upvalues(&body, &params),
                    params,
                    body: Box::new(body),
                }
            }
            ast::Expr::Variable(name) => Expr::Variable(name),
            ast::Expr::Call { callee, arguments } => Expr::Call {
                callee: Box::new(callee.into()),
                arguments: arguments.into_iter().map(|expr| expr.into()).collect(),
            },
            ast::Expr::Unary { operator, expr } => {
                let operator = match operator {
                    syn::UnOp::Neg(_) => UnOp::Neg,
                    syn::UnOp::Not(_) => UnOp::Not,
                    _ => panic!("invalid operator '{:?}' parsed", operator),
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
                    _ => panic!("invalid operator '{:?}' parsed", operator),
                }
            }
            ast::Expr::Assign { name, value } => Expr::Assign {
                name,
                value: Box::new(value.into()),
            },
        }
    }
}

impl From<Box<ast::Expr>> for Expr {
    fn from(value: Box<ast::Expr>) -> Self {
        (*value).into()
    }
}

fn function_expr_to_tokens(
    upvalues: &HashSet<Ident>,
    params: &[Ident],
    body: &Stmt,
) -> TokenStream {
    let mut tokens = TokenStream::new();

    let mut inner = TokenStream::new();
    inner.append_all(quote! { move |mut __args: Vec<LoxValue>| -> LoxValue });

    let mut expr_body = quote! { let mut __drain = __args.drain(..); };
    for param in params {
        expr_body.append_all(quote! { let #param = LoxVariable::new(__drain.next().unwrap()); });
    }

    expr_body.append_all(quote! { { #body } });

    inner.append_all(quote! { { #expr_body #[allow(unreachable_code)] LoxValue::Nil } });

    let mut params_tokens = TokenStream::new();
    for param in params {
        let name = param.to_string();
        let name = syn::LitStr::new(&name, param.span());
        params_tokens.append_all(quote! { #name, });
    }

    let value = quote! { LoxValue::function(LoxFn::new(#inner, vec![#params_tokens])) };

    if upvalues.is_empty() {
        tokens.append_all(value);
    } else {
        let mut closes = TokenStream::new();
        for upvalue in upvalues {
            closes.append_all(quote! { let #upvalue = #upvalue.close_over(); });
        }
        tokens.append_all(quote! { {
            #closes
            #value
        } })
    }

    tokens
}

impl ToTokens for Expr {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Expr::Literal(value) => {
                if let LoxValue::Function(..) = value {
                    todo!()
                } else {
                    value.to_tokens(tokens);
                }
            }
            Expr::Array(arr) => {
                let mut inner = TokenStream::new();
                inner.append_separated(arr, Punct::new(',', Spacing::Alone));

                tokens.append_all(quote! { LoxValue::from(vec![#inner]) })
            }
            Expr::Function {
                upvalues,
                params,
                body,
            } => {
                tokens.append_all(function_expr_to_tokens(upvalues, params, body));
            }
            Expr::Variable(var) => {
                tokens.append_all(quote! { #var.get() });
            }
            Expr::Call { callee, arguments } => {
                let mut inner = TokenStream::new();
                inner.append_separated(arguments, Punct::new(',', Spacing::Alone));

                tokens.append_all(quote! { #callee.lox_call(vec![#inner]) });
            }
            Expr::Unary { operator, right } => {
                operator.to_tokens(tokens);
                right.to_tokens(tokens);
            }
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                tokens.append_all(quote! { extract(#left #operator #right) });
            }
            Expr::Comparison {
                left,
                operator,
                right,
            } => {
                tokens.append_all(quote! { LoxValue::Bool(#left #operator #right) });
            }
            Expr::And { left, right } => {
                tokens.append_all(quote! { {
                    let _left = #left;
                    if _left.is_truthy() {
                        #right
                    } else {
                        _left
                    }
                } });
            }
            Expr::Or { left, right } => {
                tokens.append_all(quote! { {
                    let _left = #left;
                    if _left.is_truthy() {
                        _left
                    } else {
                        #right
                    }
                } });
            }
            Expr::Assign { name, value } => tokens.append_all(quote! { #name.overwrite(#value) }),
        }
    }
}