use crate::ast;

use std::collections::HashSet;
use std::collections::VecDeque;

use proc_macro2::Punct;
use proc_macro2::Spacing;
use proc_macro2::TokenStream;

use quote::quote;
use quote::ToTokens;
use quote::TokenStreamExt;

use rulox_types::LoxValue;

use syn::Ident;

#[derive(Debug, Clone, Copy)]
enum FunctionType {
    None,
    Function,
    Method,
    Initialiser,
}

impl FunctionType {
    fn is_method(self) -> bool {
        matches!(self, FunctionType::Initialiser | FunctionType::Method)
    }
}

struct Resolver {
    function_type: FunctionType,
    in_loop: bool,
}

impl Resolver {
    fn new() -> Resolver {
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

impl ToTokens for Ir {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.append_all(quote! { let __rulox_fn_name = "<script>"; });
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
    params: VecDeque<Ident>,
    body: Box<Stmt>,
    upvalues: HashSet<Ident>,
    is_initialiser: bool,
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
            | Stmt::Function(Function { name, .. })
            | Stmt::Var { name, .. } => {
                forward_declarations.insert(name.clone());
            }
            _ => {}
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
        tokens.append_all(quote! {
            #[allow(non_snake_case)]
            let #name = __rulox_helpers::LoxVariable::new(LoxValue::Undefined(stringify!(#name)));
        });
    }

    for stmt in statements {
        stmt.to_tokens(&mut tokens);
    }

    tokens
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
            guard: value.guard.map(|expr| expr.into()),
            body: Box::new(value.body.into()),
        }
    }
}

impl ToTokens for Except {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let pattern = if let Some(binding) = &self.binding {
            if let Some(guard) = &self.guard {
                quote! { Err(#binding) if (#guard).is_truthy() }
            } else {
                quote! { Err(#binding) }
            }
        } else {
            quote! { Err(_) }
        };
        let body = &self.body;
        tokens.append_all(quote! { #pattern => { #body } })
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
        name: Ident,
        methods: Vec<Function>,
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
                expr.undeclared_references(declared, references)
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
                name: _,
                methods,
                superclass,
            } => {
                if let Some(superclass) = superclass {
                    insert_reference(references, declared, superclass.to_owned());
                }
                for method in methods {
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
            Stmt::Expr(expr) | Stmt::Print(expr) | Stmt::Throw(expr) => expr.resolve(resolver),
            Stmt::Break => {
                if !resolver.in_loop {
                    panic!("cannot use 'break' outside of loop");
                }
            }
            Stmt::Var {
                name: _,
                initialiser: Some(expr),
            } => expr.resolve(resolver),
            Stmt::Return(expr) => match resolver.function_type {
                FunctionType::None => panic!("cannot return from a top-level script"),
                FunctionType::Initialiser if expr.is_some() => {
                    panic!("cannot return with a value from an initialiser")
                }
                FunctionType::Initialiser => *expr = Some(Expr::This),
                _ => {}
            },
            Stmt::Function(function) => {
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
                for method in methods {
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
                initialiser: initialiser.map(|expr| expr.into()),
            },
            ast::Stmt::Return(expr) => Stmt::Return(expr.map(|expr| expr.into())),
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
                name,
                methods: methods
                    .into_iter()
                    .map(|method| {
                        let mut method: Function = method.into();
                        method
                            .params
                            .push_front(Ident::new("this", method.name.span()));
                        if method.name == "init" {
                            method.is_initialiser = true;
                        }
                        method
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
                excepts: excepts.into_iter().map(|x| x.into()).collect(),
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
                        #name.overwrite(#initialiser);
                    });
                }
            }
            Stmt::Return(expr) => {
                let expr = if let Some(expr) = expr {
                    quote! { #expr }
                } else {
                    quote! { LoxValue::Nil }
                };
                tokens.append_all(quote! { return Ok(#expr); });
            }
            Stmt::Function(Function {
                name,
                params,
                body,
                upvalues,
                is_initialiser: _,
            }) => {
                let function_object =
                    function_expr_to_tokens(upvalues, params, body, true, false, None, Some(name));

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
                let condition = wrap_extract(quote! { #condition.try_into() });
                tokens.append_all(quote! { if #condition { #then_branch } });
                if let Some(branch) = else_branch {
                    tokens.append_all(quote! { else { #branch } });
                }
            }
            Stmt::While { condition, body } => {
                let condition = wrap_extract(quote! { #condition.try_into() });
                tokens.append_all(quote! { while #condition { #body } });
            }
            Stmt::For {
                name,
                iterable,
                body,
            } => {
                tokens.append_all(quote! { for __tmp in #iterable.into_iter() {
                    let #name = __rulox_helpers::LoxVariable::new(__tmp);
                    { #body }
                } });
            }
            Stmt::Loop { body } => {
                tokens.append_all(quote! { loop { #body } });
            }
            Stmt::Class {
                name,
                methods,
                superclass,
            } => {
                let superclass = if let Some(superclass) = superclass {
                    let get = wrap_extract(quote! { #superclass.get() });
                    let class = wrap_extract(quote! { #get.as_class() });
                    quote! {
                        Some(__rulox_helpers::Rc::clone(#class))
                    }
                } else {
                    quote! { None }
                };

                let mut methods_tokens = TokenStream::new();
                for method in methods {
                    let method_tokens = function_expr_to_tokens(
                        &method.upvalues,
                        &method.params,
                        &method.body,
                        false,
                        method.is_initialiser,
                        Some(&method.name),
                        Some(&method.name),
                    );
                    let method_name = &method.name;
                    methods_tokens.append_all(
                        quote! { (stringify!(#method_name), __rulox_helpers::Rc::new(#method_tokens)), },
                    );
                }

                tokens.append_all(quote! {
                    #name.overwrite(LoxValue::Class(__rulox_helpers::Rc::new(__rulox_helpers::LoxClass::new(
                        stringify!(#name),
                        __rulox_helpers::HashMap::from_iter([#methods_tokens]),
                        #superclass,
                    ))));
                });
            }
            Stmt::Throw(expr) => tokens.append_all(quote! { return Err((#expr).into_error()); }),
            Stmt::Try {
                body,
                excepts,
                else_block,
                finally,
            } => {
                let else_block = else_block
                    .as_ref()
                    .map_or(TokenStream::new(), |block| block.to_token_stream());
                let mut catches = TokenStream::new();
                for except in excepts {
                    except.to_tokens(&mut catches);
                }
                let match_block = quote! {
                    match
                        (|| -> Result<(), LoxError> { #body #[allow(unreachable_code)] Ok(()) })()
                            .map_err(|err| __rulox_helpers::LoxVariable::new(err.into_value()))
                    {
                        Ok(()) => { #else_block }
                        #catches
                        #[allow(unreachable_code)]
                        Err(_) => {}
                    }
                };
                if let Some(finally) = finally {
                    tokens.append_all(quote! { {
                        let __result = #match_block;
                        { #finally }
                        __result
                    } });
                } else {
                    tokens.append_all(match_block);
                }
            }
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
                if !resolver.function_type.is_method() {
                    panic!("cannot use 'super' outside of a method");
                }
                for argument in arguments {
                    argument.resolve(resolver);
                }
            }
            Expr::Literal(_) | Expr::Variable(_) | Expr::This => {}
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
                    params: params.into(),
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
                arguments: arguments.into_iter().map(|expr| expr.into()).collect(),
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
    params: &VecDeque<Ident>,
    body: &Stmt,
    value_wrap: bool,
    is_initialiser: bool,
    insert_super_fn: Option<&Ident>,
    fn_name: Option<&Ident>,
) -> TokenStream {
    let mut tokens = TokenStream::new();

    let mut inner = TokenStream::new();
    inner.append_all(quote! { move |mut __args| -> __rulox_helpers::LoxResult });

    let mut expr_body = quote! { let mut __drain = __args.drain(); };
    for param in params {
        expr_body.append_all(
            quote! { let #param = __rulox_helpers::LoxVariable::new(__drain.next().unwrap()); },
        );
    }

    if let Some(name) = insert_super_fn {
        let this = wrap_extract(quote! { this.get() });
        let fun = wrap_extract(quote! { _this.clone().super_fn(stringify!(#name)) });
        expr_body.append_all(quote! {
            let __super = |args: __rulox_helpers::LoxArgs| -> __rulox_helpers::LoxResult {
                let _this = #this;
                let fun = #fun;
                let bound = LoxValue::bind(fun, _this);
                bound.call(args)
            };
        });
    }

    let fn_name = fn_name.map_or(
        quote! { "<anonymous function>" },
        |name| quote! { stringify!(#name) },
    );

    expr_body.append_all(quote! { let __rulox_fn_name = #fn_name; });

    expr_body.append_all(quote! { { #body } });

    let tail = if is_initialiser {
        quote! { this.get() }
    } else {
        quote! { Ok(LoxValue::Nil) }
    };

    inner.append_all(quote! { { #expr_body #[allow(unreachable_code)] #tail } });

    let mut params_tokens = TokenStream::new();
    for param in params {
        let name = param.to_string();
        let name = syn::LitStr::new(&name, param.span());
        params_tokens.append_all(quote! { #name, });
    }

    let mut value = quote! { __rulox_helpers::LoxFn::new(#inner, vec![#params_tokens]) };
    if value_wrap {
        value = quote! { LoxValue::function(#value) };
    }

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

fn wrap_extract(tokens: TokenStream) -> TokenStream {
    quote! { __rulox_helpers::extract!((#tokens), __rulox_fn_name) }
}

impl ToTokens for Expr {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Expr::Literal(value) => {
                value.to_tokens(tokens);
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
                tokens.append_all(function_expr_to_tokens(
                    upvalues, params, body, true, false, None, None,
                ));
            }
            Expr::Variable(var) => {
                tokens.append_all(wrap_extract(quote! { #var.get() }));
            }
            Expr::Call { callee, arguments } => {
                let mut inner = TokenStream::new();
                inner.append_separated(arguments, Punct::new(',', Spacing::Alone));

                tokens.append_all(wrap_extract(quote! { #callee.call([#inner].into()) }));
            }
            Expr::Unary { operator, right } => {
                tokens.append_all(wrap_extract(quote! { #operator #right }));
            }
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                tokens.append_all(wrap_extract(quote! { #left #operator #right }));
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
            Expr::This => tokens.append_all(wrap_extract(quote! { this.get() })),
            Expr::Get { object, name } => {
                tokens.append_all(wrap_extract(quote! { (#object).get(stringify!(#name)) }));
            }
            Expr::Set {
                object,
                name,
                value,
            } => {
                tokens.append_all(wrap_extract(
                    quote! { (#object).set(stringify!(#name), #value) },
                ));
            }
            Expr::Super { arguments } => {
                let mut inner = TokenStream::new();
                inner.append_separated(arguments, Punct::new(',', Spacing::Alone));

                tokens.append_all(quote! { __super([#inner].into())? });
            }
        }
    }
}
