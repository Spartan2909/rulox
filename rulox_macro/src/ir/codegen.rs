use super::Except;
use super::Expr;
use super::Function;
use super::Ir;
use super::Stmt;

use std::collections::HashSet;
use std::collections::VecDeque;

use proc_macro2::Punct;
use proc_macro2::Spacing;
use proc_macro2::TokenStream;

use quote::quote;
use quote::ToTokens;
use quote::TokenStreamExt;

use syn::Ident;

impl ToTokens for Ir {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.append_all(quote! { let __rulox_fn_name = "<script>"; });
        tokens.append_all(block_to_token_stream(
            &self.forward_declarations,
            &self.statements,
        ));
    }
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
            Stmt::Function {
                is_async,
                function:
                    Function {
                        name,
                        params,
                        body,
                        upvalues,
                        is_initialiser: _,
                    },
            } => {
                let function_object = function_expr_to_tokens(
                    upvalues,
                    params,
                    body,
                    true,
                    false,
                    None,
                    Some(name),
                    *is_async,
                );

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
                        Some(__rulox_helpers::LoxRc::clone(#class))
                    }
                } else {
                    quote! { None }
                };

                let mut methods_tokens = TokenStream::new();
                for (is_async, method) in methods {
                    let method_tokens = function_expr_to_tokens(
                        &method.upvalues,
                        &method.params,
                        &method.body,
                        false,
                        method.is_initialiser,
                        Some(&method.name),
                        Some(&method.name),
                        *is_async,
                    );
                    let method_name = &method.name;
                    methods_tokens.append_all(
                        quote! { (stringify!(#method_name), __rulox_helpers::LoxRc::new(#method_tokens).into()), },
                    );
                }

                tokens.append_all(quote! {
                    #name.overwrite(LoxValue::Class(__rulox_helpers::LoxRc::new(__rulox_helpers::LoxClass::new(
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

fn function_expr_to_tokens(
    upvalues: &HashSet<Ident>,
    params: &VecDeque<Ident>,
    body: &Stmt,
    value_wrap: bool,
    is_initialiser: bool,
    insert_super_fn: Option<&Ident>,
    fn_name: Option<&Ident>,
    is_async: bool,
) -> TokenStream {
    let mut inner = TokenStream::new();
    if is_async {
        inner.append_all(quote! {
            move |mut __args|
                -> Box<dyn __rulox_helpers::Future<Output = __rulox_helpers::LoxResult> + Send + Sync + 'static>
        });
    } else {
        inner.append_all(quote! { move |mut __args| -> __rulox_helpers::LoxResult });
    }

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

    let mut params_tokens = TokenStream::new();
    for param in params {
        let name = param.to_string();
        let name = syn::LitStr::new(&name, param.span());
        params_tokens.append_all(quote! { #name, });
    }

    let value = if is_async {
        if upvalues.is_empty() {
            inner.append_all(quote! { { Box::new(async move {
                #expr_body
                #[allow(unreachable_code)] #tail
            }) } });
        } else {
            let mut closes = TokenStream::new();
            for upvalue in upvalues {
                closes.append_all(quote! { let #upvalue = #upvalue.close_over(); });
            }
            inner.append_all(quote! { {
               #closes
               Box::new(async move {
                   #expr_body
                   #[allow(unreachable_code)] #tail
               })
            } })
        }

        let mut value =
            quote! { __rulox_helpers::Coroutine::new(Box::new(#inner), vec![#params_tokens]) };
        if value_wrap {
            value = quote! { LoxValue::Coroutine(__rulox_helpers::LoxRc::new(#value)) };
        }
        value
    } else {
        inner.append_all(quote! { { #expr_body #[allow(unreachable_code)] #tail } });
        let mut value = quote! { __rulox_helpers::LoxFn::new(#inner, vec![#params_tokens]) };
        if value_wrap {
            value = quote! { LoxValue::function(#value) };
        }
        value
    };

    if upvalues.is_empty() {
        value
    } else {
        let mut closes = TokenStream::new();
        for upvalue in upvalues {
            closes.append_all(quote! { let #upvalue = #upvalue.close_over(); });
        }
        quote! { {
            #closes
            #value
        } }
    }
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
                is_async,
                params,
                body,
            } => {
                tokens.append_all(function_expr_to_tokens(
                    upvalues, params, body, true, false, None, None, *is_async,
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
            Expr::Await { left } => tokens.append_all(quote! { (#left).await? }),
        }
    }
}
