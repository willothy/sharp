use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::{
        Block, Expression, FunctionCall, FunctionDeclaration, FunctionDefinition,
        FunctionParameter, IfExpression, LoopStatement, MemberAccess, Module, ModulePath, NodeSpan,
        ResultStatement, ReturnStatement, ScopeResolution, Statement, StructDeclaration,
        StructField, StructInitializer, StructInitializerField, Use, VarAssignment, VarDeclaration,
    },
    debug,
    tokenizer::{
        AssignmentOperator, Attribute, Keyword, Operator, OperatorType, Symbol, Token, TokenKind,
        TokenPosition,
    },
};

pub fn parse<'parse>(
    tokens: Vec<Token>,
    source: String,
    mod_name: String,
) -> Result<Rc<RefCell<Module>>, String> {
    let mut parser = Parser::new(tokens, source);
    let module = parser.module(mod_name, None)?;
    Ok(module)
}

struct Parser<'parser> {
    source: String,
    tokens: Vec<Token<'parser>>,
    current: usize,
    lookahead: usize,
    impl_ctx: Option<String>,
    current_module: Option<Rc<RefCell<Module>>>,
    unclaimed_attrs: Option<Vec<Attribute>>,
}

impl<'parser> Parser<'parser> {
    pub fn new(tokens: Vec<Token<'parser>>, source: String) -> Self {
        Self {
            tokens,
            current: 0,
            lookahead: 1,
            source,
            impl_ctx: None,
            current_module: None,
            unclaimed_attrs: None,
        }
    }

    pub fn current(&self) -> Option<TokenKind> {
        match self.tokens.get(self.current) {
            Some(token) => Some(token.kind.clone()),
            None => None,
        }
    }

    pub fn current_position(&self) -> Option<TokenPosition> {
        match self.tokens.get(self.current) {
            Some(token) => Some(TokenPosition {
                line: token.position.location_line() as usize,
                column: token.position.get_column(),
                offset: token.position.location_offset(),
            }),
            None => None,
        }
    }

    pub fn span_start(&self) -> Result<NodeSpan, String> {
        let Some(span) = self.tokens.get(self.current).map(|token| token.position) else {
            return Err(format!(
                "Expected span",
            ));
        };
        Ok(NodeSpan {
            start: TokenPosition {
                line: span.location_line() as usize,
                column: span.get_column(),
                offset: span.location_offset(),
            },
            end: TokenPosition::new(),
        })
    }

    pub fn span_end(&self, mut span: NodeSpan) -> Result<NodeSpan, String> {
        let Some(end) = self.tokens.get(if self.current == 0 {0} else {self.current-1}).map(|token| token.position) else {
            return Err(format!(
                "Expected span",
            ));
        };
        span.end = TokenPosition::from(end);
        Ok(span)
    }

    fn debug_position(&self) -> String {
        match self.current_position() {
            Some(position) => format!("{}", position),
            None => "at EOF".to_string(),
        }
    }

    pub fn lookahead(&self) -> Option<TokenKind> {
        match self.tokens.get(self.lookahead) {
            Some(token) => Some(token.kind.clone()),
            None => None,
        }
    }

    pub fn advance(&mut self) -> Option<TokenKind> {
        let curr = self.current();
        self.current += 1;
        self.lookahead += 1;
        curr
    }

    pub fn eat(&mut self, token: TokenKind) -> Result<(), String> {
        if self.current() == Some(token.clone()) {
            self.advance();
            Ok(())
        } else {
            Err(format!(
                "Expected {:?} but got {:?} {}",
                token,
                self.current(),
                self.debug_position()
            ))
        }
    }

    pub fn expect(&self, expected: &TokenKind, file: &str, line: u32) -> Result<(), String> {
        if let Some(token) = self.current() {
            if token == *expected {
                Ok(())
            } else {
                Err(format!(
                    "(expect guard) Expected {:?}, got {:?} at {} (compiler {}:{}){}",
                    expected,
                    token,
                    self.debug_position(),
                    file,
                    line,
                    self.source
                        .lines()
                        .nth((self.current_position().unwrap().line as usize) - 1)
                        .unwrap()
                ))
            }
        } else {
            Err(format!(
                "(expect guard) Expected {:?}, got EOF at {} (compiler {}:{})",
                expected,
                self.debug_position(),
                file,
                line
            ))
        }
    }

    pub(crate) fn use_decl(&mut self, current_mod: Rc<RefCell<Module>>) -> Result<Use, String> {
        let span = self.span_start()?;
        self.eat(TokenKind::Keyword(Keyword::Use))?;
        let path = self.path()?;
        let path = self.canonicalize_path(path, current_mod)?;
        self.eat(TokenKind::Symbol(Symbol::Semicolon))?;
        Ok(Use {
            local: false,
            item_path: path,
            span: self.span_end(span)?,
        })
    }

    // Turn relative module path into absolute
    // For example,     main::foo::bar -> main::foo::bar
    // In mod foo::baz: super::bar -> main::foo::bar
    // In mod foo::baz  self::bar -> main::foo::baz::bar
    // "main" is a keyword for the root module
    fn canonicalize_path(
        &self,
        path: ModulePath,
        current_mod: Rc<RefCell<Module>>,
    ) -> Result<ModulePath, String> {
        let mut path = path;
        let mut current_path = current_mod.borrow().path.clone();
        let mut i = 0;
        while i < path.len() {
            match path[i].as_str() {
                "super" => {
                    if current_path.len() == 0 {
                        return Err(format!(
                            "Cannot use super in the root module {}",
                            self.debug_position()
                        ));
                    }
                    current_path.pop();
                    path.remove(i);
                }
                "self" => {
                    path.remove(i);
                }
                "main" => {
                    if i != 0 {
                        return Err(format!(
                            "Parser: main can only be used as the first identifier in a path {}",
                            self.debug_position()
                        ));
                    }
                    i += 1;
                }
                _ => {
                    i += 1;
                }
            }
        }
        current_path.append(&mut path);
        Ok(current_path)
    }

    pub fn path(&mut self) -> Result<ModulePath, String> {
        let mut path = Vec::new();
        loop {
            if let Some(TokenKind::Identifier(ident)) = self.current() {
                path.push(ident.clone());
                self.advance();
            } else {
                return Err(format!(
                    "Path expected identifier, got {:?} {}",
                    self.current(),
                    self.debug_position()
                ));
            }
            if let Some(TokenKind::Symbol(Symbol::DoubleColon)) = self.current() {
                self.advance();
            } else {
                break;
            }
        }
        Ok(path)
    }

    pub fn module(
        &mut self,
        mod_name: String,
        parent: Option<Rc<RefCell<Module>>>,
    ) -> Result<Rc<RefCell<Module>>, String> {
        debug!("parser::Parser::module");

        let module = Rc::new(RefCell::new(Module {
            fn_defs: Vec::new(),
            fn_decls: Vec::new(),
            structs: Vec::new(),
            submodules: Vec::new(),
            dependencies: Vec::new(),
            name: mod_name.clone(),
            parent: parent.clone(),
            path: {
                let mut path = Vec::from([mod_name]);
                let mut parent = parent;
                while let Some(parent_mod) = parent {
                    path.push(parent_mod.borrow().name.clone());
                    parent = parent_mod.borrow().parent.clone();
                }
                path.reverse();
                path
            },
        }));
        self.current_module = Some(module.clone());

        loop {
            let mod_ptr = module.clone();
            match self.current() {
                Some(TokenKind::Attributes(attrs)) => {
                    self.advance();
                    self.unclaimed_attrs = Some(attrs);
                }
                Some(TokenKind::Keyword(Keyword::Function)) => {
                    let fn_sig = self.function_sig()?;
                    if let Some(TokenKind::Symbol(Symbol::Semicolon)) = self.current() {
                        let func_decl: FunctionDeclaration = self.function_decl(fn_sig)?;
                        mod_ptr.borrow_mut().fn_decls.push(func_decl);
                    } else {
                        let func: FunctionDefinition = self.function_def(fn_sig)?;
                        mod_ptr.borrow_mut().fn_defs.push(func);
                    }
                }
                Some(TokenKind::Keyword(Keyword::Use)) => {
                    let use_decl = self.use_decl(module.clone())?;
                    mod_ptr.borrow_mut().dependencies.push(use_decl);
                }
                Some(TokenKind::Keyword(Keyword::Struct)) => {
                    let structure = self.struct_decl()?;
                    mod_ptr.borrow_mut().structs.push(structure);
                }
                Some(TokenKind::Keyword(Keyword::Module)) => {
                    let module = self.module_decl(module.clone())?;
                    mod_ptr.borrow_mut().submodules.push(module);
                }
                Some(TokenKind::Keyword(Keyword::Impl)) => {
                    self.eat(TokenKind::Keyword(Keyword::Impl))?;
                    let type_name = if let Some(TokenKind::Identifier(name)) = self.advance() {
                        name
                    } else {
                        return Err(format!(
                            "Impl name expected identifier, got {:?} at {}",
                            self.current(),
                            self.debug_position()
                        ));
                    };

                    let mut mod_ptr = mod_ptr.borrow_mut();
                    let Some(struct_decl) = mod_ptr.structs.iter_mut().find(|decl| {
                        decl.name == type_name
                    }) else {
                        return Err(format!(
                            "No struct with name {} found at {}",
                            type_name,
                            self.debug_position()
                        ));
                    };

                    self.eat(TokenKind::Symbol(Symbol::OpenBrace))?;

                    self.impl_ctx = Some(struct_decl.name.clone());

                    loop {
                        match self.current() {
                            Some(TokenKind::Keyword(Keyword::Function)) => {
                                let fn_sig = self.function_sig()?;
                                if let Some(TokenKind::Symbol(Symbol::Semicolon)) = self.current() {
                                    let _func_decl: FunctionDeclaration =
                                        self.function_decl(fn_sig)?;
                                    unreachable!("No extern functions in impls");
                                    // body.push(Declaration::FunctionDecl(func_decl));
                                } else {
                                    let func = self.function_def(fn_sig)?;
                                    struct_decl.methods.push(func);
                                    // body.push(Declaration::FunctionDef(func));
                                }
                            }
                            Some(TokenKind::Symbol(Symbol::CloseBrace)) => {
                                break;
                            }
                            Some(_) => {
                                return Err(format!(
                                    "Unexpected token {:?} at {}",
                                    self.current(),
                                    self.debug_position()
                                ))
                            }
                            None => break,
                        }
                    }

                    self.eat(TokenKind::Symbol(Symbol::CloseBrace))?;
                }
                Some(TokenKind::Symbol(Symbol::CloseBrace)) => {
                    break;
                }
                Some(_) => {
                    return Err(format!(
                        "Unexpected token {:?} at {}",
                        self.current(),
                        self.debug_position()
                    ))
                }
                None => break,
            }
        }

        Ok(module)
    }

    pub fn module_decl(
        &mut self,
        parent: Rc<RefCell<Module>>,
    ) -> Result<Rc<RefCell<Module>>, String> {
        debug!("parser::Parser::module_decl");
        self.eat(TokenKind::Keyword(Keyword::Module))?;
        let name = if let Some(TokenKind::Identifier(name)) = self.advance() {
            name.clone()
        } else {
            return Err(format!(
                "mod name expected identifier, got {:?} at {}",
                self.current(),
                self.debug_position()
            ));
        };

        self.eat(TokenKind::Symbol(Symbol::OpenBrace))?;
        let module = self.module(name, Some(parent))?;
        self.advance();
        Ok(module)
    }

    pub fn type_decl_name(&mut self) -> Result<String, String> {
        let type_name = if let Some(TokenKind::Identifier(type_name)) = self.current() {
            type_name
        } else {
            return Err(format!(
                "Type name expected identifier, got {:?} at {}",
                self.current(),
                self.debug_position()
            ));
        };
        self.advance();
        Ok(type_name)
    }

    pub fn type_name(&mut self) -> Result<String, String> {
        debug!("parser::Parser::type_name");
        let mut type_name = String::new();
        let span = self.span_start()?;
        while let Some(TokenKind::Identifier(name)) = self.current() {
            type_name += &name;
            self.advance();
            if let Some(TokenKind::Symbol(Symbol::DoubleColon)) = self.current() {
                type_name += "::";
                self.advance();
            } else {
                break;
            }
        }

        while let Some(TokenKind::Operator(Operator::Times)) = self.current() {
            self.advance();
            type_name = format!("*{}", type_name);
        }

        let path = type_name
            .split("::")
            .map(|s| s.to_string())
            .collect::<Vec<String>>();

        //let path = self.canonicalize_path(path, self.current_module.clone().unwrap())?;

        let name = if let Some(curr_mod) = &self.current_module {
            if path.len() > 1 {
                curr_mod.borrow_mut().dependencies.push(Use {
                    item_path: path.clone(),
                    local: true,
                    span: self.span_end(span)?,
                })
            }

            path.last().unwrap().clone()
        } else {
            path.last().unwrap().clone()
        };

        Ok(name)
    }

    pub fn fn_params(&mut self) -> Result<(Vec<FunctionParameter>, bool), String> {
        debug!("parser::Parser::fn_params");
        let mut params = Vec::new();
        let mut param_idx: u32 = 0;
        let mut variadic = false;

        loop {
            if let Some(TokenKind::Symbol(Symbol::CloseParen)) = self.current() {
                self.advance();
                break;
            }

            let span = self.span_start()?;

            if let Some(TokenKind::Symbol(Symbol::Ellipsis)) = self.current() {
                self.advance();
                variadic = true;
                continue;
            }

            if let Some(TokenKind::Identifier(maybe_self)) = self.current() {
                if maybe_self.as_str() == "self" {
                    if param_idx != 0 {
                        return Err(format!(
                            "Self must be the first parameter at {}",
                            self.debug_position()
                        ));
                    }
                    let Some(struct_decl) = &self.impl_ctx else {
                        return Err(format!(
                            "Self can only be used in impls at {}",
                            self.debug_position()
                        ));
                    };
                    params.push(FunctionParameter {
                        name: "self".to_string(),
                        type_name: "*".to_string() + struct_decl,
                        idx: param_idx,
                        span: self.span_end(span)?,
                    });
                    param_idx += 1;
                    self.advance();

                    if let Some(TokenKind::Symbol(Symbol::Comma)) = self.current() {
                        self.advance();
                    } else if let Some(TokenKind::Symbol(Symbol::CloseParen)) = self.current() {
                    } else {
                        return Err(format!(
                            "Expected comma or close paren, got {:?} at {}",
                            self.current(),
                            self.debug_position()
                        ));
                    }
                    continue;
                }
            }

            let param_type = self.type_name()?;

            let param_name = if let Some(TokenKind::Identifier(param_name)) = self.current() {
                param_name.clone()
            } else {
                return Err(format!(
                    "Invalid param: expected identifier, got {:?} at {}",
                    self.current(),
                    self.debug_position()
                ));
            };

            params.push(FunctionParameter {
                name: param_name,
                type_name: param_type,
                idx: param_idx,
                span: self.span_end(span)?,
            });
            param_idx += 1;

            self.advance();

            if let Some(TokenKind::Symbol(Symbol::Comma)) = self.current() {
                self.advance();
            } else if let Some(TokenKind::Symbol(Symbol::CloseParen)) = self.current() {
            } else {
                return Err(format!(
                    "Expected comma or close paren, got {:?} at {}",
                    self.current(),
                    self.debug_position()
                ));
            }
        }
        Ok((params, variadic))
    }

    pub fn function_sig(
        &mut self,
    ) -> Result<
        (
            String,
            Vec<FunctionParameter>,
            Option<String>,
            bool,
            NodeSpan,
        ),
        String,
    > {
        debug!("parser::Parser::function_sig");
        let span = self.span_start()?;
        self.advance(); // Skip function keyword
        let name = if let Some(TokenKind::Identifier(name)) = self.current() {
            name.clone()
        } else {
            return Err(format!(
                "Function sig expected identifier, got {:?} at {}",
                self.current(),
                self.debug_position()
            ));
        };

        self.advance();
        self.expect(&TokenKind::Symbol(Symbol::OpenParen), file!(), line!())?;
        self.advance();

        let (params, variadic): (Vec<FunctionParameter>, bool) = self.fn_params()?;

        let return_type = if let Some(TokenKind::Symbol(Symbol::RightArrow)) = self.current() {
            self.advance();
            Some(self.type_name()?)
        } else {
            None
        };

        Ok((name, params, return_type, variadic, self.span_end(span)?))
    }

    pub fn function_def(
        &mut self,
        fn_sig: (
            String,
            Vec<FunctionParameter>,
            Option<String>,
            bool,
            NodeSpan,
        ),
    ) -> Result<crate::ast::FunctionDefinition, String> {
        debug!("parser::Parser::function_def");
        let (name, params, return_type, variadic, _) = fn_sig;
        let span = self.span_start()?;
        let body = self.block()?;

        Ok(FunctionDefinition {
            name,
            return_type,
            params,
            body,
            variadic,
            span: self.span_end(span)?,
            attrs: {
                if self.unclaimed_attrs.is_some() {
                    /* let attrs = self.unclaimed_attrs.clone().unwrap();
                    self.unclaimed_attrs = None; */
                    let attrs = std::mem::replace(&mut self.unclaimed_attrs, None);
                    if let Some(attrs) = attrs {
                        attrs
                    } else {
                        Vec::new()
                    }
                } else {
                    Vec::new()
                }
            },
        })
    }

    pub fn struct_decl(&mut self) -> Result<StructDeclaration, String> {
        debug!("parser::Parser::struct_decl");
        let outer_span = self.span_start()?;
        self.eat(TokenKind::Keyword(Keyword::Struct))?;
        /* let name = if let Some(TokenKind::Identifier(name)) = self.advance() {
            name.clone()
        } else {
            return Err(format!(
                "Struct decl expected identifier, got {:?} at {}",
                self.current(),
                self.debug_position()
            ));
        }; */
        let name = self.type_decl_name()?;
        let mut fields = Vec::new();
        self.eat(TokenKind::Symbol(Symbol::OpenBrace))?;
        let mut field_idx = 0;
        loop {
            if let Some(TokenKind::Symbol(Symbol::CloseBrace)) = self.current() {
                self.advance();
                break;
            }

            let span = self.span_start()?;

            let type_name = self.type_name()?;

            let Some(TokenKind::Identifier(field_name)) = self.advance() else {
                return Err(format!(
                    "Struct field expected identifier, got {:?} at {}",
                    self.current(),
                    self.debug_position()
                ));
            };

            fields.push(StructField {
                name: field_name,
                type_name,
                idx: field_idx,
                span: self.span_end(span)?,
            });
            field_idx += 1;

            match self.current() {
                Some(TokenKind::Symbol(Symbol::Comma)) => {
                    self.advance();
                }
                Some(TokenKind::Symbol(Symbol::CloseBrace)) => {}
                _ => {
                    return Err(format!(
                        "Expected comma or close brace, got {:?} at {}",
                        self.current(),
                        self.debug_position()
                    ))
                }
            }
        }

        Ok(StructDeclaration {
            name,
            fields,
            methods: Vec::new(),
            span: self.span_end(outer_span)?,
        })
    }

    pub fn block(&mut self) -> Result<crate::ast::Block, String> {
        debug!("parser::Parser::block");
        let span = self.span_start()?;
        self.expect(&TokenKind::Symbol(Symbol::OpenBrace), file!(), line!())?;
        self.advance();

        let mut statements = Vec::new();
        loop {
            if let Some(TokenKind::Symbol(Symbol::CloseBrace)) = self.current() {
                self.advance();
                break;
            }

            let statement = self.statement()?;
            statements.push(statement);
        }

        Ok(Block {
            statements,
            span: self.span_end(span)?,
        })
    }

    pub fn assignment(&mut self) -> Result<Expression, String> {
        debug!("parser::Parser::assignment");
        let left = self.logical_or()?;

        if let Some(TokenKind::Operator(Operator::Assign(assign_op))) = self.current() {
            self.advance();
            let right = self.assignment()?;
            return Ok(Expression::VarAssignment {
                var_assign: VarAssignment {
                    operator: assign_op,
                    left: Box::from(self.validate_assign_target(left)?),
                    right: Box::from(right),
                    span: self.span_start()?,
                },
            });
        } else {
            return Ok(left);
        }

        /* if let Expression::BinaryOp {
            left: binop_left,
            right: binop_right,
            op,
            span,
        } = left
        {
            if let Operator::Assign(op) = op {
                Ok(Expression::VarAssignment {
                    var_assign: VarAssignment {
                        operator: op,
                        left: Box::from(self.validate_assign_target(*binop_left)?),
                        right: binop_right,
                        span,
                    },
                })
            } else {
                Ok(Expression::BinaryOp {
                    left: binop_left,
                    right: binop_right,
                    op,
                    span,
                })
            }
        } else {
            Ok(left)
        } */
    }

    fn validate_assign_target(&self, target: Expression) -> Result<Expression, String> {
        debug!("parser::Parser::validate_assign_target");
        match &target {
            Expression::Identifier { .. } => Ok(target),
            Expression::MemberAccess { .. } => Ok(target),
            Expression::UnaryOp { op, .. } => {
                if let Operator::Times = op {
                    Ok(target)
                } else {
                    Err(format!(
                        "Invalid assignment target: {:?} at {}",
                        target,
                        self.debug_position()
                    ))
                }
            }
            //Expression::IndexAccess(_) => Ok(()),
            _ => Err(format!(
                "Invalid assignment target: {:?} at {}",
                target,
                self.debug_position()
            )),
        }
    }

    pub fn if_expr(&mut self) -> Result<Expression, String> {
        debug!("parser::Parser::if_expr");
        let span = self.span_start()?;
        self.eat(TokenKind::Keyword(Keyword::If))?;
        let condition = self.expression()?;
        let body = self.expression()?;
        let else_body = if let Some(TokenKind::Keyword(Keyword::Else)) = self.advance() {
            Some(self.expression()?)
        } else {
            None
        };
        Ok(Expression::If {
            expr: IfExpression {
                condition: Box::from(condition),
                body: Box::from(body),
                else_body: Box::from(else_body),
                span: self.span_end(span)?,
            },
        })
    }

    pub fn struct_init(&mut self) -> Result<Expression, String> {
        debug!("parser::Parser::struct_init");
        let outer_span = self.span_start()?;
        /* let name = if let Some(TokenKind::Identifier(name)) = self.advance() {
            name
        } else {
            return Err(format!(
                "Failed to parse struct init: Expected identifier, got {:?} at {}",
                self.current(),
                self.debug_position()
            ));
        }; */
        let name = self.type_name()?;

        self.eat(TokenKind::Symbol(Symbol::OpenBrace))?;
        let mut fields = Vec::new();
        let mut field_idx = 0;
        loop {
            if let Some(TokenKind::Symbol(Symbol::CloseBrace)) = self.current() {
                self.advance();
                break;
            }

            let field_span = self.span_start()?;

            let Some(TokenKind::Identifier(field_name)) = self.advance() else {
                return Err(format!(
                    "Failed to parse field name: Expected identifier, got {:?} at {}",
                    self.current(),
                    self.debug_position()
                ));
            };

            if let Some(TokenKind::Symbol(Symbol::Colon)) = self.current() {
                self.advance();
            } else if let Some(TokenKind::Symbol(Symbol::Comma)) = self.current() {
                self.advance();
                let value = Expression::Identifier {
                    name: field_name.clone(),
                    span: self.span_end(field_span.clone())?,
                };
                fields.push(StructInitializerField {
                    field_name: field_name.clone(),
                    value,
                    idx: field_idx,
                    span: self.span_end(field_span)?,
                });
                field_idx += 1;
                continue;
            } else if let Some(TokenKind::Symbol(Symbol::CloseBrace)) = self.current() {
                let value = Expression::Identifier {
                    name: field_name.clone(),
                    span: self.span_end(field_span.clone())?,
                };
                fields.push(StructInitializerField {
                    field_name: field_name.clone(),
                    value,
                    idx: field_idx,
                    span: self.span_end(field_span)?,
                });
                break;
            } else {
                return Err(format!(
                    "Expected colon, comma or close brace, got {:?} at {}",
                    self.current(),
                    self.debug_position()
                ));
            }

            let value = self.expression()?;
            fields.push(StructInitializerField {
                field_name,
                value,
                idx: field_idx,
                span: self.span_end(field_span)?,
            });
            field_idx += 1;

            if let Some(TokenKind::Symbol(Symbol::Comma)) = self.current() {
                self.advance();
            } else {
                self.expect(&TokenKind::Symbol(Symbol::CloseBrace), file!(), line!())?;
            }
        }
        Ok(Expression::StructInitializer {
            struct_init: StructInitializer {
                struct_name: name,
                fields,
                span: self.span_end(outer_span)?,
            },
        })
    }

    pub fn expression(&mut self) -> Result<Expression, String> {
        debug!("parser::Parser::expression");
        let curr = self.current();
        // Check for If, Block, and Fn Calls
        debug!(format!("{:?}", curr));
        if let Some(TokenKind::Keyword(Keyword::If)) = curr {
            return self.if_expr();
        } else if let Some(TokenKind::Keyword(Keyword::SizeOf)) = curr {
            return self.sizeof_expr();
        } else if let Some(TokenKind::Symbol(Symbol::OpenBrace)) = curr {
            return Ok(Expression::Block {
                block: self.block()?,
            });
        }
        /* else if let Some(TokenKind::Identifier(_)) = curr {
            if let Some(TokenKind::Symbol(Symbol::OpenBrace)) = self.lookahead() {
                return Ok(self.struct_init()?);
            }
        } */
        /* else if let Some(TokenKind::Identifier(_)) = curr {
            if let Some(TokenKind::Symbol(Symbol::OpenParen)) = self.lookahead() {
                return self.call_expr();
            }
        } */
        self.assignment()
    }

    pub fn var_declaration(&mut self) -> Result<Statement, String> {
        debug!("parser::Parser::var_declaration");
        let span = self.span_start()?;
        self.advance();
        let type_name = self.type_name()?;
        let name = if let Some(TokenKind::Identifier(name)) = self.current() {
            name.clone()
        } else {
            return Err(format!(
                "Failed to parse var declaration: Expected identifier, got {:?} at {}",
                self.current(),
                self.debug_position()
            ));
        };
        self.advance();

        let initializer =
            if let Some(TokenKind::Operator(Operator::Assign(AssignmentOperator::Assign))) =
                self.current()
            {
                self.advance();
                let initializer = self.expression()?;
                Some(initializer)
            } else {
                None
            };
        Ok(Statement::Variable(VarDeclaration {
            name,
            type_name,
            initializer,
            span: self.span_end(span)?,
        }))
    }

    pub fn statement(&mut self) -> Result<crate::ast::Statement, String> {
        debug!("parser::Parser::statement");
        let span = self.span_start()?;
        let statement = match self.current() {
            Some(TokenKind::Keyword(Keyword::Return)) => {
                self.advance();
                let value = if let Some(TokenKind::Symbol(Symbol::Semicolon)) = self.current() {
                    None
                } else {
                    let value = self.expression()?;
                    Some(value)
                };
                Ok(Statement::Return(ReturnStatement { value, span }))
            }
            Some(TokenKind::Symbol(Symbol::LeftArrow)) => {
                self.advance();
                let expression = self.expression()?;
                Ok(Statement::Result(ResultStatement {
                    value: expression,
                    span: self.span_end(span)?,
                }))
            }
            Some(TokenKind::Keyword(Keyword::Break)) => {
                self.advance();
                Ok(Statement::Break)
            }
            Some(TokenKind::Keyword(Keyword::Continue)) => {
                self.advance();
                Ok(Statement::Continue)
            }
            Some(TokenKind::Keyword(Keyword::Loop)) => {
                self.advance();
                let body = self.block()?;
                Ok(Statement::Loop(LoopStatement { body, span }))
            }
            Some(TokenKind::Keyword(Keyword::Let)) => self.var_declaration(),
            Some(_) => {
                let expression = self.expression()?;
                Ok(Statement::Expression(expression))
            }
            None => Err(format!(
                "Expected statement, got {:?} at {}",
                self.current(),
                self.debug_position()
            )),
        }?;
        self.expect(&TokenKind::Symbol(Symbol::Semicolon), file!(), line!())?;
        self.advance();
        Ok(statement)
    }

    fn logical_expr_helper(
        &mut self,
        builder: fn(&mut Parser<'parser>) -> Result<Expression, String>,
        op_type: OperatorType,
    ) -> Result<Expression, String> {
        debug!("parser::Parser::logical_expr_helper");
        let mut left = builder(self)?;

        while let Some(TokenKind::Operator(op)) = self.current() {
            if op.op_type() != op_type {
                break;
            }
            let span = self.span_start()?;
            self.advance();
            let right = builder(self)?;
            left = Expression::LogicalOp {
                left: Box::from(left),
                right: Box::from(right),
                op,
                span: self.span_end(span)?,
            };
        }

        Ok(left)
    }

    fn binary_expr_helper(
        &mut self,
        builder: fn(&mut Parser<'parser>) -> Result<Expression, String>,
        op_type: OperatorType,
    ) -> Result<Expression, String> {
        debug!("parser::Parser::binary_expr_helper");
        let mut left = builder(self)?;

        while let Some(TokenKind::Operator(op)) = self.current() {
            if op.op_type() != op_type {
                break;
            }
            let span = self.span_start()?;
            self.advance();
            let right = builder(self)?;
            left = Expression::BinaryOp {
                left: Box::from(left),
                right: Box::from(right),
                op,
                span: self.span_end(span)?,
            };
        }

        Ok(left)
    }

    pub fn logical_or(&mut self) -> Result<Expression, String> {
        debug!("parser::Parser::logical_or");
        self.logical_expr_helper(Self::logical_and, OperatorType::LogicalOr)
    }

    pub fn logical_and(&mut self) -> Result<Expression, String> {
        debug!("parser::Parser::logical_and");
        self.logical_expr_helper(Self::equality, OperatorType::LogicalAnd)
    }

    pub fn equality(&mut self) -> Result<Expression, String> {
        debug!("parser::Parser::equality");
        self.binary_expr_helper(Self::relational, OperatorType::Equality)
    }

    pub fn relational(&mut self) -> Result<Expression, String> {
        debug!("parser::Parser::relational");
        self.binary_expr_helper(Self::additive, OperatorType::Relational)
    }

    pub fn additive(&mut self) -> Result<Expression, String> {
        debug!("parser::Parser::additive");
        self.binary_expr_helper(Self::multiplicative, OperatorType::Additive)
    }

    pub fn multiplicative(&mut self) -> Result<Expression, String> {
        debug!("parser::Parser::multiplicative");
        self.binary_expr_helper(Self::as_expr, OperatorType::Multiplicative)
    }

    pub fn as_expr(&mut self) -> Result<Expression, String> {
        debug!("parser::Parser::as_expr");
        let mut expr = self.unary()?;

        if let Some(TokenKind::Operator(Operator::As)) = self.current() {
            let span = self.span_start()?;
            self.advance();
            let type_name = self.type_name()?;
            expr = Expression::AsExpr {
                expr: Box::from(expr),
                type_name,
                span: self.span_end(span)?,
            };
        }
        Ok(expr)
    }

    pub fn unary(&mut self) -> Result<Expression, String> {
        debug!("parser::Parser::unary");
        let span = self.span_start()?;
        let curr = self.current();
        match curr {
            Some(TokenKind::Operator(op)) => {
                self.advance();
                if op.is_unary() {
                    Ok(Expression::UnaryOp {
                        op,
                        expr: Box::from(self.unary()?),
                        span: self.span_end(span)?,
                    })
                } else {
                    Err(format!(
                        "Expected unary operator, got {:?} at {}",
                        op,
                        self.debug_position()
                    ))
                }
            }
            /* Some(_s) => {
                debug!(format!("{:?}", _s));
                //panic!();
                self.left_hand_side_expr()
            } */
            Some(_) => self.left_hand_side_expr(),
            None => Err(format!(
                "Expected unary operator, got {:?} at {}",
                curr,
                self.debug_position()
            )),
        }
    }

    pub fn left_hand_side_expr(&mut self) -> Result<Expression, String> {
        debug!("parser::Parser::left_hand_side_expr");
        self.call_member_expr()
    }

    pub fn call_member_expr(&mut self) -> Result<Expression, String> {
        debug!("parser::Parser::call_member_expr");
        let member = self.member_expr()?;
        debug!(format!("{:?}", self.current()));
        if let Some(TokenKind::Symbol(Symbol::OpenParen)) = self.current() {
            self.call_expr(member)
        } else {
            Ok(member)
        }
    }

    fn call_expr(&mut self, callee: Expression) -> Result<Expression, String> {
        debug!("parser::Parser::call_expr");
        let span = self.span_start()?;
        Ok(Expression::FnCall {
            fn_call: FunctionCall {
                callee: Box::from(callee),
                args: self.arguments()?,
                span: self.span_end(span)?,
            },
        })
    }

    pub fn arguments(&mut self) -> Result<Vec<Expression>, String> {
        debug!("parser::Parser::arguments");
        let mut args = Vec::new();
        self.expect(&TokenKind::Symbol(Symbol::OpenParen), file!(), line!())?;
        self.advance();
        loop {
            match self.current() {
                Some(TokenKind::Symbol(Symbol::CloseParen)) => {
                    self.advance();
                    break;
                }
                Some(_) => {
                    let arg = self.expression()?;
                    args.push(arg);
                    if let Some(TokenKind::Symbol(Symbol::CloseParen)) = self.current() {
                        self.advance();
                        break;
                    }
                    self.expect(&TokenKind::Symbol(Symbol::Comma), file!(), line!())?;
                    self.advance();
                }
                None => {
                    return Err(format!(
                        "Expected argument, got {:?} at {}",
                        self.current(),
                        self.debug_position()
                    ))
                }
            };
        }
        Ok(args)
    }

    pub fn member_expr(&mut self) -> Result<Expression, String> {
        debug!("parser::Parser::member_expr");
        let mut object: Expression = self.primary_expr()?;

        while let Some(TokenKind::Symbol(Symbol::Dot))
        | Some(TokenKind::Symbol(Symbol::OpenBracket))
        | Some(TokenKind::Symbol(Symbol::DoubleColon)) = self.current()
        {
            let span = self.span_start()?;
            if let Some(TokenKind::Symbol(Symbol::Dot)) = self.current() {
                self.advance();
                let prop = self.identifier()?;
                object = Expression::MemberAccess {
                    member_access: MemberAccess {
                        object: Box::from(object),
                        member: Box::from(prop),
                        computed: false,
                        span: self.span_end(span)?,
                    },
                };
            } else if let Some(TokenKind::Symbol(Symbol::DoubleColon)) = self.current() {
                self.eat(TokenKind::Symbol(Symbol::DoubleColon))?;
                let prop = self.identifier()?;
                object = Expression::ScopeResolution {
                    scope_resolution: ScopeResolution {
                        object: Box::from(object),
                        member: Box::from(prop),
                        span: self.span_end(span)?,
                    },
                };
            } else {
                self.advance();
                let prop = self.expression()?;
                self.expect(&TokenKind::Symbol(Symbol::CloseBracket), file!(), line!())?;
                self.advance();
                object = Expression::MemberAccess {
                    member_access: MemberAccess {
                        object: Box::from(object),
                        member: Box::from(prop),
                        computed: true,
                        span: self.span_end(span)?,
                    },
                };
            }
        }

        Ok(object)
    }

    pub fn identifier(&mut self) -> Result<Expression, String> {
        debug!("parser::Parser::identifier");
        let span = self.span_start()?;
        match self.current() {
            Some(TokenKind::Identifier(id)) => {
                self.advance();
                Ok(Expression::Identifier {
                    name: id,
                    span: self.span_end(span)?,
                })
            }
            Some(_) => Err("Expected identifier".to_string()),
            None => Err(format!(
                "Failed to parse identifier: Expected identifier, got {:?} at {}",
                self.current(),
                self.debug_position()
            )),
        }
    }

    pub fn primary_expr(&mut self) -> Result<Expression, String> {
        debug!("parser::Parser::primary_expr");
        match self.current() {
            Some(TokenKind::Literal(_)) => Ok(self.literal()?),
            Some(TokenKind::Symbol(Symbol::OpenParen)) => Ok(self.paren_expr()?),
            Some(TokenKind::Identifier(_)) => {
                if let Some(TokenKind::Symbol(Symbol::OpenBrace)) = self.lookahead() {
                    Ok(self.struct_init()?)
                } else {
                    Ok(self.identifier()?)
                }
                //Ok(self.identifier()?)
            }
            Some(_) => Ok(self.left_hand_side_expr()?),
            None => Err(format!(
                "Expected primary expression, got {:?} at {}",
                self.current(),
                self.debug_position()
            )),
        }
    }

    pub fn literal(&mut self) -> Result<Expression, String> {
        debug!("parser::Parser::literal");
        let Some(TokenKind::Literal(literal)) = self.current() else {
            return Err("Expected literal".to_string());
        };

        self.advance();
        Ok(Expression::Literal {
            literal,
            //span: self.span()?,
        })
    }

    fn paren_expr(&mut self) -> Result<Expression, String> {
        debug!("parser::Parser::paren_expr");
        self.expect(&TokenKind::Symbol(Symbol::OpenParen), file!(), line!())?;
        self.advance();
        let expr = self.expression()?;
        self.expect(&TokenKind::Symbol(Symbol::CloseParen), file!(), line!())?;
        self.advance();
        Ok(expr)
    }

    fn function_decl(
        &mut self,
        fn_sig: (
            String,
            Vec<FunctionParameter>,
            Option<String>,
            bool,
            NodeSpan,
        ),
    ) -> Result<FunctionDeclaration, String> {
        debug!("parser::Parser::function_decl");
        self.eat(TokenKind::Symbol(Symbol::Semicolon))?;
        Ok(FunctionDeclaration {
            name: fn_sig.0,
            params: fn_sig.1,
            return_type: fn_sig.2,
            variadic: fn_sig.3,
            attrs: {
                if self.unclaimed_attrs.is_some() {
                    /* let attrs = self.unclaimed_attrs.clone().unwrap();
                    self.unclaimed_attrs = None; */
                    let attrs = std::mem::replace(&mut self.unclaimed_attrs, None);
                    if let Some(attrs) = attrs {
                        attrs
                    } else {
                        Vec::new()
                    }
                } else {
                    Vec::new()
                }
            },
        })
    }

    fn sizeof_expr(&mut self) -> Result<Expression, String> {
        debug!("parser::Parser::sizeof_expr");
        let span = self.span_start()?;
        self.eat(TokenKind::Keyword(Keyword::SizeOf))?;
        self.eat(TokenKind::Symbol(Symbol::OpenParen))?;
        let expr = self.identifier()?;
        self.eat(TokenKind::Symbol(Symbol::CloseParen))?;
        Ok(Expression::SizeOfExpr {
            expr: Box::from(expr),
            span: self.span_end(span)?,
        })
    }
}
