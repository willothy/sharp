use crate::{
    ast::{
        Block, Declaration, Expression, FunctionCall, FunctionDeclaration, FunctionParameter,
        IfExpression, LoopStatement, MemberAccess, Module, ReturnStatement, Statement,
        StructDeclaration, StructField, StructInitializer, StructInitializerField, VarAssignment,
        VarDeclaration, YieldStatement,
    },
    tokenizer::{
        AssignmentOperator, Keyword, Operator, OperatorType, Symbol, Token, TokenKind,
        TokenPosition,
    },
};

pub fn parse<'parse>(tokens: Vec<Token>, mod_name: String) -> Result<Module, String> {
    let mut parser = Parser::new(tokens);
    let module = parser.module(mod_name)?;
    Ok(module)
}

struct Parser<'parser> {
    tokens: Vec<Token>,
    current: usize,
    lookahead: usize,
    lifetime: std::marker::PhantomData<&'parser ()>,
}

impl<'parser> Parser<'parser> {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current: 0,
            lookahead: 1,
            lifetime: std::marker::PhantomData,
        }
    }

    pub fn check_n(&self, n: usize, token: &Token) -> bool {
        self.tokens.get(self.current + n) == Some(token)
    }

    pub fn current(&self) -> Option<TokenKind> {
        match self.tokens.get(self.current) {
            Some(token) => Some(token.kind.clone()),
            None => None,
        }
    }

    pub fn current_position(&self) -> Option<TokenPosition> {
        match self.tokens.get(self.current) {
            Some(token) => Some(token.position.clone()),
            None => None,
        }
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

    pub fn expect(&self, expected: &TokenKind) -> Result<(), String> {
        if let Some(token) = self.current() {
            if token == *expected {
                Ok(())
            } else {
                Err(format!(
                    "(expect guard) Expected {:?}, got {:?} at {}",
                    expected,
                    token,
                    self.debug_position()
                ))
            }
        } else {
            Err(format!(
                "(expect guard) Expected {:?}, got EOF at {}",
                expected,
                self.debug_position()
            ))
        }
    }

    pub fn module(&mut self, mod_name: String) -> Result<Module, String> {
        let mut body = Vec::new();
        let mut submodules = Vec::new();

        loop {
            match self.current() {
                Some(TokenKind::Keyword(Keyword::Function)) => {
                    let function = self.function_decl()?;
                    body.push(Declaration::Function(function));
                }
                Some(TokenKind::Keyword(Keyword::Struct)) => {
                    let structure = self.struct_decl()?;
                    body.push(Declaration::Struct(structure));
                }
                Some(TokenKind::Keyword(Keyword::Module)) => {
                    let module = self.module_decl()?;
                    submodules.push(module);
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

        Ok(Module {
            body,
            submodules,
            requirements: Vec::new(),
            name: mod_name,
        })
    }

    pub fn module_decl(&mut self) -> Result<Module, String> {
        self.eat(TokenKind::Keyword(Keyword::Module))?;
        let name = if let Some(TokenKind::Identifier(name)) = self.advance() {
            name.clone()
        } else {
            return Err(format!(
                "Expected identifier, got {:?} at {}",
                self.current(),
                self.debug_position()
            ));
        };

        self.eat(TokenKind::Symbol(Symbol::OpenBrace))?;
        let module = self.module(name)?;
        self.advance();
        Ok(module)
    }

    pub fn fn_params(&mut self) -> Result<Vec<FunctionParameter>, String> {
        let mut params = Vec::new();
        loop {
            if let Some(TokenKind::Symbol(Symbol::CloseParen)) = self.current() {
                self.advance();
                break;
            }

            let param_type = if let Some(TokenKind::Identifier(param_type)) = self.current() {
                param_type.clone()
            } else {
                return Err(format!(
                    "Expected type name, got {:?} at {}",
                    self.current(),
                    self.debug_position()
                ));
            };

            self.advance();
            let param_name = if let Some(TokenKind::Identifier(param_name)) = self.current() {
                param_name.clone()
            } else {
                return Err(format!(
                    "Expected identifier, got {:?} at {}",
                    self.current(),
                    self.debug_position()
                ));
            };

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
            params.push(FunctionParameter {
                name: param_name,
                type_name: param_type,
            });
        }
        Ok(params)
    }

    pub fn function_decl(&mut self) -> Result<crate::ast::FunctionDeclaration, String> {
        self.advance(); // Skip function keyword
        let name = if let Some(TokenKind::Identifier(name)) = self.current() {
            name.clone()
        } else {
            return Err(format!(
                "Expected identifier, got {:?} at {}",
                self.current(),
                self.debug_position()
            ));
        };

        self.advance();
        self.expect(&TokenKind::Symbol(Symbol::OpenParen))?;
        self.advance();

        let params: Vec<FunctionParameter> = self.fn_params()?;

        let return_type = if let Some(TokenKind::Symbol(Symbol::Arrow)) = self.current() {
            self.advance();
            if let Some(TokenKind::Identifier(return_type)) = self.current() {
                self.advance();
                Some(return_type.clone())
            } else {
                return Err(format!(
                    "Expected type name, got {:?} at {}",
                    self.current(),
                    self.debug_position()
                ));
            }
        } else {
            None
        };

        let body = self.block()?;

        Ok(FunctionDeclaration {
            name,
            return_type,
            params,
            body,
        })
    }

    pub fn struct_decl(&mut self) -> Result<StructDeclaration, String> {
        self.eat(TokenKind::Keyword(Keyword::Struct))?;
        let name = if let Some(TokenKind::Identifier(name)) = self.advance() {
            name.clone()
        } else {
            return Err(format!(
                "Expected identifier, got {:?} at {}",
                self.current(),
                self.debug_position()
            ));
        };
        let mut fields = Vec::new();
        self.eat(TokenKind::Symbol(Symbol::OpenBrace))?;
        loop {
            if let Some(TokenKind::Symbol(Symbol::CloseBrace)) = self.current() {
                self.advance();
                break;
            }

            let Some(TokenKind::Identifier(type_name)) = self.advance() else {
                return Err(format!(
                    "Expected identifier, got {:?} at {}",
                    self.current(),
                    self.debug_position()
                ));
            };

            let Some(TokenKind::Identifier(field_name)) = self.advance() else {
                return Err(format!(
                    "Expected identifier, got {:?} at {}",
                    self.current(),
                    self.debug_position()
                ));
            };

            fields.push(StructField {
                name: field_name,
                type_name,
            });

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

        Ok(StructDeclaration { name, fields })
    }

    pub fn block(&mut self) -> Result<crate::ast::Block, String> {
        self.expect(&TokenKind::Symbol(Symbol::OpenBrace))?;
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

        Ok(Block { statements })
    }

    pub fn assignment(&mut self) -> Result<Expression, String> {
        let left = self.logical_or()?;

        if let Some(TokenKind::Operator(Operator::Assign(op))) = self.current() {
            Ok(Expression::VarAssignment {
                var_assign: VarAssignment {
                    operator: op,
                    left: Box::from(self.validate_assign_target(left)?),
                    right: Box::from(self.expression()?),
                },
            })
        } else {
            Ok(left)
        }
    }

    fn validate_assign_target(&self, target: Expression) -> Result<Expression, String> {
        match &target {
            Expression::Identifier { name } => Ok(target),
            Expression::MemberAccess { member_access } => Ok(target),
            //Expression::IndexAccess(_) => Ok(()),
            _ => Err(format!(
                "Invalid assignment target: {:?} at {}",
                target,
                self.debug_position()
            )),
        }
    }

    pub fn if_expr(&mut self) -> Result<Expression, String> {
        self.advance();
        let condition = self.expression()?;
        let body = self.statement()?;
        let else_body = if let Some(TokenKind::Keyword(Keyword::Else)) = self.current() {
            self.advance();
            Some(self.statement()?)
        } else {
            None
        };
        Ok(Expression::If {
            expr: IfExpression {
                condition: Box::from(condition),
                body: Box::from(body),
                else_body: Box::from(else_body),
            },
        })
    }

    pub fn struct_init(&mut self) -> Result<Expression, String> {
        let name = if let Some(TokenKind::Identifier(name)) = self.advance() {
            name
        } else {
            return Err(format!(
                "Expected identifier, got {:?} at {}",
                self.current(),
                self.debug_position()
            ));
        };

        self.eat(TokenKind::Symbol(Symbol::OpenBrace))?;
        let mut fields = Vec::new();
        loop {
            if let Some(TokenKind::Symbol(Symbol::CloseBrace)) = self.current() {
                self.advance();
                break;
            }

            let Some(TokenKind::Identifier(field_name)) = self.advance() else {
                return Err(format!(
                    "Expected identifier, got {:?} at {}",
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
                };
                fields.push(StructInitializerField {
                    field_name: field_name.clone(),
                    value,
                });
                continue;
            } else if let Some(TokenKind::Symbol(Symbol::CloseBrace)) = self.current() {
                let value = Expression::Identifier {
                    name: field_name.clone(),
                };
                fields.push(StructInitializerField {
                    field_name: field_name.clone(),
                    value,
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
            fields.push(StructInitializerField { field_name, value });

            if let Some(TokenKind::Symbol(Symbol::Comma)) = self.current() {
                self.advance();
            } else {
                self.expect(&TokenKind::Symbol(Symbol::CloseBrace))?;
            }
        }
        Ok(Expression::StructInitializer {
            struct_init: StructInitializer {
                struct_name: name,
                fields,
            },
        })
    }

    pub fn expression(&mut self) -> Result<Expression, String> {
        let curr = self.current();
        // Check for If, Block, and Fn Calls
        if let Some(TokenKind::Keyword(Keyword::If)) = curr {
            return self.if_expr();
        } else if let Some(TokenKind::Symbol(Symbol::OpenBrace)) = curr {
            return Ok(Expression::Block {
                block: self.block()?,
            });
        } else if let Some(TokenKind::Identifier(_)) = curr {
            if let Some(TokenKind::Symbol(Symbol::OpenBrace)) = self.lookahead() {
                return Ok(self.struct_init()?);
            }
        }

        /* else if let Some(Token::Identifier(_)) = curr {
            if let Some(Token::Symbol(Symbol::OpenParen)) = self.lookahead() {
                return self.fn_call();
            }
        } */
        self.assignment()
    }

    pub fn var_declaration(&mut self) -> Result<Statement, String> {
        self.advance();
        let type_name = if let Some(TokenKind::Identifier(type_name)) = self.current() {
            type_name.clone()
        } else {
            return Err(format!(
                "Expected type name, got {:?} at {}",
                self.current(),
                self.debug_position()
            ));
        };
        self.advance();
        let name = if let Some(TokenKind::Identifier(name)) = self.current() {
            name.clone()
        } else {
            return Err(format!(
                "Expected identifier, got {:?} at {}",
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
        }))
    }

    pub fn statement(&mut self) -> Result<crate::ast::Statement, String> {
        let statement = match self.current() {
            Some(TokenKind::Keyword(Keyword::Return)) => {
                self.advance();
                let value = if let Some(TokenKind::Symbol(Symbol::Semicolon)) = self.current() {
                    None
                } else {
                    let value = self.expression()?;
                    Some(value)
                };
                Ok(Statement::Return(ReturnStatement { value }))
            }
            Some(TokenKind::Keyword(Keyword::Yield)) => {
                self.advance();
                let expression = self.expression()?;
                Ok(Statement::Yield(YieldStatement { value: expression }))
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
                Ok(Statement::Loop(LoopStatement { body }))
            }
            Some(TokenKind::Keyword(Keyword::Let)) => self.var_declaration(),
            Some(_) => {
                let expression = self.expression()?;
                Ok(Statement::Expression(expression))
            }
            None => Err("Unexpected EOF".to_string()),
        }?;
        self.expect(&TokenKind::Symbol(Symbol::Semicolon))?;
        self.advance();
        Ok(statement)
    }

    fn logical_expr_helper(
        &mut self,
        builder: fn(&mut Parser<'parser>) -> Result<Expression, String>,
        _op_type: OperatorType,
    ) -> Result<Expression, String> {
        let mut left = builder(self)?;

        while let Some(TokenKind::Operator(op)) = self.current() {
            self.advance();
            let right = builder(self)?;
            left = Expression::LogicalOp {
                left: Box::from(left),
                right: Box::from(right),
                op,
            };
        }

        Ok(left)
    }

    fn binary_expr_helper(
        &mut self,
        builder: fn(&mut Parser<'parser>) -> Result<Expression, String>,
        _op_type: OperatorType,
    ) -> Result<Expression, String> {
        let mut left = builder(self)?;

        while let Some(TokenKind::Operator(op)) = self.current() {
            self.advance();
            let right = builder(self)?;
            left = Expression::BinaryOp {
                left: Box::from(left),
                right: Box::from(right),
                op,
            };
        }

        Ok(left)
    }

    pub fn logical_or(&mut self) -> Result<Expression, String> {
        self.logical_expr_helper(Self::logical_and, OperatorType::LogicalOr)
    }

    pub fn logical_and(&mut self) -> Result<Expression, String> {
        self.logical_expr_helper(Self::equality, OperatorType::LogicalAnd)
    }

    pub fn additive(&mut self) -> Result<Expression, String> {
        self.binary_expr_helper(Self::multiplicative, OperatorType::Additive)
    }

    pub fn multiplicative(&mut self) -> Result<Expression, String> {
        self.binary_expr_helper(Self::unary, OperatorType::Multiplicative)
    }

    pub fn equality(&mut self) -> Result<Expression, String> {
        self.binary_expr_helper(Self::relational, OperatorType::Equality)
    }

    pub fn relational(&mut self) -> Result<Expression, String> {
        self.binary_expr_helper(Self::additive, OperatorType::Relational)
    }

    pub fn unary(&mut self) -> Result<Expression, String> {
        let curr = self.current();
        match curr {
            Some(TokenKind::Operator(op)) => {
                self.advance();
                if op.is_unary() {
                    Ok(Expression::UnaryOp {
                        op,
                        expr: Box::from(self.unary()?),
                    })
                } else {
                    Err(format!(
                        "Expected unary operator, got {:?} at {}",
                        op,
                        self.debug_position()
                    ))
                }
            }
            Some(_) => self.left_hand_side_expr(),
            None => Err("Unexpected EOF".to_string()),
        }
    }

    pub fn left_hand_side_expr(&mut self) -> Result<Expression, String> {
        self.call_member_expr()
    }

    pub fn call_member_expr(&mut self) -> Result<Expression, String> {
        let member = self.member_expr()?;

        if let Some(TokenKind::Symbol(Symbol::OpenParen)) = self.current() {
            self.call_expr(member)
        } else {
            Ok(member)
        }
    }

    fn call_expr(&mut self, callee: Expression) -> Result<Expression, String> {
        Ok(Expression::FnCall {
            fn_call: FunctionCall {
                callee: Box::from(callee),
                args: self.arguments()?,
            },
        })
    }

    pub fn arguments(&mut self) -> Result<Vec<Expression>, String> {
        let mut args = Vec::new();
        self.expect(&TokenKind::Symbol(Symbol::OpenParen))?;
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
                    self.expect(&TokenKind::Symbol(Symbol::Comma))?;
                    self.advance();
                }
                None => return Err("Unexpected EOF".to_string()),
            };
        }
        Ok(args)
    }

    pub fn member_expr(&mut self) -> Result<Expression, String> {
        let mut object: Expression = self.primary_expr()?;

        while let Some(TokenKind::Symbol(Symbol::Dot))
        | Some(TokenKind::Symbol(Symbol::OpenBracket)) = self.current()
        {
            if let Some(TokenKind::Symbol(Symbol::Dot)) = self.current() {
                self.advance();
                let prop = self.identifier()?;
                object = Expression::MemberAccess {
                    member_access: MemberAccess {
                        object: Box::from(object),
                        member: Box::from(prop),
                        computed: false,
                    },
                };
            } else {
                self.advance();
                let prop = self.expression()?;
                self.expect(&TokenKind::Symbol(Symbol::CloseBracket))?;
                self.advance();
                object = Expression::MemberAccess {
                    member_access: MemberAccess {
                        object: Box::from(object),
                        member: Box::from(prop),
                        computed: true,
                    },
                };
            }
        }

        Ok(object)
    }

    pub fn identifier(&mut self) -> Result<Expression, String> {
        match self.current() {
            Some(TokenKind::Identifier(id)) => {
                self.advance();
                Ok(Expression::Identifier { name: id })
            }
            Some(_) => Err("Expected identifier".to_string()),
            None => Err("Unexpected EOF".to_string()),
        }
    }

    pub fn primary_expr(&mut self) -> Result<Expression, String> {
        match self.current() {
            Some(TokenKind::Literal(literal)) => Ok(self.literal()?),
            Some(TokenKind::Symbol(Symbol::OpenParen)) => Ok(self.paren_expr()?),
            Some(TokenKind::Identifier(_)) => Ok(self.identifier()?),
            Some(_) => Ok(self.left_hand_side_expr()?),
            None => Err("Unexpected EOF".to_string()),
        }
    }

    pub fn literal(&mut self) -> Result<Expression, String> {
        let Some(TokenKind::Literal(literal)) = self.current() else {
            return Err("Expected literal".to_string());
        };
        self.advance();
        Ok(Expression::Literal { literal })
    }

    fn paren_expr(&mut self) -> Result<Expression, String> {
        self.expect(&TokenKind::Symbol(Symbol::OpenParen))?;
        self.advance();
        let expr = self.expression()?;
        self.expect(&TokenKind::Symbol(Symbol::CloseParen))?;
        self.advance();
        Ok(expr)
    }
}
