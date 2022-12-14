// Author: Will Hopkins

use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    ast::{
        self, Block, Declaration, Expression, FunctionDeclaration, FunctionDefinition,
        MemberAccess, StructDeclaration, StructInitializer, StructInitializerField, VarDeclaration,
    },
    tokenizer::{Literal, Operator},
    typechecker::{
        context::new_type,
        type_sig::{Name, Type, TypeSignature},
        typed_ast::{TypedExpression, TypedReturnStatement, TypedYieldStatement},
    },
};

use super::{
    context::{LocalTypecheckContext, TypeCheckContext, TypeRef, TypeSig},
    type_sig::{
        self, FunctionType, PrimitiveType, StructType, TypedFunctionParameter, TypedStructField,
    },
    typed_ast::{
        self, TypedBlock, TypedDeclaration, TypedExpressionData, TypedFunctionCall,
        TypedFunctionDeclaration, TypedFunctionDefinition, TypedIfExpression, TypedLiteral,
        TypedMemberAccess, TypedStatement, TypedStructDeclaration, TypedStructInitializer,
        TypedStructInitializerField, TypedVarAssignment,
    },
};

#[derive(Debug, Clone)]
pub struct TypeCheckModule<'tc> {
    pub ctx: TypeCheckContext<'tc>,
    pub module: super::typed_ast::TypedModule<'tc>,
}

#[derive(Debug, Clone)]
pub struct TypeChecker<'tc> {
    pub ctx: TypeCheckContext<'tc>,
}

impl<'tc> TypeChecker<'tc> {
    pub fn new() -> Self {
        Self {
            ctx: TypeCheckContext::new(),
        }
    }

    pub fn typecheck_module(
        &'tc mut self,
        module: &'tc ast::Module,
    ) -> Result<TypeCheckModule<'tc>, String> {
        self.preload_structs(&module.body)?;

        let mut typed_decls = Vec::new();

        // Resolve struct types
        for decl in &module.body {
            if let Declaration::Struct(s) = decl {
                let typed_decl: TypedStructDeclaration<'tc> = self.typecheck_struct_decl(&s)?;
                let struct_type = typed_decl.ty.clone();
                self.ctx.types.insert(s.name.clone(), struct_type);
                typed_decls.push(TypedDeclaration::Struct(typed_decl));
            }
        }

        // Resolve function types
        for decl in &module.body {
            if let Declaration::FunctionDef(f) = decl {
                let fn_type: Type = self.resolve_fn_def_type(&f)?;
                self.ctx.names.insert(
                    f.name.clone(),
                    Name {
                        ty: new_type(fn_type),
                    },
                );
            } else if let Declaration::FunctionDecl(f) = decl {
                let fn_type: Type = self.resolve_fn_decl_type(&f)?;
                self.ctx.names.insert(
                    f.name.clone(),
                    Name {
                        ty: new_type(fn_type),
                    },
                );
            }
        }

        let local_ctx = LocalTypecheckContext::from(&self.ctx);

        // Typecheck functions
        for decl in &module.body {
            if let Declaration::FunctionDef(f) = decl {
                let typed_decl: TypedFunctionDefinition<'tc> =
                    self.typecheck_fn_def(&f, local_ctx.clone())?;
                typed_decls.push(TypedDeclaration::FunctionDef(typed_decl));
            } else if let Declaration::FunctionDecl(f) = decl {
                let typed_decl: TypedFunctionDeclaration<'tc> =
                    self.typecheck_fn_decl(&f, local_ctx.clone())?;

                typed_decls.push(TypedDeclaration::FunctionDecl(typed_decl));
            }
        }

        let typed_module = super::typed_ast::TypedModule {
            name: module.name.clone(),
            body: typed_decls,
        };

        Ok(TypeCheckModule {
            ctx: self.ctx.clone(),
            module: typed_module,
        })
    }

    fn typecheck_fn_decl(
        &self,
        function: &FunctionDeclaration,
        mut local_ctx: LocalTypecheckContext<'tc>,
    ) -> Result<TypedFunctionDeclaration<'tc>, String> {
        use TypeSignature::*;
        let fn_type = self.resolve_fn_decl_type(function)?;
        let Function(FunctionType {
            return_type,
            params,
            variadic
        }) = &fn_type.sig else {
            return Err(format!(
                "Expected function type, found {:?}",
                fn_type.sig
            ));
        };
        local_ctx.return_type = return_type.clone();
        for (_, param) in params {
            local_ctx.names.insert(
                param.name.clone(),
                Name {
                    ty: param.ty.clone(),
                },
            );
        }
        Ok(TypedFunctionDeclaration {
            name: function.name.clone(),
            ret_ty: return_type.clone(),
            params: params.clone(),
            fn_ty: new_type(fn_type.clone()),
            variadic: variadic.clone(),
        })
    }

    fn preload_structs(&mut self, declarations: &Vec<Declaration>) -> Result<(), String> {
        use Declaration::Struct;
        // Load all types without fields for resolving circular dependencies
        for decl in declarations {
            if let Struct(structure) = decl {
                self.ctx.types.insert(
                    structure.name.clone(),
                    new_type(Type {
                        sig: TypeSignature::Struct(None),
                    }),
                );
            }
        }
        Ok(())
    }

    fn typecheck_fn_def(
        &'tc self,
        function: &'tc FunctionDefinition,
        mut local_ctx: LocalTypecheckContext<'tc>,
    ) -> Result<TypedFunctionDefinition<'tc>, String> {
        use TypeSignature::*;
        let fn_type = self.resolve_fn_def_type(function)?;
        let Function(FunctionType {
            return_type,
            params,
            variadic
        }) = &fn_type.sig else {
            return Err(format!(
                "Expected function type, found {:?}",
                fn_type.sig
            ));
        };
        local_ctx.return_type = return_type.clone();
        for (_, param) in params {
            local_ctx.names.insert(
                param.name.clone(),
                Name {
                    ty: param.ty.clone(),
                },
            );
        }
        let body = self.typecheck_block(&function.body, local_ctx.clone())?;
        Ok(TypedFunctionDefinition {
            name: function.name.clone(),
            ret_ty: return_type.clone(),
            params: params.clone(),
            body,
            fn_ty: new_type(fn_type.clone()),
            variadic: variadic.clone(),
        })
    }

    fn typecheck_block(
        &'tc self,
        body: &Block,
        mut local_ctx: LocalTypecheckContext<'tc>,
    ) -> Result<TypedBlock, String> {
        let mut typed_statements = Vec::new();
        let mut yielded = false;
        for statement in &body.statements {
            let typed_statement = self.typecheck_statement(statement, &mut local_ctx)?;
            // check if yield
            if let TypedStatement::Yield(_) = typed_statement {
                yielded = true;
            }
            typed_statements.push(typed_statement);
        }

        // make sure there's a yield if one is expected
        if yielded && local_ctx.yield_type.is_none() {
            return Err("Unexpected yield statement".into());
        } else if !yielded && local_ctx.yield_type.is_some() {
            return Err("Expected yield statement".into());
        }

        Ok(TypedBlock {
            statements: typed_statements,
            ty: local_ctx.yield_type.clone(),
        })
    }

    fn typecheck_statement(
        &'tc self,
        statement: &ast::Statement,
        local_ctx: &mut LocalTypecheckContext<'tc>,
    ) -> Result<typed_ast::TypedStatement, String> {
        use ast::Statement::*;
        match statement {
            Variable(var_stmt) => {
                let VarDeclaration {
                    name,
                    type_name,
                    initializer,
                    span,
                } = var_stmt;
                let ty = self.ctx.get_type(type_name.clone())?;
                let initializer: Option<TypedExpression> = if let Some(init) = initializer {
                    Some(self.typecheck_expression(init, local_ctx.with_yield(Some(ty.clone())))?)
                } else {
                    None
                };
                // Make sure initializer is of expected type
                if let Some(init) = &initializer {
                    if init.ty != Some(ty.clone()) {
                        return Err(format!(
                            "Variable init is not of expected type: Expected {:?}, found {:?}",
                            ty, init.ty
                        ));
                    }
                }
                local_ctx
                    .names
                    .insert(name.clone(), Name { ty: ty.clone() });
                Ok(typed_ast::TypedStatement::Variable(
                    typed_ast::TypedVarDeclaration {
                        name: name.clone(),
                        ty,
                        initializer,
                    },
                ))
            }
            Expression(expr_stmt) => {
                let expr = self.typecheck_expression(expr_stmt, local_ctx.clone())?;
                Ok(typed_ast::TypedStatement::Expression(expr))
            }
            Loop(loop_stmt) => {
                let body = self.typecheck_block(&loop_stmt.body, local_ctx.clone())?;
                Ok(typed_ast::TypedStatement::Loop(
                    typed_ast::TypedLoopStatement { body },
                ))
            }
            Return(ret_stmt) => {
                let (ret_val, ret_ty) = if let Some(ret_val) = &ret_stmt.value {
                    let expr = self.typecheck_expression(ret_val, local_ctx.clone())?;
                    let ty = expr.ty.clone();
                    (Some(expr), ty)
                } else {
                    (None, None)
                };

                if ret_ty != local_ctx.return_type {
                    return Err(format!(
                        "Expected return type {:?}, found {:?}",
                        local_ctx.return_type, ret_ty
                    ));
                }
                Ok(typed_ast::TypedStatement::Return(TypedReturnStatement {
                    value: ret_val,
                }))
            }
            Yield(yield_stmt) => {
                let expr = self.typecheck_expression(&yield_stmt.value, local_ctx.clone())?;
                let ty = expr.ty.clone();
                if ty != local_ctx.yield_type {
                    return Err(format!(
                        "Expected yield type {:?}, found {:?}",
                        local_ctx.yield_type, expr.ty
                    ));
                }
                Ok(typed_ast::TypedStatement::Yield(TypedYieldStatement {
                    value: expr,
                }))
            }
            Continue => {
                if !local_ctx.in_loop {
                    return Err("Continue statement outside of loop".to_string());
                }
                Ok(typed_ast::TypedStatement::Continue)
            }
            Break => {
                if !local_ctx.in_loop {
                    return Err("Break statement outside of loop".to_string());
                }
                Ok(typed_ast::TypedStatement::Break)
            }
        }
    }

    fn typecheck_expression(
        &'tc self,
        expr: &Expression,
        local_ctx: LocalTypecheckContext<'tc>,
    ) -> Result<TypedExpression<'tc>, String> {
        match expr {
            Expression::BinaryOp {
                left,
                right,
                op,
                span,
            } => self.typecheck_binary_op(left, right, op, local_ctx),
            Expression::LogicalOp {
                left,
                right,
                op,
                span,
            } => self.typecheck_logical_op(left, right, op, local_ctx),
            Expression::UnaryOp { expr, op, span } => self.typecheck_unary_op(expr, op, local_ctx),
            Expression::Identifier { name, span } => {
                let var = local_ctx
                    .names
                    .get(name)
                    .ok_or_else(|| format!("Identifier {} not found in local context", name))?;
                Ok(TypedExpression {
                    ty: Some(var.ty.clone()),
                    expr: TypedExpressionData::Identifier { name: name.clone() },
                })
            }
            Expression::Literal { literal } => {
                let lit: TypedLiteral = self.typecheck_literal(literal, &local_ctx)?;
                Ok(TypedExpression {
                    ty: Some(lit.ty.clone()),
                    expr: TypedExpressionData::Literal { literal: lit },
                })
            }
            Expression::If { expr } => {
                let bool_type = self.ctx.get_type("bool".to_string())?;
                let cond = self.typecheck_expression(&expr.condition, local_ctx.clone())?;
                if cond.ty != Some(bool_type) {
                    return Err(format!(
                        "Invalid condition type: Expected type bool, found {:?} in if condition",
                        cond.ty
                    ));
                }
                let then = self.typecheck_expression(&expr.body, local_ctx.clone())?;

                let otherwise = match *expr.else_body.clone() {
                    Some(body) => Some(self.typecheck_expression(&body, local_ctx.clone())?),
                    None => None,
                };
                if let Some(otherwise) = &otherwise {
                    if otherwise.ty != then.ty {
                        return Err(format!(
                            "If branch result types don't match: {:?} and {:?}",
                            then.ty, otherwise.ty
                        ));
                    }
                }
                let ty = then.ty.clone();

                let yield_ty = match &then {
                    TypedExpression { expr, ty } => {
                        if let TypedExpressionData::Block { block } = expr {
                            if let Some(yield_ty) = &block.ty {
                                Some(yield_ty.clone())
                            } else {
                                None
                            }
                        } else {
                            ty.clone()
                        }
                    }
                };

                // check if yield type is expected
                if let Some(yield_ty) = &yield_ty {
                    if let Some(expected_yield_ty) = &local_ctx.yield_type {
                        if *yield_ty != *expected_yield_ty {
                            return Err(format!(
                                "Expected yield type {:?}, found {:?}",
                                expected_yield_ty, yield_ty
                            ));
                        }
                    }
                }

                Ok(TypedExpression {
                    ty: ty.clone(),
                    expr: TypedExpressionData::If {
                        expr: TypedIfExpression {
                            condition: Box::from(cond),
                            body: Box::from(then),
                            else_body: Box::from(otherwise),
                            yield_ty,
                        },
                    },
                })
            }
            Expression::Block { block } => {
                let block = self.typecheck_block(block, local_ctx.clone())?;
                Ok(TypedExpression {
                    ty: block.ty.clone(),
                    expr: TypedExpressionData::Block { block },
                })
            }
            Expression::VarAssignment { var_assign } => {
                let left = self.typecheck_expression(&*var_assign.left, local_ctx.clone())?;
                match left.expr {
                    TypedExpressionData::Identifier { name } => {
                        let var = local_ctx.names.get(&name).ok_or_else(|| {
                            format!("Identifier {} not found in local context", name)
                        })?;

                        let right = self.typecheck_expression(
                            &var_assign.right,
                            local_ctx.with_yield(Some(var.ty.clone())),
                        )?;
                        if right.ty != Some(var.ty.clone()) {
                            return Err(format!(
                                "Invalid assignment: Expected type {:?}, found {:?} in assignment",
                                var.ty, right.ty
                            ));
                        }
                        Ok(TypedExpression {
                            ty: Some(var.ty.clone()),
                            expr: TypedExpressionData::VarAssignment {
                                var_assign: TypedVarAssignment {
                                    left: Box::from(TypedExpression {
                                        ty: Some(var.ty.clone()),
                                        expr: TypedExpressionData::Identifier {
                                            name: name.clone(),
                                        },
                                    }),
                                    right: Box::from(right),
                                    operator: var_assign.operator.clone(),
                                },
                            },
                        })
                    }
                    TypedExpressionData::MemberAccess { member_access } => {
                        let ty = match member_access.member.ty.clone() {
                            Some(t) => t,
                            None => return Err("Member access type is unknown".to_string()),
                        };

                        let right =
                            self.typecheck_expression(&var_assign.right, local_ctx.clone())?;
                        /* if right.ty != Some(ty.clone()) {
                            return Err(format!(
                                "Invalid assignment: Expected type {:?}, found {:?} in assignment",
                                ty.clone(),
                                right.ty
                            ));
                        } */
                        Ok(TypedExpression {
                            ty: Some(ty.clone()),
                            expr: TypedExpressionData::VarAssignment {
                                var_assign: TypedVarAssignment {
                                    left: Box::from(TypedExpression {
                                        ty: Some(ty.clone()),
                                        expr: TypedExpressionData::MemberAccess { member_access },
                                    }),
                                    right: Box::from(right),
                                    operator: var_assign.operator.clone(),
                                },
                            },
                        })
                    }
                    TypedExpressionData::UnaryOp { expr, op } => {
                        let ty = match expr.ty.clone() {
                            Some(t) => t.sig().get_ptr_inner_ty(),
                            None => return Err("Unary op type is unknown".to_string()),
                        };

                        let ty = new_type(Type { sig: ty });

                        let right = self.typecheck_expression(
                            &var_assign.right,
                            local_ctx.with_yield(Some(ty.clone())),
                        )?;
                        /* if right.ty != Some(ty.clone()) {
                            return Err(format!(
                                "Invalid assignment: Expected type {:?}, found {:?} in assignment",
                                ty.clone(),
                                right.ty
                            ));
                        } */
                        Ok(TypedExpression {
                            ty: Some(ty.clone()),
                            expr: TypedExpressionData::VarAssignment {
                                var_assign: TypedVarAssignment {
                                    left: Box::from(TypedExpression {
                                        ty: Some(ty.clone()),
                                        expr: TypedExpressionData::UnaryOp {
                                            expr: Box::from(expr),
                                            op,
                                        },
                                    }),
                                    right: Box::from(right),
                                    operator: var_assign.operator.clone(),
                                },
                            },
                        })
                    }
                    other => {
                        return Err(format!("Invalid left hand side of assignment: {:?}", other))
                    }
                }
            }
            Expression::FnCall { fn_call } => self.typecheck_fn_call(fn_call, local_ctx),
            Expression::MemberAccess { member_access } => {
                self.typecheck_member_access(member_access, local_ctx)
            }
            Expression::StructInitializer { struct_init } => {
                self.typecheck_struct_init(struct_init, local_ctx)
            }
        }
    }

    fn typecheck_literal(
        &'tc self,
        literal: &Literal,
        local_ctx: &LocalTypecheckContext<'tc>,
    ) -> Result<TypedLiteral, String> {
        match literal {
            Literal::Str(s, pos) => Ok(TypedLiteral {
                ty: self.ctx.get_type("str".to_string())?,
                literal: Literal::Str(s.clone(), pos.clone()),
            }),
            Literal::Int(i, pos) => match &local_ctx.yield_type {
                Some(t) => {
                    if *t == self.ctx.get_type("i32".into())? {
                        Ok(TypedLiteral {
                            ty: t.clone(),
                            literal: Literal::Int(*i, pos.clone()),
                        })
                    } else if *t == self.ctx.get_type("i64".into())? {
                        Ok(TypedLiteral {
                            ty: t.clone(),
                            literal: Literal::Int(*i, pos.clone()),
                        })
                    } else if *t == self.ctx.get_type("i8".into())? {
                        Ok(TypedLiteral {
                            ty: t.clone(),
                            literal: Literal::Int(*i, pos.clone()),
                        })
                    } else if *t == self.ctx.get_type("i16".into())? {
                        Ok(TypedLiteral {
                            ty: t.clone(),
                            literal: Literal::Int(*i, pos.clone()),
                        })
                    } else {
                        Err(format!(
                            "Invalid int literal: Expected type {:?}, found {:?} on {}",
                            local_ctx.yield_type,
                            *t,
                            literal.position()
                        ))
                    }
                }
                None => return Err(format!("Cannot infer type of int literal {}", i)),
            },
            Literal::Float(f, pos) => match &local_ctx.yield_type {
                Some(t) => {
                    if *t == self.ctx.get_type("f32".into())? {
                        Ok(TypedLiteral {
                            ty: t.clone(),
                            literal: Literal::Float(*f, pos.clone()),
                        })
                    } else if *t == self.ctx.get_type("f64".into())? {
                        Ok(TypedLiteral {
                            ty: t.clone(),
                            literal: Literal::Float(*f, pos.clone()),
                        })
                    } else {
                        Err(format!(
                            "Invalid float literal: Expected type {:?}, found {:?}",
                            local_ctx.yield_type, t
                        ))
                    }
                }
                None => return Err("Cannot infer type of float literal".to_string()),
            },
            Literal::Char(c, pos) => {
                if local_ctx.yield_type == Some(self.ctx.get_type("char".into())?) {
                    Ok(TypedLiteral {
                        ty: self.ctx.get_type("char".into())?,
                        literal: Literal::Char(*c, pos.clone()),
                    })
                } else {
                    Err(format!(
                        "Invalid char literal: Expected type {:?}, found {:?}",
                        local_ctx.yield_type,
                        self.ctx.get_type("char".into())?
                    ))
                }
            }
            Literal::Bool(b, pos) => {
                if local_ctx.yield_type == Some(self.ctx.get_type("bool".into())?) {
                    Ok(TypedLiteral {
                        ty: self.ctx.get_type("bool".into())?,
                        literal: Literal::Bool(*b, pos.clone()),
                    })
                } else {
                    Err(format!(
                        "Invalid bool literal: Expected type {:?}, found {:?}",
                        local_ctx.yield_type,
                        self.ctx.get_type("bool".into())?
                    ))
                }
            }
        }
    }

    fn typecheck_struct_decl(
        &self,
        structure: &StructDeclaration,
    ) -> Result<TypedStructDeclaration<'tc>, String> {
        let mut fields = HashMap::new();
        for field in &structure.fields {
            let Ok(type_val) = self.ctx.get_type(field.type_name.clone()) else {
            return Err(format!("{} is not a valid type 590", field.type_name));
        };

            fields.insert(
                field.name.clone(),
                TypedStructField {
                    name: field.name.clone(),
                    ty: type_val.clone(),
                    idx: field.idx,
                },
            );
        }
        let Ok(type_ref) = self.ctx.get_type(structure.name.clone()) else {
            return Err("".into())
        };
        *type_ref.borrow_mut() = Type::new(TypeSignature::Struct(Some(StructType {
            name: structure.name.clone(),
            fields: fields.clone(),
        })));
        Ok(TypedStructDeclaration {
            name: structure.name.clone(),
            fields: fields.values().map(|field| field.clone()).collect(),
            ty: type_ref,
        })
    }

    fn typecheck_binary_op(
        &'tc self,
        left: &Expression,
        right: &Expression,
        op: &Operator,
        local_ctx: LocalTypecheckContext<'tc>,
    ) -> Result<TypedExpression<'tc>, String> {
        let left_type = self.typecheck_expression(left, local_ctx.clone())?;
        let right_type = self.typecheck_expression(right, local_ctx.clone())?;
        use Operator::*;
        let expr_ty = match op {
            Plus | Minus | Times | Divide => {
                if left_type.ty != right_type.ty {
                    return Err(format!(
                        "Cannot perform operation {} on {:?} and {:?}",
                        op, left_type.ty, right_type.ty
                    ));
                }
                left_type.ty.clone()
            }
            Equals | NotEquals | LessThan | GreaterThan | LessOrEqual | GreaterOrEqual => {
                if left_type.ty != right_type.ty {
                    return Err(format!(
                        "Equality: Cannot perform operation {} on {:?} and {:?}",
                        op, left_type.ty, right_type.ty
                    ));
                }

                let bool = self.ctx.primitives.get("bool").cloned();
                bool
            }
            And | Or => {
                if left_type.ty != right_type.ty {
                    return Err(format!(
                        "Cannot perform operation {} on {:?} and {:?}",
                        op, left_type.ty, right_type.ty
                    ));
                }
                left_type.ty.clone()
            }
            /* Assign(_op) => {
                if left_type != right_type {
                    return Err(format!(
                        "Cannot assign {:?} to {:?}",
                        right_type.ty, left_type.ty
                    ));
                }
                Ok(left_type)
            } */
            Modulo | Power | Not | BitwiseAnd | BitwiseOr | BitwiseXor | BitwiseNot
            | BitwiseLeftShift | BitwiseRightShift => {
                if left_type.ty != right_type.ty {
                    return Err(format!(
                        "Cannot perform operation {} on {:?} and {:?}",
                        op, left_type, right_type
                    ));
                }
                left_type.ty.clone()
            }
            _ => return Err(format!("Unknown: Cannot perform operation {}", op)),
        };
        Ok(TypedExpression {
            ty: expr_ty.clone(),
            expr: TypedExpressionData::BinaryOp {
                left: Box::from(left_type),
                right: Box::from(right_type),
                op: *op,
            },
        })
    }

    fn resolve_fn_decl_type(
        &self,
        function: &FunctionDeclaration,
    ) -> Result<type_sig::Type<'tc>, String> {
        let mut params = HashMap::new();
        for param in &function.params {
            params.insert(
                param.name.clone(),
                TypedFunctionParameter {
                    name: param.name.clone(),
                    ty: self.ctx.get_type(param.type_name.clone())?,
                    idx: param.idx,
                },
            );
        }
        let return_type = if let Some(return_type) = &function.return_type {
            Some(self.ctx.get_type(return_type.clone())?)
        } else {
            None
        };

        Ok(Type {
            sig: TypeSignature::Function(FunctionType {
                return_type,
                params,
                variadic: function.variadic,
            }),
        })
    }

    fn resolve_fn_def_type(
        &self,
        function: &FunctionDefinition,
    ) -> Result<type_sig::Type<'tc>, String> {
        let mut params = HashMap::new();
        for param in &function.params {
            let t = self.ctx.get_type(param.type_name.clone())?;

            params.insert(
                param.name.clone(),
                TypedFunctionParameter {
                    name: param.name.clone(),
                    ty: t,
                    idx: param.idx,
                },
            );
        }
        let return_type = if let Some(return_type) = &function.return_type {
            Some(self.ctx.get_type(return_type.clone())?)
        } else {
            None
        };
        Ok(Type {
            sig: TypeSignature::Function(FunctionType {
                return_type,
                params,
                variadic: function.variadic,
            }),
        })
    }

    fn typecheck_fn_call(
        &'tc self,
        fn_call: &ast::FunctionCall,
        local_ctx: LocalTypecheckContext<'tc>,
    ) -> Result<TypedExpression, String> {
        let fn_name = match *fn_call.callee.clone() {
            Expression::Identifier { name, span } => name,
            _ => return Err("Unsupported callee type".into()),
        };
        let fn_type = local_ctx
            .names
            .get(&fn_name)
            .ok_or(format!("Function {} is not defined in this scope", fn_name))?;
        let ret_type = match fn_type.ty.borrow_mut().sig.clone() {
            TypeSignature::Function(FunctionType { return_type, .. }) => return_type,
            _ => return Err(format!("{} is not a function", fn_name)),
        };
        let mut args = Vec::new();
        for (arg_idx, arg) in fn_call.args.iter().enumerate() {
            let param_type = match fn_type.ty.sig() {
                TypeSignature::Function(FunctionType {
                    params, variadic, ..
                }) => {
                    let t = match params.iter().find(|(_, p)| p.idx as usize == arg_idx) {
                        Some((_, t)) => Some(t.ty.clone()),
                        None => {
                            if arg_idx > (params.len() - 1) {
                                if variadic {
                                    self.typecheck_expression(
                                        &arg.clone(),
                                        local_ctx.with_yield(None),
                                    )?
                                    .ty
                                } else {
                                    return Err(format!(
                                        "Function {} does not have a parameter at index {} and it's not variadic",
                                        fn_name, arg_idx
                                    ));
                                }
                            } else {
                                return Err(format!(
                                    "Function {} does not have a parameter at index {}",
                                    fn_name, arg_idx
                                ));
                            }
                        }
                    };
                    t.clone()
                }
                _ => return Err(format!("{} is not a function", fn_name)),
            };
            args.push(self.typecheck_expression(arg, local_ctx.with_yield(param_type))?);
        }
        let callee = self.typecheck_expression(&fn_call.callee, local_ctx.clone())?;
        Ok(TypedExpression {
            ty: ret_type,
            expr: TypedExpressionData::FnCall {
                fn_call: TypedFunctionCall {
                    callee: Box::from(callee),
                    args,
                    fn_ty: fn_type.ty.clone(),
                },
            },
        })
    }

    fn typecheck_logical_op(
        &'tc self,
        left: &Expression,
        right: &Expression,
        op: &Operator,
        local_ctx: LocalTypecheckContext<'tc>,
    ) -> Result<TypedExpression<'tc>, String> {
        let left_type = self.typecheck_expression(left, local_ctx.clone())?;
        let right_type = self.typecheck_expression(right, local_ctx.clone())?;
        if left_type != right_type {
            return Err(format!(
                "Cannot perform operation {} on {:?} and {:?}",
                op, left_type, right_type
            ));
        }
        Ok(TypedExpression {
            ty: Some(self.ctx.get_type("bool".into())?),
            expr: TypedExpressionData::BinaryOp {
                left: Box::from(left_type),
                right: Box::from(right_type),
                op: *op,
            },
        })
    }

    fn typecheck_unary_op(
        &'tc self,
        expr: &Expression,
        op: &Operator,
        local_ctx: LocalTypecheckContext<'tc>,
    ) -> Result<TypedExpression, String> {
        let expr_type = self.typecheck_expression(expr, local_ctx.clone())?;
        let Some(ty) = &expr_type.ty else {
            return Err(format!("Cannot dereference {:?}", expr_type));
        };
        let mut ty = ty.sig();

        if let Operator::Times = op {
            ty = ty.get_ptr_inner_ty();
        }

        let ty = new_type(Type { sig: ty });

        Ok(TypedExpression {
            ty: Some(ty),
            expr: TypedExpressionData::UnaryOp {
                expr: Box::from(expr_type),
                op: *op,
            },
        })
    }

    fn typecheck_struct_init(
        &'tc self,
        struct_init: &StructInitializer,
        mut local_ctx: LocalTypecheckContext<'tc>,
    ) -> Result<TypedExpression, String> {
        let StructInitializer {
            struct_name,
            fields,
            span,
        } = struct_init;
        let struct_type = self.ctx.get_type(struct_name.clone())?;
        let struct_sig = struct_type.sig();

        let decl_fields = match &struct_sig {
            TypeSignature::Struct(Some(structure_t)) => &structure_t.fields,
            _ => unreachable!(),
        };

        let mut typed_fields = Vec::new();
        for field in fields {
            let StructInitializerField {
                field_name,
                value,
                idx,
                span: field_span,
            } = field;

            let expected_field = decl_fields.get(field_name).ok_or(format!(
                "Field {} does not exist in struct {}",
                field_name, struct_name
            ))?;
            // TODO: Possible issue
            local_ctx.yield_type = Some(expected_field.ty.clone());

            let typed_value = self.typecheck_expression(value, local_ctx.clone())?;

            if typed_value.ty != Some(expected_field.ty.clone()) {
                return Err(format!(
                    "Expected field {} to be of type {:?}, but got {:?}",
                    field_name, expected_field.ty, typed_value.ty
                ));
            }

            let typed_field = TypedStructInitializerField {
                field_name: field_name.clone(),
                value: typed_value,
                idx: expected_field.idx,
            };
            typed_fields.push(typed_field);
        }

        Ok(TypedExpression {
            ty: Some(struct_type.clone()),
            expr: TypedExpressionData::StructInitializer {
                struct_init: TypedStructInitializer {
                    struct_name: struct_name.clone(),
                    fields: typed_fields,
                    struct_ty: struct_type,
                },
            },
        })
    }

    fn typecheck_member_access(
        &'tc self,
        member_access: &ast::MemberAccess,
        mut local_ctx: LocalTypecheckContext<'tc>,
    ) -> Result<TypedExpression<'tc>, String> {
        let MemberAccess {
            object,
            member,
            computed,
            span,
        } = member_access;

        if *computed {
            unimplemented!("Computed member access is not supported yet");
        }

        //let object_type = self.typecheck_expression(object, local_ctx.clone())?;
        //let member_type = self.typecheck_expression(member, local_ctx)?;
        let object = *(object.clone());
        let member = *(member.clone());
        let checked_object = self.typecheck_expression(&object, local_ctx.clone())?;
        //let checked_member = self.typecheck_expression(&member, local_ctx.clone())?;
        match object.clone() {
            Expression::Identifier { name, span } => {
                // If the object is an identifier or function call, we can check the type of the member
                let object_reg = local_ctx
                    .names
                    .get(&name)
                    .ok_or(format!("Cannot find type of object {} in this scope", name))?;
                let object_type = object_reg.ty.clone().sig();

                if let TypeSignature::Struct(Some(structure)) = object_type {
                    if let Expression::Identifier { name, span } = &member {
                        let member_type = structure.fields.get(name).ok_or(format!(
                            "Cannot find field {} in struct {}",
                            name, structure.name
                        ))?;
                        // add struct fields to local context
                        for (field_name, field_type) in &structure.fields {
                            local_ctx.names.insert(
                                field_name.clone(),
                                Name {
                                    ty: field_type.ty.clone(),
                                },
                            );
                        }
                        let member_expr = self.typecheck_expression(&member, local_ctx)?;
                        Ok(TypedExpression {
                            ty: Some(new_type(Type {
                                sig: member_type.ty.sig().wrap_in_ptr(),
                            })),
                            expr: TypedExpressionData::MemberAccess {
                                member_access: TypedMemberAccess {
                                    object: Box::from(checked_object),
                                    member: Box::from(member_expr),
                                    computed: *computed,
                                },
                            },
                        })
                    } else if let Expression::FnCall { fn_call } = member {
                        unimplemented!("Struct member functions are not yet supported");
                    } else {
                        return Err(format!(
                            "Unsupported member access expression: {:?}",
                            member
                        ));
                    }
                } else {
                    unimplemented!()
                }
            }
            Expression::FnCall { fn_call } => todo!(),
            Expression::MemberAccess { member_access } => {
                // ensure object is a struct type
                // ensure object has a member of the given name

                // add struct fields to local context
                // typecheck member expression
                // return member expression
                let checked_access =
                    self.typecheck_member_access(&member_access, local_ctx.clone())?;

                let Some(ty) = checked_access.ty else {
                    return Err(format!(
                        "Cannot access member of non-struct type {:?}",
                        checked_access.ty
                    ));
                };
                let sig = ty.sig();
                if let TypeSignature::Struct(Some(structure)) = sig {
                    if let Expression::Identifier { name, span } = &member {
                        let member_type = structure.fields.get(name).ok_or(format!(
                            "Cannot find field {} in struct {}",
                            name, structure.name
                        ))?;
                        // add struct fields to local context
                        for (field_name, field_type) in &structure.fields {
                            local_ctx.names.insert(
                                field_name.clone(),
                                Name {
                                    ty: field_type.ty.clone(),
                                },
                            );
                        }
                        let member_expr = self.typecheck_expression(&member, local_ctx)?;
                        Ok(TypedExpression {
                            ty: Some(member_type.ty.clone()),
                            expr: TypedExpressionData::MemberAccess {
                                member_access: TypedMemberAccess {
                                    object: Box::from(checked_object),
                                    member: Box::from(member_expr),
                                    computed: *computed,
                                },
                            },
                        })
                    } else if let Expression::FnCall { fn_call } = member {
                        unimplemented!("Struct member functions are not yet supported");
                    } else {
                        return Err(format!(
                            "Unsupported member access expression: {:?}",
                            member
                        ));
                    }
                } else {
                    unimplemented!()
                }
            }
            _ => unimplemented!("Member access on this type is not supported yet"),
        }
    }
}
