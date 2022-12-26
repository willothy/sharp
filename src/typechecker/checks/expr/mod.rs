use crate::{
    ast::{Expression, ScopeResolution},
    debug, debugln,
    typechecker::{
        context::{new_type, LocalTypecheckContext, TypeSig},
        type_sig::{FunctionType, Name, PointerType, Type, TypeSignature},
        typed_ast::{
            TypedExpression, TypedExpressionData, TypedIfExpression, TypedLiteral,
            TypedVarAssignment,
        },
        TypeChecker,
    },
};

pub mod binop;
pub mod block;
pub mod fn_call;
pub mod literal;
pub mod logical_op;
pub mod member_access;
pub mod struct_init;
pub mod unary;

impl<'tc> TypeChecker<'tc> {
    pub fn typecheck_expression(
        &self,
        expr: &Expression,
        mut local_ctx: LocalTypecheckContext<'tc>,
    ) -> Result<TypedExpression<'tc>, String> {
        debug!(format!("typechecker::typecheck_expression: {:?}", expr));
        match expr {
            Expression::BinaryOp {
                left, right, op, ..
            } => self.typecheck_binary_op(left, right, op, local_ctx),
            Expression::LogicalOp {
                left, right, op, ..
            } => self.typecheck_logical_op(left, right, op, local_ctx),
            Expression::ScopeResolution { scope_resolution } => {
                let ScopeResolution {
                    object,
                    member,
                    span: _,
                } = scope_resolution;
                debugln!();
                let mut object = object.as_ref();
                let mut path = Vec::new();
                while let Expression::ScopeResolution { scope_resolution } = object {
                    let ScopeResolution {
                        object: new_object,
                        member,
                        span: _,
                    } = scope_resolution;
                    if let Expression::Identifier { name, .. } = member.as_ref() {
                        path.push(name);
                    } else {
                        return Err(format!("Unsupported scope resolution member"));
                    }
                    object = new_object.as_ref();
                }
                if let Expression::Identifier { name, .. } = object {
                    path.push(name);
                } else {
                    return Err(format!("Unsupported scope resolution object"));
                }

                let Expression::Identifier { name: fn_name, .. } = member.as_ref() else {
                    return Err(format!("Unsupported scope resolution member"));
                };

                let start = path.first().unwrap().clone();

                // Resolve type of object
                if let Ok(object_type) = self.ctx.get_type(start.clone()) {
                    // It's a struct
                    let object_sig = object_type.sig();

                    let Some(struct_ty) = (match &object_sig {
                        TypeSignature::Struct(s) => self.ctx.struct_types.get(&s),
                        TypeSignature::Pointer(PointerType {
                            target,
                        }) => {
                            match target.as_ref() {
                                TypeSignature::Struct(s_id) => {
                                    self.ctx.struct_types.get(&s_id)
                                },
                                _ => return Err(format!("Cannot access member of non-struct type {:?}", object_sig)),
                            }
                        }
                        _ => return Err(format!("Cannot access member of non-struct type {:?}", object_sig)),
                    }) else {
                        return Err(format!("Struct type does not exist"));
                    };

                    let member_method = struct_ty.methods.get(fn_name).ok_or(format!(
                        "Struct {} does not have a method {}",
                        struct_ty.name, fn_name
                    ))?;

                    if member_method.has_self_param {
                        return Err(format!(
                            "Cannot call static method <{}> on instance of struct {}",
                            fn_name, struct_ty.name
                        ));
                    }

                    let member_fn_ty = member_method.fn_ty.clone();
                    let full_name = if let TypeSignature::Function(f) = member_fn_ty.sig() {
                        if f.has_self_param {
                            struct_ty.name.clone() + "." + &fn_name
                        } else {
                            struct_ty.name.clone() + "::" + &fn_name
                        }
                    } else {
                        return Err(format!("Member function must be a function"));
                    };
                    local_ctx
                        .names
                        .insert(full_name.clone(), Name { ty: member_fn_ty });
                    let name = local_ctx.names.get(&full_name).unwrap();
                    Ok(TypedExpression {
                        ty: match name.ty.sig() {
                            TypeSignature::Function(FunctionType { return_type, .. }) => {
                                return_type
                            }
                            _ => return Err(format!("{:?} is not a function", name.ty.sig())),
                        },
                        expr: TypedExpressionData::Identifier { name: full_name },
                    })
                } else {
                    // It's a module
                    todo!()
                }
            }
            Expression::UnaryOp { expr, op, .. } => self.typecheck_unary_op(expr, op, local_ctx),
            Expression::Identifier { name, .. } => {
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
                let cond = self.typecheck_expression(
                    &expr.condition,
                    local_ctx.expect_result(Some(self.ctx.get_type("bool".into())?)),
                )?;
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

                let result_ty = match &then {
                    TypedExpression { expr, ty } => {
                        if let TypedExpressionData::Block { block } = expr {
                            if let Some(result_ty) = &block.ty {
                                Some(result_ty.clone())
                            } else {
                                None
                            }
                        } else {
                            ty.clone()
                        }
                    }
                };

                // check if result type is expected
                if let Some(result_ty) = &result_ty {
                    if let Some(expected_result_ty) = &local_ctx.result_type {
                        if *result_ty != *expected_result_ty {
                            return Err(format!(
                                "Expected result type {:?}, found {:?}",
                                expected_result_ty, result_ty
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
                            result_ty,
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
                            local_ctx.expect_result(left.ty),
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

                        let right = self.typecheck_expression(
                            &var_assign.right,
                            local_ctx.expect_result(Some(ty.clone())),
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
                            local_ctx.expect_result(Some(ty.clone())),
                        )?;

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
            Expression::SizeOfExpr { expr, .. } => {
                if let Expression::Identifier { name, .. } = expr.as_ref() {
                    let ty = self.ctx.get_type(name.clone())?;
                    Ok(TypedExpression {
                        ty: Some(self.ctx.get_type("i64".to_string())?),
                        expr: TypedExpressionData::SizeOfExpr { ty },
                    })
                } else {
                    return Err("Invalid argument to sizeof".to_string());
                }
            }
            Expression::AsExpr {
                expr, type_name, ..
            } => {
                let expr = self.typecheck_expression(expr, local_ctx)?;
                let as_ty = self.ctx.get_type(type_name.clone())?;
                let Some(expr_ty) = expr.ty.clone() else {
                    return Err(format!("Cannot cast expression of unknown type"));
                };
                if expr_ty.sig().can_cast_to(&as_ty.sig()) {
                    Ok(TypedExpression {
                        ty: Some(as_ty.clone()),
                        expr: TypedExpressionData::AsExpr {
                            expr: Box::from(expr),
                            ty: as_ty,
                        },
                    })
                } else {
                    Err(format!(
                        "Cannot cast expression of type {:?} to type {:?}",
                        expr_ty, as_ty
                    ))
                }
            }
        }
    }
}
