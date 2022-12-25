use crate::{
    ast::Expression,
    debug,
    tokenizer::Operator,
    typechecker::{
        context::{LocalTypecheckContext, TypeSig},
        typed_ast::{TypedExpression, TypedExpressionData},
        TypeChecker,
    },
};

impl<'tc> TypeChecker<'tc> {
    pub fn typecheck_binary_op(
        &self,
        left: &Expression,
        right: &Expression,
        op: &Operator,
        local_ctx: LocalTypecheckContext<'tc>,
    ) -> Result<TypedExpression<'tc>, String> {
        debug!("typechecker::typecheck_binary_op");

        use Operator::*;
        let left_type = self.typecheck_expression(left, local_ctx.expect_result(None))?;
        let right_type =
            self.typecheck_expression(right, local_ctx.expect_result(left_type.ty.clone()))?;
        let (Some(left_ty), Some(right_ty)) = (&left_type.ty, &right_type.ty) else {
            return Err("Cannot perform binary operation on non-existant types".into());
        };
        let (left_ty, right_ty) = (left_ty.sig(), right_ty.sig());
        let expr_ty = match op {
            Plus | Minus | Times | Divide => {
                if left_ty != right_ty {
                    return Err(format!(
                        "Additive: Cannot perform operation {} on {:?} and {:?}",
                        op, left_ty, right_ty
                    ));
                }
                left_type.ty.clone()
            }
            Equals | NotEquals | LessThan | GreaterThan | LessOrEqual | GreaterOrEqual => {
                if left_ty != right_ty {
                    return Err(format!(
                        "Equality: Cannot perform operation {} on {:?} and {:?}",
                        op, left_ty, right_ty
                    ));
                }

                let bool = self.ctx.types.get("bool").cloned();
                bool
            }
            And | Or => {
                if left_ty != right_ty {
                    return Err(format!(
                        "And/Or: Cannot perform operation {} on {:?} and {:?}",
                        op, left_ty, right_ty
                    ));
                }
                left_type.ty.clone()
            }
            /* Assign(_op) => {
                if left_type != right_type {
                    return Err(format!(
                        "Cannot assign {:?} to {:?}",
                        right_ty, left_ty
                    ));
                }
                Ok(left_type)
            } */
            Modulo | Power | Not | BitwiseAnd | BitwiseOr | BitwiseXor | BitwiseNot
            | BitwiseLeftShift | BitwiseRightShift => {
                if left_ty != right_ty {
                    return Err(format!(
                        "Bitwise: Cannot perform operation {} on {:?} and {:?}",
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
}
