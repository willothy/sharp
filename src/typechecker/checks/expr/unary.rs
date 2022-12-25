use crate::{
    ast::Expression,
    debug,
    tokenizer::Operator,
    typechecker::{
        context::{new_type, LocalTypecheckContext, TypeSig},
        type_sig::Type,
        typed_ast::{TypedExpression, TypedExpressionData},
        TypeChecker,
    },
};

impl<'tc> TypeChecker<'tc> {
    pub fn typecheck_unary_op(
        &self,
        expr: &Expression,
        op: &Operator,
        local_ctx: LocalTypecheckContext<'tc>,
    ) -> Result<TypedExpression<'tc>, String> {
        debug!("typechecker::typecheck_unary_op");
        let expr_type = self.typecheck_expression(expr, local_ctx.clone())?;
        let Some(ty) = &expr_type.ty else {
            return Err(format!("Cannot dereference {:?}", expr_type));
        };
        let mut ty = ty.sig();
        if matches!(op, Operator::Times) {
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
}
