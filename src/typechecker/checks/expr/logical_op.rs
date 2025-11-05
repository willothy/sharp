use crate::{
    ast::Expression,
    debug,
    tokenizer::Operator,
    typechecker::{
        context::LocalTypecheckContext,
        typed_ast::{TypedExpression, TypedExpressionData},
        TypeChecker,
    },
};

impl<'tc> TypeChecker<'tc> {
    pub fn typecheck_logical_op(
        &self,
        left: &Expression,
        right: &Expression,
        op: &Operator,
        local_ctx: LocalTypecheckContext<'tc>,
    ) -> Result<TypedExpression<'tc>, String> {
        debug!("typechcker::typecheck_logical_op");
        let left_type = self.typecheck_expression(left, local_ctx.clone())?;
        let right_type = self.typecheck_expression(right, local_ctx.clone())?;
        if left_type.ty != right_type.ty {
            return Err(format!(
                "Logical op: Cannot perform operation {} on {:?} and {:?}",
                op, left_type, right_type
            ));
        }
        Ok(TypedExpression {
            ty: Some(self.ctx.get_type("bool".into())?),
            expr: TypedExpressionData::LogicalOp {
                left: Box::from(left_type),
                right: Box::from(right_type),
                op: *op,
            },
        })
    }
}
