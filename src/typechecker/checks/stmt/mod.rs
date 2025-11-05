use crate::{
    ast, debug,
    typechecker::{
        context::LocalTypecheckContext,
        typed_ast::{self, TypedResultStatement, TypedReturnStatement},
        TypeChecker,
    },
};

pub mod var_stmt;

impl<'tc> TypeChecker<'tc> {
    pub fn typecheck_statement(
        &self,
        statement: &ast::Statement,
        local_ctx: &mut LocalTypecheckContext<'tc>,
    ) -> Result<typed_ast::TypedStatement<'tc>, String> {
        debug!("typechecker::typecheck_statement");
        use ast::Statement::*;
        match statement {
            Variable(var_stmt) => self.typecheck_variable_stmt(var_stmt, local_ctx),
            Expression(expr_stmt) => {
                let expr = self.typecheck_expression(expr_stmt, local_ctx.clone())?;
                Ok(typed_ast::TypedStatement::Expression(expr))
            }
            Loop(loop_stmt) => {
                let body = self.typecheck_block(&loop_stmt.body, local_ctx.enter_loop())?;
                Ok(typed_ast::TypedStatement::Loop(
                    typed_ast::TypedLoopStatement { body },
                ))
            }
            Return(ret_stmt) => {
                let (ret_val, ret_ty) = if let Some(ret_val) = &ret_stmt.value {
                    let expr = self.typecheck_expression(
                        ret_val,
                        local_ctx.expect_result(local_ctx.return_type.clone().into()),
                    )?;
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
            Result(result_stmt) => {
                let expr = self.typecheck_expression(&result_stmt.value, local_ctx.clone())?;
                let ty = expr.ty.clone();
                if ty != local_ctx.result_type {
                    return Err(format!(
                        "Expected result type {:?}, found {:?}",
                        local_ctx.result_type, expr.ty
                    ));
                }
                Ok(typed_ast::TypedStatement::Result(TypedResultStatement {
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
}
