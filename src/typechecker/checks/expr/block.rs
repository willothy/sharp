use crate::{
    ast::Block,
    debug,
    typechecker::{
        context::LocalTypecheckContext,
        typed_ast::{TypedBlock, TypedStatement},
        TypeChecker,
    },
};

impl<'tc> TypeChecker<'tc> {
    pub fn typecheck_block(
        &self,
        body: &Block,
        mut local_ctx: LocalTypecheckContext<'tc>,
    ) -> Result<TypedBlock<'tc>, String> {
        debug!("typechecker::typecheck_block");
        let mut typed_statements = Vec::new();
        let mut has_result = false;
        for statement in &body.statements {
            let typed_statement = self.typecheck_statement(statement, &mut local_ctx)?;
            // check if resulted
            if matches!(typed_statement, TypedStatement::Result(_)) {
                has_result = true;
            }
            typed_statements.push(typed_statement);
        }

        // make sure there's a result if one is expected
        if has_result && local_ctx.result_type.is_none() {
            return Err("Unexpected result statement".into());
        } else if !has_result && local_ctx.result_type.is_some() {
            return Err("Expected result statement".into());
        }

        Ok(TypedBlock {
            statements: typed_statements,
            ty: local_ctx.result_type.clone(),
        })
    }
}
