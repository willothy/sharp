use crate::{
    ast::VarDeclaration,
    typechecker::{
        context::LocalTypecheckContext,
        type_sig::Name,
        typed_ast::{self, TypedExpression, TypedStatement},
        TypeChecker,
    },
};

impl<'tc> TypeChecker<'tc> {
    pub fn typecheck_variable_stmt(
        &self,
        var_stmt: &VarDeclaration,
        local_ctx: &mut LocalTypecheckContext<'tc>,
    ) -> Result<TypedStatement<'tc>, String> {
        let VarDeclaration {
            name,
            type_name,
            initializer,
            ..
        } = var_stmt;
        let ty = self.ctx.get_type(type_name.clone())?;
        let initializer: Option<TypedExpression> = if let Some(init) = initializer {
            Some(self.typecheck_expression(init, local_ctx.expect_result(Some(ty.clone()).into()))?)
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
}
