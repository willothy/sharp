use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    ast::{FunctionDeclaration, FunctionDefinition},
    debug,
    lowering::IntermediateModule,
    typechecker::{
        context::{new_type, LocalTypecheckContext},
        type_sig::{self, FunctionType, Name, Type, TypeSignature, TypedFunctionParameter},
        typed_ast::{TypedFunctionDeclaration, TypedFunctionDefinition},
        TypeChecker,
    },
};

impl<'tc> TypeChecker<'tc> {
    pub fn typecheck_functions(
        &mut self,
        module: Rc<RefCell<IntermediateModule>>,
    ) -> Result<
        (
            Vec<TypedFunctionDeclaration<'tc>>,
            Vec<TypedFunctionDefinition<'tc>>,
        ),
        String,
    > {
        let mut fn_decls = Vec::new();
        let mut fn_defs = Vec::new();

        for decl in &module.borrow().fn_decls {
            let fn_type = self.resolve_fn_decl_type(&decl)?;
            self.ctx.functions.insert(
                decl.name.clone(),
                Name {
                    ty: new_type(fn_type),
                },
            );
        }
        for def in &module.borrow().fn_defs {
            let fn_type = self.resolve_fn_def_type(def)?;
            self.ctx.functions.insert(
                def.name.clone(),
                Name {
                    ty: new_type(fn_type),
                },
            );
        }

        let local_ctx = LocalTypecheckContext::from(&self.ctx);

        for decl in &module.borrow().fn_decls {
            let func = self.typecheck_fn_decl(decl, local_ctx.clone())?;
            fn_decls.push(func);
        }

        for def in &module.borrow().fn_defs {
            let func = self.typecheck_fn_def(def, local_ctx.clone())?;
            fn_defs.push(func);
        }

        Ok((fn_decls, fn_defs))
    }

    pub fn typecheck_fn_decl(
        &self,
        function: &FunctionDeclaration,
        mut local_ctx: LocalTypecheckContext<'tc>,
    ) -> Result<TypedFunctionDeclaration<'tc>, String> {
        debug!("typechecker::typecheck_fn_decl");
        use TypeSignature::*;
        let fn_type = self.resolve_fn_decl_type(function)?;
        let Function(FunctionType {
            return_type,
            params,
            variadic,
            has_self_param: _,
            name: _,
            attrs: _
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
            attrs: function.attrs.clone(),
        })
    }

    pub fn typecheck_fn_def(
        &self,
        function: &FunctionDefinition,
        mut local_ctx: LocalTypecheckContext<'tc>,
    ) -> Result<TypedFunctionDefinition<'tc>, String> {
        debug!("typechecker::typecheck_fn_def");
        use TypeSignature::*;
        let fn_type = self.resolve_fn_def_type(function)?;
        let Function(FunctionType {
            return_type,
            params,
            variadic,
            has_self_param,
            name: _,
            attrs: _
        }) = &fn_type.sig else {
            return Err(format!(
                "Expected function type, found {:?}",
                fn_type.sig
            ));
        };

        /* let fn_name = if let Some(impl_ctx) = local_ctx.impl_ctx.clone() {
            let TypeSignature::Struct(struct_id) = impl_ctx.struct_ty.sig() else {
                return Err(format!(
                    "Expected struct type, found {:?}",
                    impl_ctx.struct_ty.sig()
                ));
            };
            let Some(struct_ty) = self.ctx.struct_types.get(&struct_id) else {
                return Err(format!("Could not find struct type {}", struct_id));
            };

            struct_ty.name.clone() + "." + &function.name
        } else {
            function.name.clone()
        }; */

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
            has_self_param: *has_self_param,
            attrs: function.attrs.clone(),
        })
    }

    pub fn resolve_fn_decl_type(
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
                has_self_param: false,
                name: function.name.clone(),
                attrs: function.attrs.clone(),
            }),
        })
    }

    pub fn resolve_fn_def_type(
        &self,
        function: &FunctionDefinition,
    ) -> Result<type_sig::Type<'tc>, String> {
        let mut params = HashMap::new();
        let mut has_self_param = false;
        for param in &function.params {
            let t = self.ctx.get_type(param.type_name.clone())?;
            if param.name.as_str() == "self" {
                has_self_param = true;
            }
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
                has_self_param,
                name: function.name.clone(),
                attrs: function.attrs.clone(),
            }),
        })
    }
}
