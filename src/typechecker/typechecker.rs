// Author: Will Hopkins

use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    ast::{
        self, Block, Declaration, Expression, FmtPath, FunctionDeclaration, FunctionDefinition,
        MemberAccess, Module, ModulePath, ScopeResolution, StructDeclaration, StructInitializer,
        StructInitializerField, VarDeclaration,
    },
    debug, debugln,
    lowering::{Export, IntermediateModule, IntermediateProgram, ModuleId},
    tokenizer::{Literal, Operator},
    typechecker::{
        context::new_type,
        type_sig::{Name, PointerType, Type, TypeSignature},
        typed_ast::{TypedExpression, TypedResultStatement, TypedReturnStatement},
    },
};

use super::{
    context::{LocalTypecheckContext, ModuleTypeCheckCtx, StructId, TypeRef, TypeSig},
    type_sig::{self, FunctionType, StructType, TypedFunctionParameter, TypedStructField},
    typed_ast::{
        self, TypedBlock, TypedDeclaration, TypedExport, TypedExportType, TypedExpressionData,
        TypedFunctionCall, TypedFunctionDeclaration, TypedFunctionDefinition, TypedIfExpression,
        TypedImport, TypedLiteral, TypedMemberAccess, TypedModule, TypedStatement,
        TypedStructDeclaration, TypedStructInitializer, TypedStructInitializerField,
        TypedVarAssignment,
    },
};

/* #[derive(Debug, Clone)]
pub struct TypeCheckModule<'tc> {
    pub ctx: ModuleTypeCheckCtx<'tc>,
    pub module: Rc<RefCell<TypedModule<'tc>>>,
} */

#[derive(Debug, Clone)]
pub struct TypeChecker<'tc> {
    pub ctx: ModuleTypeCheckCtx<'tc>,
    pub intermediate: IntermediateProgram,
    pub modules: Vec<Rc<RefCell<TypedModule<'tc>>>>,
    pub module_ids: HashMap<ModulePath, ModuleId>,
    pub current_module_path: ModulePath,
    pub next_struct_id: StructId,
}

pub trait TCModule<'ctx> {
    fn get_fn_defs(&self) -> Vec<TypedFunctionDefinition<'ctx>>;
    fn get_fn_decls(&self) -> Vec<TypedFunctionDeclaration<'ctx>>;
    fn get_structs(&self) -> Vec<TypedStructDeclaration<'ctx>>;
    fn get_submodules(&self) -> HashMap<String, ModuleId>;
    fn get_requirements(&self) -> Vec<TypedImport>;
    fn get_parent_id(&self) -> Option<ModuleId>;
    fn get_path(&self) -> ast::ModulePath;
    fn get_name(&self) -> String;
    fn has_parent(&self) -> bool;
    fn get_ctx(&self) -> ModuleTypeCheckCtx<'ctx>;
}

impl<'ctx> TCModule<'ctx> for Rc<RefCell<TypedModule<'ctx>>> {
    fn get_fn_defs(&self) -> Vec<TypedFunctionDefinition<'ctx>> {
        self.borrow().fn_defs.clone()
    }

    fn get_fn_decls(&self) -> Vec<TypedFunctionDeclaration<'ctx>> {
        self.borrow().fn_decls.clone()
    }

    fn get_structs(&self) -> Vec<TypedStructDeclaration<'ctx>> {
        self.borrow().structs.clone()
    }

    fn get_submodules(&self) -> HashMap<String, ModuleId> {
        self.borrow().submodules.clone()
    }

    fn get_requirements(&self) -> Vec<TypedImport> {
        self.borrow().dependencies.clone()
    }

    fn get_parent_id(&self) -> Option<ModuleId> {
        self.borrow().parent.clone()
    }

    fn get_path(&self) -> ast::ModulePath {
        self.borrow().path.clone()
    }

    fn get_name(&self) -> String {
        self.borrow().name.clone()
    }

    fn has_parent(&self) -> bool {
        self.borrow().parent.is_some()
    }

    fn get_ctx(&self) -> ModuleTypeCheckCtx<'ctx> {
        self.borrow().ctx.clone()
    }
}

#[derive(Debug, Clone)]
pub struct TypeCheckerOutput<'tc> {
    pub modules: Vec<Rc<RefCell<TypedModule<'tc>>>>,
    pub module_ids: HashMap<ModulePath, ModuleId>,
}

impl<'tc> TypeChecker<'tc> {
    pub fn new(intermediate: IntermediateProgram) -> Self {
        Self {
            intermediate,
            ctx: ModuleTypeCheckCtx::new(),
            modules: Vec::new(),
            module_ids: HashMap::new(),
            current_module_path: Vec::new(),
            next_struct_id: 0,
        }
    }

    pub fn get_intermediate_module_export(&self, path: &ModulePath) -> Option<Export> {
        let mut path = path.clone();
        let Some(name) = path.pop() else {
            return None;
        };
        let Some(module) = self.intermediate.get_module_by_path(&path).ok() else {
            return None;
        };
        let module = module.borrow();
        let Some(export) = module.exports.get(&name) else {
            return None;
        };
        Some(export.clone())
    }

    //pub fn combine_modules(&mut self) -> Result<IntermediateProgram

    pub fn typecheck(&mut self) -> Result<TypeCheckerOutput<'tc>, String> {
        // Preload struct names
        for (path, module_id) in self.intermediate.module_ids.clone() {
            self.module_ids.insert(path.clone(), module_id);
            self.current_module_path = path.clone();

            let Some(module) = self.intermediate.modules.get(module_id).cloned() else {
                return Err(format!("Module with id {} not found", module_id));
            };

            let module = module.borrow();
            for structure in &module.structs {
                let id = self.get_next_struct_id();
                self.ctx.types.insert(
                    structure.name.clone(),
                    new_type(Type {
                        sig: TypeSignature::Struct(id),
                    }),
                );
            }
        }

        // Load dependencies into context

        let mut all_structs: HashMap<StructId, Vec<TypedStructDeclaration>> = HashMap::new();
        for (path, module_id) in self.intermediate.module_ids.clone() {
            self.current_module_path = path.clone();
            let Some(module) = self.intermediate.modules.get(module_id).cloned() else {
                return Err(format!("Module with id {} not found", module_id));
            };

            let structs = self.typecheck_structs(module.clone())?;
            all_structs.insert(module_id, structs);
        }

        for (path, module_id) in self.intermediate.module_ids.clone() {
            self.current_module_path = path.clone();
            let Some(module) = self.intermediate.modules.get(module_id).cloned() else {
                return Err(format!("Module with id {} not found", module_id));
            };
            self.typecheck_struct_methods(module)?;
        }

        for (path, module_id) in self.intermediate.module_ids.clone() {
            self.current_module_path = path.clone();

            let Some(module) = self.intermediate.modules.get(module_id).cloned() else {
                return Err(format!("Module with id {} not found", module_id));
            };

            let structs = all_structs.get(&module_id).cloned().unwrap();

            let (fn_decls, fn_defs) = self.typecheck_functions(module.clone())?;

            let submodules = module.borrow().submodules.clone();

            let parent = module.borrow().parent.clone();

            let name = module.borrow().name.clone();

            let dependencies = module
                .borrow()
                .dependencies
                .iter()
                .map(|dep| TypedImport {
                    name: dep.name.clone(),
                    source_module: dep.source_module,
                })
                .collect::<Vec<TypedImport>>();

            let exports = self.resolve_exports(module.clone(), &submodules)?;
            let new_ctx = ModuleTypeCheckCtx::with_types(&self.ctx);

            let module = TypedModule {
                fn_defs,
                fn_decls,
                structs,
                submodules,
                dependencies,
                parent,
                exports,
                path,
                name,
                id: module_id,
                ctx: std::mem::replace(&mut self.ctx, new_ctx),
            };
            self.modules.push(Rc::new(RefCell::new(module)));
        }

        Ok(TypeCheckerOutput {
            modules: self.modules.clone(),
            module_ids: self.module_ids.clone(),
        })
    }

    pub fn typecheck_struct_methods(
        &mut self,
        module: Rc<RefCell<IntermediateModule>>,
    ) -> Result<(), String> {
        for structure in &module.borrow().structs {
            for method in &structure.methods {
                let Ok(type_ref) = self.ctx.get_type(structure.name.clone()) else {
                    return Err("".into())
                };
                let struct_id = if let TypeSignature::Struct(id) = type_ref.sig() {
                    id
                } else {
                    return Err(format!("{} is not a struct", structure.name));
                };

                // function def
                let func = self.typecheck_fn_def(
                    method,
                    LocalTypecheckContext::impl_method(type_ref.clone()),
                )?;

                let struct_ty = self.ctx.struct_types.get_mut(&struct_id).unwrap();

                struct_ty.methods.insert(func.name.clone(), func);
            }
        }
        Ok(())
    }

    pub fn typecheck_structs(
        &mut self,
        module: Rc<RefCell<IntermediateModule>>,
    ) -> Result<Vec<TypedStructDeclaration<'tc>>, String> {
        let mut structs = Vec::new();
        for structure in &module.borrow().structs {
            /* let id = if let TypeSignature::Struct(id) =
                self.ctx.get_type(structure.name.clone())?.sig()
            {
                id
            } else {
                return Err(format!("{} is not a struct", structure.name));
            };

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

            let mut struct_type = StructType {
                name: structure.name.clone(),
                fields: fields.clone(),
                methods: HashMap::new(),
                id,
            };

            let Ok(type_ref) = self.ctx.get_type(structure.name.clone()) else {
                return Err("".into())
            };
            // *type_ref.borrow_mut() = Type::new(TypeSignature::Struct(id));

            self.ctx.struct_types.insert(id, struct_type.clone());

            for method in &structure.methods {
                // function def
                let func = self.typecheck_fn_def(
                    method,
                    LocalTypecheckContext::impl_method(type_ref.clone()),
                )?;
                struct_type.methods.insert(method.name.clone(), func);
            }

            structs.push(TypedStructDeclaration {
                name: structure.name.clone(),
                fields: fields.values().map(|field| field.clone()).collect(),
                ty: type_ref,
                id,
                methods: struct_type.methods.clone(),
            }); */

            let id = if let TypeSignature::Struct(id) =
                self.ctx.get_type(structure.name.clone())?.sig()
            {
                id
            } else {
                return Err(format!("{} is not a struct", structure.name));
            };

            let mut fields = HashMap::new();
            for field in &structure.fields {
                let type_val = self.ctx.get_type(field.type_name.clone())?;
                fields.insert(
                    field.name.clone(),
                    TypedStructField {
                        name: field.name.clone(),
                        ty: type_val.clone(),
                        idx: field.idx,
                    },
                );
            }

            let struct_type = StructType {
                name: structure.name.clone(),
                fields: fields.clone(),
                methods: HashMap::new(),
                id,
            };

            let Ok(type_ref) = self.ctx.get_type(structure.name.clone()) else {
                return Err("".into())
            };
            //*type_ref.borrow_mut() = Type::new(TypeSignature::Struct(id));

            self.ctx.struct_types.insert(id, struct_type.clone());

            //self.ctx.struct_types.insert(id, struct_type.clone());

            structs.push(TypedStructDeclaration {
                name: structure.name.clone(),
                fields: fields.values().map(|field| field.clone()).collect(),
                ty: type_ref,
                id,
                methods: struct_type.methods.clone(),
            });
        }

        Ok(structs)
    }

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

    pub(crate) fn resolve_exports(
        &self,
        module: Rc<RefCell<IntermediateModule>>,
        submodules: &HashMap<String, ModuleId>,
    ) -> Result<HashMap<String, TypedExport<'tc>>, String> {
        use crate::lowering::ExportType::*;
        let module_exports = module.borrow().exports.clone();

        let mut exports = HashMap::new();
        for (name, export) in module_exports {
            exports.insert(
                name.clone(),
                TypedExport {
                    name: name.clone(),
                    ty: match export.ty {
                        FunctionDef(_) | FunctionDecl(_) => {
                            let Some(fn_id) = self.ctx.functions.get(&name).cloned() else {
                                panic!("Export {} is not a function", name);
                            };
                            if let TypeSignature::Function(func) = fn_id.ty.sig() {
                                TypedExportType::Function(func)
                            } else {
                                panic!("Export {} is not a function", name);
                            }
                        }
                        crate::lowering::ExportType::Struct(_) => {
                            let Some(struct_t) = self.ctx.types.get(&name).cloned() else {
                                panic!("Export {} is not a struct", name);
                            };
                            if let TypeSignature::Struct(id) = struct_t.sig() {
                                TypedExportType::Struct(
                                    self.ctx
                                        .struct_types
                                        .get(&id)
                                        .expect("Struct not found")
                                        .clone(),
                                )
                            } else {
                                panic!("Export {} is not a struct", name);
                            }
                        }
                        crate::lowering::ExportType::Module(_) => {
                            if let Some(sub) = submodules.get(&name) {
                                TypedExportType::Module(*sub)
                            } else {
                                return Err(format!("Export {} is not a module", name));
                            }
                        }
                    },
                },
            );
        }
        Ok(exports)
    }

    fn name_prefix(&self, name: &str) -> String {
        let mod_path = self.current_module_path.fmt_path_no_main();
        if mod_path.as_str() == "main" {
            name.to_string()
        } else {
            format!("{}::{}", mod_path, name)
        }
    }

    fn typecheck_fn_decl(
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
            name: _
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
            name: self.name_prefix(&function.name),
            ret_ty: return_type.clone(),
            params: params.clone(),
            fn_ty: new_type(fn_type.clone()),
            variadic: variadic.clone(),
        })
    }

    fn typecheck_fn_def(
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
            name: _
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
        })
    }

    fn typecheck_block(
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

    pub(crate) fn typecheck_variable_stmt(
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

    fn typecheck_statement(
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

    fn typecheck_expression(
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
                    span,
                } = scope_resolution;
                debugln!();
                let mut object = object.as_ref();
                let mut path = Vec::new();
                while let Expression::ScopeResolution { scope_resolution } = object {
                    let ScopeResolution {
                        object: new_object,
                        member,
                        span,
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
                    let full_name = struct_ty.name.clone() + "." + &fn_name;
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

    fn typecheck_literal(
        &self,
        literal: &Literal,
        local_ctx: &LocalTypecheckContext<'tc>,
    ) -> Result<TypedLiteral<'tc>, String> {
        debug!(format!("typechecker::typecheck_literal: {:?}", literal));
        match literal {
            Literal::Str(s, pos) => Ok(TypedLiteral {
                ty: self.ctx.get_type("str".to_string())?,
                literal: Literal::Str(s.clone(), pos.clone()),
            }),
            Literal::Int(i, pos) => match &local_ctx.result_type {
                Some(t) => {
                    if t.sig() == self.ctx.get_type("i32".into())?.sig() {
                        Ok(TypedLiteral {
                            ty: t.clone(),
                            literal: Literal::Int(*i, pos.clone()),
                        })
                    } else if t.sig() == self.ctx.get_type("i64".into())?.sig() {
                        Ok(TypedLiteral {
                            ty: t.clone(),
                            literal: Literal::Int(*i, pos.clone()),
                        })
                    } else if t.sig() == self.ctx.get_type("i8".into())?.sig() {
                        Ok(TypedLiteral {
                            ty: t.clone(),
                            literal: Literal::Int(*i, pos.clone()),
                        })
                    } else if t.sig() == self.ctx.get_type("i16".into())?.sig() {
                        Ok(TypedLiteral {
                            ty: t.clone(),
                            literal: Literal::Int(*i, pos.clone()),
                        })
                    } else {
                        Err(format!(
                            "Invalid int literal: Expected type {:?}, found {:?} on {}",
                            local_ctx.result_type,
                            *t,
                            literal.position()
                        ))
                    }
                }
                None => {
                    Ok(TypedLiteral {
                        ty: self.ctx.get_type("i32".into())?,
                        literal: Literal::Int(*i, pos.clone()),
                    })
                    /* return Err(format!(
                        "Cannot infer type of int literal {} at {}",
                        i,
                        literal.position()
                    )) */
                }
            },
            Literal::Float(f, pos) => match &local_ctx.result_type {
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
                            local_ctx.result_type, t
                        ))
                    }
                }
                None => return Err("Cannot infer type of float literal".to_string()),
            },
            Literal::Char(c, pos) => {
                if local_ctx.result_type == Some(self.ctx.get_type("char".into())?) {
                    Ok(TypedLiteral {
                        ty: self.ctx.get_type("char".into())?,
                        literal: Literal::Char(*c, pos.clone()),
                    })
                } else {
                    Err(format!(
                        "Invalid char literal: Expected type {:?}, found {:?}",
                        local_ctx.result_type,
                        self.ctx.get_type("char".into())?
                    ))
                }
            }
            Literal::Bool(b, pos) => {
                if local_ctx.result_type == Some(self.ctx.get_type("bool".into())?) {
                    Ok(TypedLiteral {
                        ty: self.ctx.get_type("bool".into())?,
                        literal: Literal::Bool(*b, pos.clone()),
                    })
                } else {
                    Err(format!(
                        "Invalid bool literal: Expected type {:?}, found {:?}",
                        local_ctx.result_type,
                        self.ctx.get_type("bool".into())?
                    ))
                }
            }
        }
    }

    fn get_next_struct_id(&mut self) -> usize {
        let id = self.next_struct_id;
        self.next_struct_id += 1;
        id
    }

    fn typecheck_struct_decl(
        &mut self,
        structure: &StructDeclaration,
    ) -> Result<TypedStructDeclaration<'tc>, String> {
        debug!("typechecker::typecheck_struct_decl");
        let id = self.get_next_struct_id();
        self.ctx.add_type(
            structure.name.clone(),
            new_type(Type::new(TypeSignature::Struct(id))),
        )?;
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

        let mut struct_type = StructType {
            name: structure.name.clone(),
            fields: fields.clone(),
            methods: HashMap::new(),
            id,
        };

        let Ok(type_ref) = self.ctx.get_type(structure.name.clone()) else {
            return Err("".into())
        };
        //*type_ref.borrow_mut() = Type::new(TypeSignature::Struct(id));

        for method in &structure.methods {
            // function def
            let func = self
                .typecheck_fn_def(method, LocalTypecheckContext::impl_method(type_ref.clone()))?;
            struct_type.methods.insert(method.name.clone(), func);
        }

        self.ctx.struct_types.insert(id, struct_type.clone());
        //self.ctx.struct_types.insert(id, struct_type.clone());

        Ok(TypedStructDeclaration {
            name: structure.name.clone(),
            fields: fields.values().map(|field| field.clone()).collect(),
            ty: type_ref,
            id,
            methods: struct_type.methods.clone(),
        })
    }

    fn typecheck_binary_op(
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
                has_self_param: false,
                name: function.name.clone(),
            }),
        })
    }

    fn resolve_fn_def_type(
        &self,
        function: &FunctionDefinition,
    ) -> Result<type_sig::Type<'tc>, String> {
        let mut params = HashMap::new();
        let mut has_self_param = false;
        for param in &function.params {
            let t = self.ctx.get_type(param.type_name.clone())?;
            if param.name == "self" {
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
            }),
        })
    }

    fn typecheck_fn_call(
        &self,
        fn_call: &ast::FunctionCall,
        mut local_ctx: LocalTypecheckContext<'tc>,
    ) -> Result<TypedExpression<'tc>, String> {
        debug!("typechecker::typecheck_fn_call");

        let (fn_type, fn_name, member_self_type) = match *fn_call.callee.clone() {
            Expression::Identifier { name, .. } => (
                local_ctx
                    .names
                    .get(&name)
                    .ok_or(format!("Function {} is not defined in this scope", name))?,
                name,
                None,
            ),
            Expression::MemberAccess { member_access } => {
                debugln!();
                let object = self.typecheck_expression(&member_access.object, local_ctx.clone())?;

                let Some(object_type) = &object.ty else {
                    return Err(format!("Cannot access member of non-existant type"));
                };
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

                let Expression::Identifier { name: member_name, span: _ } = *member_access.member else {
                    return Err(format!("Member access must be an identifier"));
                };

                let member_method = struct_ty.methods.get(&member_name).ok_or(format!(
                    "Struct {} does not have a method {}",
                    struct_ty.name, member_name
                ))?;

                let member_fn_ty = member_method.fn_ty.clone();
                let full_name = struct_ty.name.clone() + "." + &member_name;
                local_ctx
                    .names
                    .insert(full_name.clone(), Name { ty: member_fn_ty });
                let name = local_ctx.names.get(&full_name).unwrap();
                (name, full_name, Some(object))
            }
            Expression::ScopeResolution { scope_resolution } => {
                let ScopeResolution {
                    object,
                    member,
                    span,
                } = scope_resolution;
                debugln!();
                let mut object = *object;
                let mut path = Vec::new();
                while let Expression::ScopeResolution { scope_resolution } = object {
                    let ScopeResolution {
                        object: new_object,
                        member,
                        span,
                    } = scope_resolution;
                    if let Expression::Identifier { name, .. } = *member {
                        path.push(name);
                    } else {
                        return Err(format!("Unsupported scope resolution member"));
                    }
                    object = *new_object;
                }
                if let Expression::Identifier { name, .. } = object {
                    path.push(name);
                } else {
                    return Err(format!("Unsupported scope resolution object"));
                }

                let Expression::Identifier { name: fn_name, .. } = *member else {
                    return Err(format!("Unsupported scope resolution member"));
                };

                let first = path.first().unwrap();
                let rest = if path.len() > 2 {
                    Some(&path[1..path.len() - 1])
                } else {
                    None
                };

                // Resolve type of object
                if let Ok(object_type) = self.ctx.get_type(first.clone()) {
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

                    let member_method = struct_ty.methods.get(&fn_name).ok_or(format!(
                        "Struct {} does not have a method {}",
                        struct_ty.name, fn_name
                    ))?;

                    let member_fn_ty = member_method.fn_ty.clone();
                    let full_name = struct_ty.name.clone() + "." + &fn_name;
                    local_ctx
                        .names
                        .insert(full_name.clone(), Name { ty: member_fn_ty });
                    let name = local_ctx.names.get(&full_name).unwrap();
                    (name, full_name, None)
                } else {
                    // It's a module
                    todo!()
                }
            }
            _ => return Err(format!("Unsupported callee type {:?}", fn_call.callee)),
        };

        let ret_type = match fn_type.ty.sig() {
            TypeSignature::Function(FunctionType { return_type, .. }) => return_type,
            _ => return Err(format!("{:?} is not a function", fn_type.ty.sig())),
        };
        let mut args = Vec::new();
        if let Some(t) = member_self_type {
            args.push(t);
        }
        for (arg_idx, arg) in fn_call.args.iter().enumerate() {
            let param_type = match fn_type.ty.sig() {
                TypeSignature::Function(FunctionType {
                    params,
                    variadic,
                    has_self_param,
                    ..
                }) => {
                    let t = match params
                        .iter()
                        .find(|(_, p)| p.idx as usize == (arg_idx + (has_self_param as usize)))
                    {
                        Some((_, t)) => Some(t.ty.clone()),
                        None => {
                            if arg_idx > (params.len() - 1) {
                                if variadic {
                                    self.typecheck_expression(
                                        &arg.clone(),
                                        local_ctx.expect_result(None),
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
            args.push(self.typecheck_expression(arg, local_ctx.expect_result(param_type))?);
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

    fn typecheck_unary_op(
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

    fn typecheck_struct_init(
        &self,
        struct_init: &StructInitializer,
        mut local_ctx: LocalTypecheckContext<'tc>,
    ) -> Result<TypedExpression<'tc>, String> {
        debug!("typechecker::typecheck_struct_init");
        let StructInitializer {
            struct_name,
            fields,
            ..
        } = struct_init;
        let struct_type = self.ctx.get_type(struct_name.clone())?;
        let struct_sig = struct_type.sig();

        let decl_fields = match &struct_sig {
            TypeSignature::Struct(id) => match self.ctx.struct_types.get(id) {
                Some(s) => s.fields.clone(),
                None => return Err(format!("Struct {} is not defined", struct_name)),
            },
            _ => unreachable!(),
        };

        let mut typed_fields = Vec::new();
        for field in fields {
            let StructInitializerField {
                field_name, value, ..
            } = field;

            let expected_field = decl_fields.get(field_name).ok_or(format!(
                "Field {} does not exist in struct {}",
                field_name, struct_name
            ))?;
            // TODO: Possible issue
            local_ctx.result_type = Some(expected_field.ty.clone());

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
        &self,
        member_access: &ast::MemberAccess,
        local_ctx: LocalTypecheckContext<'tc>,
    ) -> Result<TypedExpression<'tc>, String> {
        debug!("typechecker::typecheck_member_access");
        let MemberAccess {
            object,
            member,
            computed,
            ..
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

        let Some(ty) = &checked_object.ty else {
            return Err(format!("Cannot dereference {:?}", checked_object));
        };
        let sig = ty.sig();

        if let TypeSignature::Struct(id) = sig {
            debugln!();
            let structure = self
                .ctx
                .struct_types
                .get(&id)
                .ok_or(format!("Struct {} is not defined", id))?;
            let mut struct_ctx = LocalTypecheckContext {
                names: HashMap::new(),
                result_type: local_ctx.result_type.clone(),
                return_type: local_ctx.return_type.clone(),
                in_loop: local_ctx.in_loop,
                impl_ctx: None,
            };

            // add struct fields to internal context
            for (field_name, field_type) in &structure.fields {
                struct_ctx.names.insert(
                    field_name.clone(),
                    Name {
                        ty: field_type.ty.clone(),
                    },
                );
            }

            // add struct methods to internal context
            for (method_name, method_type) in &structure.methods {
                struct_ctx.names.insert(
                    method_name.clone(),
                    Name {
                        ty: method_type.fn_ty.clone(),
                    },
                );
            }

            let checked_member = self.typecheck_expression(&member, struct_ctx)?;
            Ok(TypedExpression {
                ty: checked_member.ty.clone(),
                expr: TypedExpressionData::MemberAccess {
                    member_access: TypedMemberAccess {
                        object: Box::from(checked_object),
                        member: Box::from(checked_member),
                        computed: *computed,
                    },
                },
            })
        } else if let TypeSignature::Pointer(pointer) = &sig {
            if let TypeSignature::Struct(id) = pointer.target.as_ref() {
                let mut struct_ctx = LocalTypecheckContext {
                    names: HashMap::new(),
                    result_type: local_ctx.result_type.clone(),
                    return_type: local_ctx.return_type.clone(),
                    in_loop: local_ctx.in_loop,
                    impl_ctx: None,
                };

                debugln!();
                let s = self
                    .ctx
                    .struct_types
                    .get(id)
                    .ok_or(format!("Struct {} is not defined", id))?;

                // add struct fields to internal context
                for (field_name, field_type) in &s.fields {
                    struct_ctx.names.insert(
                        field_name.clone(),
                        Name {
                            ty: field_type.ty.clone(),
                        },
                    );
                }

                // add struct methods to internal context
                for (method_name, method_type) in &s.methods {
                    struct_ctx.names.insert(
                        method_name.clone(),
                        Name {
                            ty: method_type.fn_ty.clone(),
                        },
                    );
                }

                let checked_member = self.typecheck_expression(&member, struct_ctx)?;
                let Some(ty) = checked_member.ty.clone() else {
                    return Err(format!("Cannot access member of {:?}", sig));
                };
                Ok(TypedExpression {
                    ty: Some(new_type(Type {
                        sig: ty.sig().wrap_in_ptr(),
                    })),
                    expr: TypedExpressionData::MemberAccess {
                        member_access: TypedMemberAccess {
                            object: Box::from(checked_object),
                            member: Box::from(checked_member),
                            computed: *computed,
                        },
                    },
                })
            } else {
                return Err(format!("Cannot access member of {:?}", sig));
            }
        } else {
            return Err(format!("Cannot access member of {:?}", sig));
        }
    }
}
