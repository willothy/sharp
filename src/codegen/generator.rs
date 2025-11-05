use std::{borrow::BorrowMut, cell::RefCell, collections::HashMap, error::Error, rc::Rc};

use inkwell::{
    module::Linkage,
    types::BasicType,
    values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, PointerValue},
    AddressSpace,
};

use crate::{
    ast::ModulePath,
    codegen::context::ImplCtx,
    debug,
    tokenizer::Literal,
    typechecker::{
        typed_ast::{
            TypedExportType, TypedExpression, TypedExpressionData, TypedFunctionCall, TypedImport,
            TypedLiteral, TypedLoopStatement, TypedModule, TypedVarAssignment,
        },
        TCModule, TypeCheckerOutput,
    },
};
use crate::{
    tokenizer::Operator,
    typechecker::{
        context::TypeSig,
        type_sig::TypeSignature,
        typed_ast::{
            TypedFunctionDeclaration, TypedFunctionDefinition, TypedIfExpression,
            TypedMemberAccess, TypedResultStatement, TypedReturnStatement, TypedStatement,
            TypedStructDeclaration, TypedStructInitializer, TypedVarDeclaration,
        },
    },
};

use super::context::{CodegenContext, CodegenName, CodegenType, LocalCodegenContext, LoopContext};

pub struct CodeGenerator<'gen> {
    pub ctx: CodegenContext<'gen>,
}

impl<'gen> CodeGenerator<'gen> {
    pub fn new(tc_mod: TypeCheckerOutput<'gen>, llvm_ctx: &'gen inkwell::context::Context) -> Self {
        debug!("generator::CodeGenerator::new");
        let module = llvm_ctx.create_module("main");
        let builder = llvm_ctx.create_builder();
        Self {
            ctx: CodegenContext::new(llvm_ctx, module, builder, tc_mod),
        }
    }

    pub fn codegen(&mut self) -> Result<inkwell::module::Module<'gen>, Box<dyn std::error::Error>> {
        debug!("generator::CodeGenerator::codegen");

        let tc_modules = self.ctx.tc_mod.modules.clone();

        let mut generated_modules = Vec::new();
        let mut main_mod_idx = None;
        for module in tc_modules {
            self.ctx.llvm_module = self
                .ctx
                .llvm_ctx
                .create_module(module.get_path().join("::").as_str());
            if module.borrow().path.join("::") == "main" {
                main_mod_idx = Some(generated_modules.len());
            }
            self.codegen_module(module.clone())?;
            generated_modules.push(self.ctx.llvm_module.clone());
        }

        let main_module = generated_modules.remove(main_mod_idx.unwrap());
        for module in generated_modules {
            main_module.link_in_module(module)?;
        }

        Ok(main_module)
    }

    pub fn canonicalize_fn_name(&self, name: &str, source_module: &ModulePath) -> String {
        let path = source_module.join("::");
        if path == "main" {
            name.to_string()
        } else {
            path + "::" + name
        }
    }

    pub fn canonicalize_struct_name(&self, name: &str, source_module: &ModulePath) -> String {
        let path = source_module.join("::");
        if path == "main" {
            name.to_string()
        } else {
            path + "::" + name
        }
    }

    pub fn canonicalize_struct_member_name(
        &self,
        name: &str,
        source_module: &ModulePath,
        self_name: &str,
        has_self_param: bool,
    ) -> String {
        let path = source_module.join("::");
        let final_sep = if has_self_param { "." } else { "::" };
        if path == "main" {
            self_name.to_string() + final_sep + name
        } else {
            path + "::" + self_name + final_sep + name
        }
    }

    pub fn codegen_module(
        &mut self,
        module: Rc<RefCell<TypedModule<'gen>>>,
    ) -> Result<(), Box<dyn std::error::Error>> {
        debug!("generator::CodeGenerator::codegen_module");
        let mut local_ctx = LocalCodegenContext {
            names: HashMap::new(),
            types: HashMap::new(),
            return_type: None,
            result_type: None,
            current_fn: None,
            structs: HashMap::new(),
            loop_ctx: None,
            impl_ctx: None,
            is_self_arg: false,
        };

        self.ctx.add_primitive_types(&mut local_ctx);
        local_ctx.structs = module.get_ctx().struct_types.clone();

        for dep in &module.borrow().dependencies {
            let TypedImport {
                name,
                source_module,
                local: _,
                ty: _,
            } = dep;
            let Some(source_mod) = self.ctx.tc_mod.modules.get(*source_module) else {
                return Err(format!("Failed to generate IR for module: Module not found: {}", name).into());
            };
            let source_mod = source_mod.borrow();

            let Some(source_item) = source_mod.exports.get(name) else {
                return Err(format!("source_item: Export not found: {}", name).into());
            };

            match &source_item.ty {
                TypedExportType::Function(f) => {
                    let no_mangle = f
                        .attrs
                        .iter()
                        .find(|a| a.val.as_str() == "no-mangle")
                        .is_some();
                    let full_name = if no_mangle {
                        name.clone()
                    } else {
                        self.canonicalize_fn_name(name, &source_mod.path)
                    };
                    local_ctx.add_name(
                        name.clone(),
                        CodegenName {
                            name: full_name.clone(),
                            ty: Rc::new(CodegenType::new(
                                full_name,
                                TypeSignature::Function(f.clone()),
                                Some(self.ctx.function_to_llvm_ty(&f, &local_ctx.structs)?),
                            )),
                            alloc: None,
                            is_arg: false,
                            arg_idx: None,
                        },
                    );
                }
                TypedExportType::Struct(id) => {
                    // get struct
                    let Some(structure) = source_mod.ctx.struct_types.get(id) else {
                        return Err(format!("Failed to generate IR for module: Struct not found: {}", name).into());
                    };
                    local_ctx.structs.insert(*id, structure.clone());
                    local_ctx.add_type(CodegenType::new(
                        self.canonicalize_struct_name(&structure.name, &source_mod.path),
                        TypeSignature::Struct(*id),
                        Some(
                            self.ctx
                                .to_llvm_ty(&TypeSignature::Struct(*id), &local_ctx.structs)?,
                        ),
                    ));
                }
                TypedExportType::Module(_) => todo!(),
            }
        }

        // Codegen structs
        for structure in &module.borrow().structs {
            self.codegen_struct(structure.clone(), module.clone(), &mut local_ctx)?;
        }

        for def in &module.borrow().fn_defs {
            let t = def.fn_ty.sig();
            let t = if let TypeSignature::Function(fn_t) = t.clone() {
                let no_mangle = fn_t
                    .attrs
                    .iter()
                    .find(|a| a.val.as_str() == "no-mangle")
                    .is_some();
                let fn_name = if no_mangle {
                    def.name.clone()
                } else {
                    self.canonicalize_fn_name(&def.name, &module.get_path())
                };
                Rc::from(CodegenType::new(
                    fn_name,
                    t,
                    Some(self.ctx.function_to_llvm_ty(&fn_t, &local_ctx.structs)?),
                ))
            } else {
                let Some(t) = local_ctx.types.get(&t) else {
                    return Err(format!("Type not found: {:?} {}:{}", t, file!(), line!()).into());
                };
                t.clone()
            };

            let name = CodegenName::new(def.name.clone(), t.clone());
            local_ctx.add_name(name.name.clone(), name);
        }

        for decl in &module.borrow().fn_decls {
            let t = decl.fn_ty.sig();
            let t = if let TypeSignature::Function(fn_t) = t.clone() {
                let no_mangle = fn_t
                    .attrs
                    .iter()
                    .find(|a| a.val.as_str() == "no-mangle")
                    .is_some();
                let fn_name = if no_mangle {
                    decl.name.clone()
                } else {
                    self.canonicalize_fn_name(&decl.name, &module.get_path())
                };
                Rc::from(CodegenType::new(
                    fn_name,
                    t,
                    Some(self.ctx.function_to_llvm_ty(&fn_t, &local_ctx.structs)?),
                ))
            } else {
                let Some(t) = local_ctx.types.get(&t) else {
                    return Err(format!("Type not found: {:?} {}:{}", t, file!(), line!()).into());
                };
                t.clone()
            };
            let name = CodegenName::new(decl.name.clone(), t.clone());
            local_ctx.add_name(name.name.clone(), name);
        }

        // Codegen function declarations
        for decl in &module.borrow().fn_decls {
            self.codegen_fn_decl(decl.clone(), local_ctx.clone())?;
        }

        for def in &module.borrow().fn_defs {
            self.codegen_fn_def(def.clone(), module.clone(), local_ctx.clone())?;
        }

        /* for (_, submodule_id) in &module.borrow().submodules {
            let Some(submodule) = self.ctx.tc_mod.modules.get(submodule_id.clone()) else {
                return Err(format!("Module not found: {:?} {}:{}", submodule_id, file!(), line!()).into());
            };
            let submod = self.codegen_module(submodule.clone())?;
            self.ctx.llvm_module.link_in_module(submod)?;
        } */

        Ok(())
    }

    fn codegen_struct(
        &self,
        structure: TypedStructDeclaration<'gen>,
        mod_ref: Rc<RefCell<TypedModule<'gen>>>,
        local_ctx: &mut LocalCodegenContext<'gen>,
    ) -> Result<(), Box<dyn std::error::Error>> {
        debug!("generator::CodeGenerator::codegen_struct");
        let TypedStructDeclaration { name, ty, .. } = structure;
        let struct_t = self.ctx.to_llvm_ty(&ty.sig(), &local_ctx.structs)?;

        local_ctx.types.insert(
            ty.sig(),
            Rc::new(CodegenType {
                name: self.canonicalize_struct_name(&name, &mod_ref.get_path()),
                ty: ty.sig(),
                llvm_ty: Some(struct_t),
            }),
        );
        let Some(struct_t) = local_ctx.types.get(&ty.sig()) else {
            return Err(format!("Type not found: {:?} {}:{}", ty.sig(), file!(), line!()).into());
        };
        let struct_name = struct_t.name.clone();

        let Some(struct_id) = ty.sig().struct_id() else {
            return Err(format!("Type not found: {:?} {}:{}", ty.sig(), file!(), line!()).into());
        };

        let Some(methods_t) = local_ctx.structs.get(&struct_id) else {
            return Err(format!("Type not found: {:?} {}:{}", ty.sig(), file!(), line!()).into());
        };
        let methods = methods_t.methods.clone();

        for (_, method) in &methods {
            let TypeSignature::Function(func_ty) = method.fn_ty.sig() else {
                return Err(format!("Type not found: {:?} {}:{}", method.fn_ty.sig(), file!(), line!()).into());
            };
            let full_name = self.canonicalize_struct_member_name(
                &method.name,
                &mod_ref.get_path(),
                &struct_name,
                method.has_self_param,
            );
            let mut method_ctx = local_ctx.with_impl(
                Some(ImplCtx {
                    self_type: ty.sig(),
                    self_name: struct_name.clone(),
                }),
                method.has_self_param,
            )?;
            method_ctx.add_name(
                full_name.clone(),
                CodegenName::new(
                    full_name.clone(),
                    Rc::new(CodegenType {
                        name: full_name,
                        ty: method.fn_ty.sig(),
                        llvm_ty: Some(self.ctx.function_to_llvm_ty(&func_ty, &local_ctx.structs)?),
                    }),
                ),
            );
            self.codegen_fn_def(method.clone(), mod_ref.clone(), method_ctx)?;
        }

        Ok(())
    }

    fn codegen_fn_decl(
        &self,
        func: TypedFunctionDeclaration<'gen>,
        local_ctx: LocalCodegenContext<'gen>,
    ) -> Result<(), Box<dyn Error>> {
        debug!("generator::CodeGenerator::codegen_fn_decl");
        let Some(func_reg) = local_ctx.names.get(&func.name) else {
            return Err(format!("Function not found: {} ({})", func.name, line!()).into());
        };

        let Some(llvm_ty) = func_reg.ty.llvm_ty.clone() else {
            return Err(format!("Function not found: {} ({})", func.name, line!()).into());
        };

        let _llvm_fn = self.ctx.llvm_module.add_function(
            &func.name.clone(),
            llvm_ty.function()?,
            Some(Linkage::External),
        );

        Ok(())
    }

    fn codegen_fn_def(
        &self,
        func: TypedFunctionDefinition<'gen>,
        mod_ref: Rc<RefCell<TypedModule<'gen>>>,
        mut local_ctx: LocalCodegenContext<'gen>,
    ) -> Result<(), Box<dyn std::error::Error>> {
        debug!("generator::CodeGenerator::codegen_fn_def");
        let source_mod_path = mod_ref.get_path().clone();
        let func_name = if let Some(impl_ctx) = &local_ctx.impl_ctx {
            self.canonicalize_struct_member_name(
                &func.name,
                &source_mod_path,
                &impl_ctx.self_name,
                func.has_self_param,
            )
        } else {
            self.canonicalize_fn_name(&func.name, &source_mod_path)
        };
        let func_reg = if let Some(func_reg) = local_ctx.names.get(&func_name) {
            func_reg
        } else if let Some(func_reg) = local_ctx.names.get(&func.name) {
            func_reg
        } else {
            return Err(format!("fn_def: Function not found: {} ({})", func_name, line!()).into());
        };

        let Some(llvm_ty) = func_reg.ty.llvm_ty.clone() else {
            return Err(format!("fn_def: Function type not found: {} ({})", func_name, line!()).into());
        };

        let llvm_fn = if let Some(func) = self.ctx.llvm_module.get_function(&func_name) {
            func
        } else {
            self.ctx.llvm_module.add_function(
                &func_name,
                llvm_ty.function()?,
                Some(Linkage::External),
            )
        };

        for param in func.params.values() {
            let type_sig = param.ty.sig();
            let Some(param_type) = local_ctx.get_base_type(&type_sig) else {
                return Err(format!("Type not found: {:?} {}:{}", type_sig, file!(), line!()).into());
            };
            let param_name =
                CodegenName::new_arg(param.name.clone(), param_type.clone(), param.idx as u32);
            llvm_fn
                .get_params()
                .iter_mut()
                .nth(param.idx as usize)
                .ok_or(format!("Param not found: {}", param.name))?
                .set_name(&param_name.name);
            local_ctx.add_name(param_name.name.clone(), param_name);
        }

        let entry_block = self.ctx.llvm_ctx.append_basic_block(llvm_fn, "entry");
        self.ctx.ir_builder.position_at_end(entry_block);

        if let Some(ret_type) = &func.ret_ty {
            let type_sig = ret_type.sig();
            let Some(ret_type) = local_ctx.get_base_type(&type_sig) else {
                return Err(format!("Type not found: {:?} {}:{}", func.ret_ty, file!(), line!()).into());
            };
            local_ctx.return_type = Some(ret_type.ty.clone());
        }

        local_ctx.current_fn = Some(llvm_fn);
        let mut did_break = false;
        for stmt in func.body.statements {
            self.codegen_statement(stmt, &mut local_ctx, &mut did_break)?;
        }

        Ok(())
    }

    fn codegen_statement(
        &self,
        stmt: TypedStatement<'gen>,
        mut local_ctx: &mut LocalCodegenContext<'gen>,
        did_break: &mut bool,
    ) -> Result<Option<BasicValueEnum<'gen>>, Box<dyn std::error::Error>> {
        debug!("generator::CodeGenerator::codegen_statement");
        match stmt {
            TypedStatement::Variable(var_stmt) => {
                self.codegen_variable_statement(var_stmt, &mut local_ctx, did_break)?;
                Ok(None)
            }
            TypedStatement::Expression(expr_stmt) => {
                let res = self.codegen_expression(expr_stmt, local_ctx.clone(), did_break)?;
                Ok(res)
            }
            TypedStatement::Loop(s) => {
                self.codegen_loop_statement(s, local_ctx.clone(), did_break)?;
                Ok(None)
            }
            TypedStatement::Return(ret_stmt) => {
                self.codegen_return_statement(ret_stmt, local_ctx.clone(), did_break)?;
                Ok(None)
            }
            TypedStatement::Result(result_stmt) => Ok(Some(
                self.codegen_result_statement(result_stmt, local_ctx.clone())?,
            )),
            TypedStatement::Continue => {
                self.codegen_continue_statement(local_ctx.clone())?;
                *did_break = true;
                Ok(None)
            }
            TypedStatement::Break => {
                self.codegen_break_statement(local_ctx.clone())?;
                *did_break = true;
                Ok(None)
            }
        }
    }

    pub(crate) fn codegen_continue_statement(
        &self,
        local_ctx: LocalCodegenContext<'gen>,
    ) -> Result<(), Box<dyn Error>> {
        let Some(loop_ctx) = local_ctx.loop_ctx else {
            return Err(format!("No loop context: {}:{}", file!(), line!()).into());
        };
        self.ctx
            .ir_builder
            .build_unconditional_branch(loop_ctx.body_block);
        Ok(())
    }

    pub(crate) fn codegen_break_statement(
        &self,
        local_ctx: LocalCodegenContext<'gen>,
    ) -> Result<(), Box<dyn Error>> {
        let Some(loop_ctx) = local_ctx.loop_ctx else {
            return Err(format!("No loop context: {}:{}", file!(), line!()).into());
        };
        self.ctx
            .ir_builder
            .build_unconditional_branch(loop_ctx.exit_block);
        Ok(())
    }

    pub(crate) fn codegen_loop_statement(
        &self,
        s: TypedLoopStatement<'gen>,
        mut local_ctx: LocalCodegenContext<'gen>,
        did_break: &mut bool,
    ) -> Result<(), Box<dyn Error>> {
        // Basic blocks:
        // 1. entry
        // 2. loop body
        // 3. exit
        let Some(current_fn) = local_ctx.current_fn else {
            return Err(format!("No current function: {}:{}", file!(), line!()).into());
        };
        let entry_block = self
            .ctx
            .llvm_ctx
            .append_basic_block(current_fn, "loop_entry");

        let body_block = self
            .ctx
            .llvm_ctx
            .append_basic_block(current_fn, "loop_body");
        let exit_block = self
            .ctx
            .llvm_ctx
            .append_basic_block(current_fn, "loop_exit");

        // Set loop context
        local_ctx.loop_ctx = Some(LoopContext {
            body_block,
            exit_block,
        });

        // Jump from current block to entry block
        self.ctx.ir_builder.build_unconditional_branch(entry_block);
        self.ctx.ir_builder.position_at_end(entry_block);

        // Jump to loop body
        self.ctx.ir_builder.build_unconditional_branch(body_block);

        // Loop body
        self.ctx.ir_builder.position_at_end(body_block);
        for stmt in s.body.statements {
            self.codegen_statement(stmt, &mut local_ctx.clone(), did_break)?;
        }
        self.ctx.ir_builder.build_unconditional_branch(body_block);

        self.ctx.ir_builder.position_at_end(exit_block);

        Ok(())
    }

    fn codegen_variable_statement(
        &self,
        var_stmt: TypedVarDeclaration<'gen>,
        local_ctx: &mut LocalCodegenContext<'gen>,
        did_break: &mut bool,
    ) -> Result<(), Box<dyn Error>> {
        debug!("generator::CodeGenerator::codegen_variable_statement");
        let type_sig = var_stmt.ty.sig();

        let Some(base_type) = local_ctx.get_base_type(&type_sig) else {
            return Err(format!("Base Type not found: {:?} {}:{}", var_stmt.ty, file!(), line!()).into());
        };

        let _base_llvm_type = self.ctx.to_llvm_ty(&type_sig, &local_ctx.structs)?;

        let ptr_depth = local_ctx.get_ptr_depth(&type_sig);

        let var_type = if ptr_depth > 0 {
            Rc::new(CodegenType {
                name: type_sig.string_repr(&local_ctx.structs),
                ty: type_sig.clone(),
                llvm_ty: Some(self.ctx.to_llvm_ty(&type_sig, &local_ctx.structs)?),
            })
        } else {
            base_type
        };

        let mut var_name = CodegenName::new(var_stmt.name.clone(), var_type.clone());

        let Some(llvm_ty) = &var_type.llvm_ty else {
            return Err(format!("Type not found: {:?} {}:{}", var_type, file!(), line!()).into());
        };

        let store_ptr = self
            .ctx
            .ir_builder
            .build_alloca(llvm_ty.basic()?, &var_stmt.name);
        // Add store ptr to local context
        if let Some(init) = var_stmt.initializer {
            let init_val = self.codegen_expression(init, local_ctx.clone(), did_break)?;
            let Some(init_val) = init_val else {
                return Err("Expected initializer".into());
            };

            self.ctx
                .ir_builder
                .build_store(store_ptr /* .into_pointer_value() */, init_val);
        }

        var_name.alloc = Some(store_ptr.clone());
        local_ctx.add_name(var_name.name.clone(), var_name);

        Ok(())
    }

    fn codegen_expression(
        &self,
        init: TypedExpression<'gen>,
        local_ctx: LocalCodegenContext<'gen>,
        did_break: &mut bool,
    ) -> Result<Option<BasicValueEnum<'gen>>, Box<dyn Error>> {
        debug!(format!(
            "generator::CodeGenerator::codegen_expression: {:?} in {:?}",
            init.expr.variant_str(),
            local_ctx.current_fn.unwrap().get_name()
        ));
        let res = match init {
            TypedExpression {
                expr: TypedExpressionData::BinaryOp { left, right, op },
                ..
            } => Some(self.codegen_binary_op(*left, *right, &op, local_ctx)?),
            TypedExpression {
                expr: TypedExpressionData::AsExpr { expr, ty },
                ..
            } => {
                let Some(expr) = self.codegen_expression(*expr, local_ctx.clone(), did_break)? else {
                    return Err("Expected value from expression".into());
                };
                let as_ty = self
                    .ctx
                    .to_llvm_ty(&ty.sig(), &local_ctx.structs)?
                    .basic()?;

                let res = match &expr {
                    BasicValueEnum::ArrayValue(_) => todo!(),
                    BasicValueEnum::IntValue(i) => self.build_int_cast(i.clone(), as_ty)?,
                    BasicValueEnum::FloatValue(f) => self.build_float_cast(f.clone(), as_ty)?,
                    BasicValueEnum::PointerValue(p) => self.build_ptr_cast(p.clone(), as_ty)?,
                    BasicValueEnum::StructValue(_) => todo!(),
                    BasicValueEnum::VectorValue(_) => todo!(),
                };
                Some(res)
            }
            TypedExpression {
                expr: TypedExpressionData::LogicalOp { left, right, op },
                ..
            } => {
                let Some(left) = self.codegen_expression(*left, local_ctx.clone(), did_break)? else {
                    return Err("Expected value from expression".into());
                };
                let Some(right) = self.codegen_expression(*right, local_ctx.clone(), did_break)? else {
                    return Err("Expected value from expression".into());
                };

                let res = match op {
                    Operator::And => self.build_and(left, right),
                    Operator::Or => self.build_or(left, right),
                    _ => todo!("Logical operator not implemented: {:?}", op),
                };

                Some(res)
            }
            TypedExpression {
                expr: TypedExpressionData::UnaryOp { expr, op },
                ..
            } => match op {
                Operator::Plus => self.codegen_expression(*expr, local_ctx.clone(), did_break)?,
                Operator::Minus => {
                    // Negate

                    let Some(expr) = self.codegen_expression(*expr, local_ctx.clone(), did_break)? else {
                        return Err("Expected value from expression".into());
                    };

                    let neg = match expr {
                        BasicValueEnum::ArrayValue(_) => unimplemented!(),
                        BasicValueEnum::IntValue(i) => self
                            .ctx
                            .ir_builder
                            .build_int_neg(i, "neg")
                            .as_basic_value_enum(),
                        BasicValueEnum::FloatValue(f) => self
                            .ctx
                            .ir_builder
                            .build_float_neg(f, "neg")
                            .as_basic_value_enum(),
                        BasicValueEnum::PointerValue(_) => unimplemented!(),
                        BasicValueEnum::StructValue(_) => unimplemented!(),
                        BasicValueEnum::VectorValue(_) => unimplemented!(),
                    };

                    Some(neg)
                }
                Operator::Times => {
                    // Dereference

                    let Some(expr_value) = self.codegen_expression(*expr.clone(), local_ctx.clone(), did_break)? else {
                        return Err("Expected value from expression".into());
                    };

                    let deref_load = self.ctx.ir_builder.build_load(
                        expr_value.into_pointer_value(),
                        (expr
                            .ty
                            .clone()
                            .unwrap()
                            .sig()
                            .string_repr(&local_ctx.structs)
                            + "_deref")
                            .as_str(),
                    );

                    Some(deref_load)
                }
                Operator::Not => {
                    // boolean not
                    let Some(expr) = self.codegen_expression(*expr, local_ctx.clone(), did_break)? else {
                        return Err("Expected value from expression".into());
                    };

                    let not = self.ctx.ir_builder.build_not(expr.into_int_value(), "not");

                    Some(not.as_basic_value_enum())
                }
                _ => return Err(format!("Invalid unary operator: {:?}", op).into()),
            },

            TypedExpression {
                expr: TypedExpressionData::Identifier { name },
                ..
            } => Some(self.codegen_identifier(&name, local_ctx)?),
            TypedExpression {
                expr: TypedExpressionData::Literal { literal },
                ..
            } => Some(self.codegen_literal(literal, local_ctx)?),
            TypedExpression {
                expr: TypedExpressionData::If { expr },
                ..
            } => {
                let TypedIfExpression {
                    condition,
                    body,
                    else_body,
                    ..
                } = expr;

                let Some(condition) = self.codegen_expression(*condition, local_ctx.clone(), did_break)? else {
                    return Err("Expected value from expression".into());
                };

                let Some(func) = local_ctx.current_fn else {
                    return Err("Expected function".into());
                };

                let entry_block = self.ctx.ir_builder.get_insert_block().unwrap();

                let then_block = self.ctx.llvm_ctx.append_basic_block(func, "then");

                let mut else_block = self.ctx.llvm_ctx.append_basic_block(func, "else");

                let end_block = self.ctx.llvm_ctx.append_basic_block(func, "end");

                self.ctx.ir_builder.position_at_end(entry_block);
                self.ctx.ir_builder.build_conditional_branch(
                    condition.into_int_value(),
                    then_block,
                    else_block,
                );

                self.ctx.ir_builder.position_at_end(then_block);
                let then_result =
                    self.codegen_expression(*body.clone(), local_ctx.clone(), did_break)?;
                if !*did_break {
                    self.ctx.ir_builder.build_unconditional_branch(end_block);
                }
                *did_break = false;
                let then_block = self.ctx.ir_builder.get_insert_block().unwrap();

                self.ctx.ir_builder.position_at_end(else_block);

                let else_result = if let Some(else_body) = *else_body {
                    let else_result =
                        self.codegen_expression(else_body, local_ctx.clone(), did_break)?;
                    if !*did_break {
                        self.ctx.ir_builder.build_unconditional_branch(end_block);
                    }
                    *did_break = false;
                    else_block = self.ctx.ir_builder.get_insert_block().unwrap();
                    else_result
                } else {
                    self.ctx.ir_builder.build_unconditional_branch(end_block);
                    None
                };

                self.ctx.ir_builder.position_at_end(end_block);
                let res = if let Some(ty) = body.ty.clone() {
                    let phi = self.ctx.ir_builder.build_phi(
                        self.ctx
                            .to_llvm_ty(&ty.sig(), &local_ctx.structs)?
                            .basic()?,
                        "if_result",
                    );
                    phi.add_incoming(&[
                        (&then_result.unwrap(), then_block),
                        (&else_result.unwrap(), else_block),
                    ]);
                    Some(phi.as_basic_value())
                } else {
                    None
                };

                self.ctx.ir_builder.position_at_end(end_block);

                res
            }
            TypedExpression {
                expr: TypedExpressionData::Block { block },
                ..
            } => {
                let mut res = None;
                let mut block_scope = local_ctx.clone();
                for statement in block.statements {
                    res = self.codegen_statement(statement, &mut block_scope, did_break)?;
                }
                res
            }
            TypedExpression {
                expr: TypedExpressionData::VarAssignment { var_assign },
                ..
            } => Some(self.codegen_var_assignment(var_assign, local_ctx)?),
            TypedExpression {
                expr: TypedExpressionData::FnCall { fn_call },
                ..
            } => self.codegen_fn_call(fn_call, local_ctx)?,
            TypedExpression {
                expr: TypedExpressionData::MemberAccess { member_access },
                ..
            } => Some(self.codegen_member_access(member_access, local_ctx)?),
            TypedExpression {
                expr: TypedExpressionData::StructInitializer { struct_init },
                ..
            } => Some(self.codegen_struct_init(struct_init, local_ctx)?),
            TypedExpression {
                expr: TypedExpressionData::SizeOfExpr { ty },
                ..
            } => {
                let Some(size) = self.ctx.to_llvm_ty(&ty.sig(), &local_ctx.structs)?.basic()?.size_of() else {
                    return Err("Expected size".into());
                };
                Some(size.as_basic_value_enum())
            }
        };
        Ok(res)
    }

    fn codegen_literal(
        &self,
        literal: TypedLiteral<'gen>,
        local_ctx: LocalCodegenContext<'gen>,
    ) -> Result<BasicValueEnum<'gen>, Box<dyn Error>> {
        debug!("generator::CodeGenerator::codegen_literal");
        let t = match literal {
            TypedLiteral {
                literal: Literal::Str(s, _pos),
                ..
            } => {
                let s = self
                    .ctx
                    .ir_builder
                    .build_global_string_ptr(&s, "str")
                    .as_pointer_value();
                s.as_basic_value_enum()
            }
            TypedLiteral {
                ty,
                literal: Literal::Int(i, pos),
            } => {
                let Some(ty) = local_ctx.get_base_type(&ty.sig()) else {
                    return Err(format!("Type not found: {:?} {}", ty, pos).into());
                };
                let Some(ty) = ty.clone().borrow_mut().llvm_ty.clone() else {
                    return Err(format!("Type not found: {:?} {}", ty, pos).into());
                };
                let (value, sign) = if i < 0 {
                    (i.abs() as u64, true)
                } else {
                    (i as u64, false)
                };

                ty.int_type()?.const_int(value, sign).as_basic_value_enum()
            }
            TypedLiteral {
                ty,
                literal: Literal::Float(f, pos),
            } => {
                let Some(ty) = local_ctx.get_base_type(&ty.sig()) else {
                    return Err(format!("Type not found: {:?} {pos}", ty).into());
                };
                let Some(ty) = ty.clone().borrow_mut().llvm_ty.clone() else {
                    return Err(format!("Type not found: {:?} {}", ty, pos).into());
                };
                ty.float_type()?.const_float(f).as_basic_value_enum()
            }
            TypedLiteral {
                literal: Literal::Char(_c, _pos),
                ..
            } => unimplemented!("codegen char literal"),
            TypedLiteral {
                ty,
                literal: Literal::Bool(b, pos),
            } => {
                let Some(ty) = local_ctx.get_base_type(&ty.sig()) else {
                    return Err(format!("Type not found: {:?} {}", ty, pos).into());
                };
                let Some(ty) = ty.clone().borrow_mut().llvm_ty.clone() else {
                    return Err(format!("Type not found: {:?} {}", ty, pos).into());
                };

                ty.int_type()?
                    .const_int(b as u64, false)
                    .as_basic_value_enum()
            }
        };
        Ok(t)
    }

    fn codegen_return_statement(
        &self,
        ret_stmt: TypedReturnStatement<'gen>,
        local_ctx: LocalCodegenContext<'gen>,
        did_break: &mut bool,
    ) -> Result<(), Box<dyn Error>> {
        debug!("generator::CodeGenerator::codegen_return_statement");
        if let Some(ret_val) = ret_stmt.value {
            let val = self.codegen_expression(ret_val, local_ctx.clone(), did_break)?;
            match val {
                Some(BasicValueEnum::ArrayValue(v)) => self.ctx.ir_builder.build_return(Some(&v)),
                Some(BasicValueEnum::IntValue(v)) => self.ctx.ir_builder.build_return(Some(&v)),
                Some(BasicValueEnum::FloatValue(v)) => self.ctx.ir_builder.build_return(Some(&v)),
                Some(BasicValueEnum::PointerValue(v)) => self.ctx.ir_builder.build_return(Some(&v)),
                Some(BasicValueEnum::StructValue(v)) => self.ctx.ir_builder.build_return(Some(&v)),
                Some(BasicValueEnum::VectorValue(v)) => self.ctx.ir_builder.build_return(Some(&v)),
                None => return Err("Return value is None".into()),
            };
        } else {
            self.ctx.ir_builder.build_return(None);
        };
        Ok(())
    }

    fn codegen_identifier(
        &self,
        name: &str,
        local_ctx: LocalCodegenContext<'gen>,
    ) -> Result<BasicValueEnum<'gen>, Box<dyn Error>> {
        debug!(format!(
            "generator::CodeGenerator::codegen_identifier: {:?}",
            name
        ));
        let Some(name) = local_ctx.get_name(name) else {
            return Err(format!("Name not found: {}", name).into());
        };
        if name.is_arg {
            // get function arg
            let Some(arg_idx) = name.arg_idx else {
                return Err(format!("Arg index not found: {}", name.name).into());
            };
            if let Some(curr_fn) = local_ctx.current_fn {
                if let Some(param) = curr_fn.get_nth_param(arg_idx) {
                    Ok(param)
                } else {
                    return Err(format!("Param not found: {}", arg_idx).into());
                }
            } else {
                return Err("No current function".into());
            }
        } else {
            let Some(alloc) = name.alloc else {
                return Err(format!("Name alloc not found: {}", name.name).into());
            };
            if local_ctx.is_self_arg && name.ty.ty.is_struct_type() {
                return Ok(alloc.as_basic_value_enum());
            }
            let name = name.clone();
            /* if local_ctx.self_call {
                Ok(alloc.as_basic_value_enum())
            } else {
                let var = self.ctx.ir_builder.build_load(alloc, &name.name);
                Ok(var)
            } */
            let var = self.ctx.ir_builder.build_load(alloc, &name.name);
            Ok(var)
        }
    }

    fn codegen_var_assignment(
        &self,
        var_assign: TypedVarAssignment<'gen>,
        local_ctx: LocalCodegenContext<'gen>,
    ) -> Result<BasicValueEnum<'gen>, Box<dyn Error>> {
        debug!("generator::CodeGenerator::codegen_var_assignment");
        match var_assign.left.expr {
            TypedExpressionData::Identifier { name } => {
                let right =
                    self.codegen_expression(*var_assign.right, local_ctx.clone(), &mut false)?;
                let Some(right) = right else {
                    return Err("Right side of assignment is None".into());
                };

                let Some(name) = local_ctx.get_name(&name) else {
                    return Err(format!("Name not found: {}", &name).into());
                };
                let name = name.clone();

                let alloc = if let Some(alloc) = name.alloc {
                    alloc
                } else {
                    let Some(llvm_ty) = &name.ty.llvm_ty else {
                        return Err(format!("LLVM type not found: {}", &name.name).into());
                    };
                    self.ctx
                        .ir_builder
                        .build_alloca(llvm_ty.basic()?, &name.name)
                };

                self.ctx.ir_builder.build_store(alloc, right);
                let load = self.ctx.ir_builder.build_load(alloc, &name.name);
                Ok(load)
            }
            TypedExpressionData::MemberAccess { member_access } => {
                let TypedMemberAccess { object, member, .. } = member_access;
                // TODO: Handle computed member access expressions and array access expressions
                let Some(member_ty) = &member.ty else {
                    return Err("Member type is None".into());
                };
                let _member_sig = member_ty.sig();

                let TypedExpressionData::Identifier { name: member_name } = &member.expr else {
                    return Err("Member name is None".into());
                };

                let Some(object_ty) = &object.ty else {
                    return Err("Object type is None".into());
                };
                let object_sig = object_ty.sig();

                let Some(right) = self.codegen_expression(*var_assign.right.clone(), local_ctx.clone(), &mut false)? else {
                    return Err("Right side of assignment is None".into());
                };

                let Some(object) = self.codegen_expression(*object, local_ctx.clone(), &mut false)? else {
                    return Err("Object is None".into());
                };

                let TypeSignature::Struct(id) = object_sig else {
                    return Err(format!("Object type is not a struct: {:#?}", object_sig).into());
                };
                let Some(struct_t) = local_ctx.structs.get(&id) else {
                    return Err(format!("var_assign: Struct not found: {1} {0}", line!() , id).into());
                };

                let Some(member_idx) = struct_t.get_member_idx(&member_name) else {
                    return Err(format!("Member not found: {1} {0}", line!() , member_name).into());
                };

                let Some(member_ptr) = self.ctx.ir_builder.build_struct_gep(
                    object.into_pointer_value(),
                    member_idx,
                    (struct_t.name.clone() + "." + member_name).as_str(),
                ).ok() else {
                    return Err("Member pointer is None".into());
                };

                let Some(right_ty) = var_assign.right.ty.clone() else {
                    return Err("Right type is None".into());
                };
                let right_ty = self
                    .ctx
                    .to_llvm_ty(&right_ty.sig(), &local_ctx.structs)?
                    .pointer_type()?
                    .as_basic_type_enum();

                let member_ptr = self.build_ptr_cast(member_ptr, right_ty)?;

                let _store = self
                    .ctx
                    .ir_builder
                    .build_store(member_ptr.into_pointer_value(), right);

                Ok(member_ptr.as_basic_value_enum())
            }
            TypedExpressionData::UnaryOp { expr, .. } => {
                // left hand side dereference
                let Some(right) = self.codegen_expression(*var_assign.right.clone(), local_ctx.clone(), &mut false)? else {
                    return Err("Right side of assignment is None".into());
                };

                let Some(left) = self.codegen_lhs_expression(*expr, local_ctx.clone())? else {
                    return Err("Left side of assignment is None".into());
                };

                let Some(size) = right.get_type().size_of() else {
                    return Err("Size of right is None".into());
                };

                if let BasicValueEnum::PointerValue(p) = right {
                    if p.is_const() {
                        self.ctx.ir_builder.build_memcpy(
                            left,
                            8,
                            right.into_pointer_value(),
                            8,
                            size,
                        )?;
                        let load = self.ctx.ir_builder.build_load(left, "deref_assign_result");
                        return Ok(load);
                    }
                }

                let Some(right_ty) = &var_assign.right.ty else {
                    return Err("No right type".into());
                };
                let right_sig = right_ty.sig();
                let right_ty = self
                    .ctx
                    .to_llvm_ty(&right_sig.wrap_in_ptr(), &local_ctx.structs)?
                    .pointer_type()?;

                let left = self.build_ptr_cast(left, right_ty.as_basic_type_enum())?;
                self.ctx
                    .ir_builder
                    .build_store(left.into_pointer_value(), right);
                let load = self
                    .ctx
                    .ir_builder
                    .build_load(left.into_pointer_value(), "deref_assign_result");
                Ok(load)
            }
            _ => return Err("Invalid left side of assignment".into()),
        }
    }

    pub(crate) fn codegen_lhs_expression(
        &self,
        expr: TypedExpression<'gen>,
        local_ctx: LocalCodegenContext<'gen>,
    ) -> Result<Option<PointerValue<'gen>>, Box<dyn Error>> {
        debug!("generator::CodeGenerator::codegen_lhs_expression");
        match &expr.expr {
            TypedExpressionData::UnaryOp { .. } => {
                /* let Some(val) = self.codegen_expression(expr, local_ctx.clone())? else {
                    return Err("Invalid left side of assignment".into());
                }; */
                let TypedExpressionData::UnaryOp { expr, op } = expr.expr else {
                    return Err("Invalid left side of assignment".into());
                };

                match op {
                    Operator::Times => Ok(self.codegen_lhs_expression(*expr, local_ctx)?),
                    _ => Err("Invalid left side of assignment".into()),
                }
            }
            TypedExpressionData::Identifier { name: _name } => {
                let Some(val) = self.codegen_expression(expr, local_ctx, &mut false)? else {
                    return Err("Expression is None".into());
                };
                Ok(Some(val.into_pointer_value()))
            }
            TypedExpressionData::MemberAccess { .. } => {
                let TypedExpressionData::MemberAccess { member_access } = expr.expr else {
                    return Err("Invalid left side of assignment".into());
                };
                let val = self.codegen_lhs_member_access(member_access, local_ctx)?;
                Ok(Some(val.into_pointer_value()))
            }
            _ => Err("Invalid left side of assignment".into()),
        }
    }

    fn codegen_lhs_member_access(
        &self,
        member_access: TypedMemberAccess<'gen>,
        local_ctx: LocalCodegenContext<'gen>,
    ) -> Result<BasicValueEnum<'gen>, Box<dyn Error>> {
        debug!("generator::CodeGenerator::codegen_lhs_member_access");
        let TypedMemberAccess { object, member, .. } = member_access;

        let Some(object_type) = &object.ty else {
            return Err("Object type is None".into());
        };
        let object_type = object_type.sig();
        let Some(object) = self.codegen_expression(*object, local_ctx.clone(), &mut false)? else {
            return Err("Object is None".into());
        };

        match &object_type {
            TypeSignature::Primitive(_primitive) => {
                unimplemented!("codegen_lhs_member_access primitive")
            }
            TypeSignature::Struct(id) => {
                let member = (*member).clone();
                match member.expr {
                    TypedExpressionData::Identifier { name } => {
                        let structure = local_ctx.structs.get(id).ok_or_else(|| {
                            format!("Struct not found: {}:{}:{}", file!(), line!(), id)
                        })?;
                        let Some(member_idx) = structure.get_member_idx(&name) else {
                            return Err(format!("Member not found: {}", &name).into());
                        };
                        let Some(member) = self.ctx.ir_builder.build_extract_value(
                            object.into_struct_value(),
                            member_idx,
                            name.as_str(),
                        ) else {
                            return Err("Member is None".into());
                        };
                        Ok(member)
                    }
                    _ => unreachable!(),
                }
            }
            TypeSignature::Function(_function) => unreachable!(),
            TypeSignature::Pointer(pointer) => {
                let member = (*member).clone();
                let target = pointer.target.as_ref();
                if let TypeSignature::Struct(id) = target {
                    let s = local_ctx.structs.get(id).ok_or_else(|| {
                        format!(
                            "member_access_ptr: Struct not found: {}:{}:{}",
                            file!(),
                            line!(),
                            id
                        )
                    })?;
                    match member.expr {
                        TypedExpressionData::Identifier { name } => {
                            let Some(member_reg) = s.fields.get(&name) else {
                                return Err("Member does not exist".into());
                            };
                            let _member_ty = member_reg.ty.sig().wrap_in_ptr();
                            let member_idx = member_reg.idx;
                            let Some(member) = self.ctx.ir_builder.build_struct_gep(
                                object.into_pointer_value(),
                                member_idx,
                                (s.name.clone() + "." + &name + "_LHS").as_str(),
                            ).ok() else {
                                return Err("Member is None".into());
                            };

                            Ok(member.as_basic_value_enum())
                        }
                        _ => unreachable!(),
                    }
                } else {
                    todo!("codegen_member_access")
                }
            }
            _ => unreachable!(),
        }
    }

    fn get_or_declare_function(
        &self,
        fn_name: &str,
        local_ctx: &LocalCodegenContext<'gen>,
    ) -> Result<FunctionValue<'gen>, Box<dyn Error>> {
        debug!("generator::CodeGenerator::get_or_declare_function");

        let func = if let Some(func) = local_ctx.names.get(fn_name) {
            if let Some(func) = self.ctx.llvm_module.get_function(&func.name) {
                return Ok(func);
            } else {
                func
            }
        } else {
            if let Some((_, func)) = local_ctx
                .names
                .iter()
                .find(|(_, reg)| reg.name.as_str() == fn_name)
            {
                func
            } else {
                return Err(format!("Function not found: {} ({})", fn_name, line!()).into());
            }
        };

        let llvm_ty = func.ty.llvm_ty.clone().unwrap();

        let func = self.ctx.llvm_module.add_function(
            &func.name,
            llvm_ty.function()?,
            Some(Linkage::External),
        );
        //println!("func: {:?}\n{}\n", fn_name, func);
        Ok(func)
    }

    fn codegen_fn_call(
        &self,
        mut fn_call: TypedFunctionCall<'gen>,
        local_ctx: LocalCodegenContext<'gen>,
    ) -> Result<Option<BasicValueEnum<'gen>>, Box<dyn Error>> {
        debug!(format!(
            "generator::CodeGenerator::codegen_fn_call: {:?}",
            fn_call.callee
        ));

        let (callee_name, callee, self_arg) = match *fn_call.callee {
            TypedExpression {
                expr: TypedExpressionData::Identifier { name },
                ..
            } => {
                // Function call
                (
                    name.clone(),
                    { self.get_or_declare_function(&name, &local_ctx)? },
                    None,
                )
            }
            TypedExpression {
                expr:
                    TypedExpressionData::MemberAccess {
                        member_access: TypedMemberAccess { object, member, .. },
                    },
                ..
            } => {
                // Member function call
                let object_val =
                    self.codegen_expression(*object.clone(), local_ctx.clone(), &mut false)?;
                let Some(object_val) = object_val else {
                    return Err("Object is None".into());
                };
                // check that object is struct
                let Some(object_type) = &object.ty else {
                    return Err("Object type is None".into());
                };

                let object_type = object_type.sig().wrap_in_ptr();

                let struct_ty = if let TypeSignature::Pointer(pointer) = &object_type {
                    if let TypeSignature::Struct(s_id) = pointer.target.as_ref() {
                        local_ctx.structs.get(s_id).ok_or_else(|| {
                            format!("Struct not found: {}:{}:{}", file!(), line!(), s_id)
                        })?
                    } else {
                        return Err("Object is not a struct".into());
                    }
                } else {
                    if let TypeSignature::Struct(s_id) = &object_type {
                        local_ctx.structs.get(s_id).ok_or_else(|| {
                            format!("Struct not found: {}:{}:{}", file!(), line!(), s_id)
                        })?
                    } else {
                        return Err("Object is not a struct".into());
                    }
                };
                let TypedExpression {
                    expr: TypedExpressionData::Identifier { name },
                    ..
                } = member.as_ref().clone() else {
                    return Err("Member is not an identifier".into());
                };
                //self.ctx.llvm_module.print_to_stderr();
                let TypeSignature::Function(f) = struct_ty.methods.get(&name).unwrap().fn_ty.sig() else {
                    return Err("Method not found".into());
                };

                let full_name = if f.has_self_param {
                    struct_ty.name.clone() + "." + &name
                } else {
                    struct_ty.name.clone() + "::" + &name
                };

                let Some(llvm_fn) = self.ctx.llvm_module.get_function(&full_name) else  {

                    return Err(format!("Callee {} not found!", full_name).into());
                };

                (full_name.clone(), llvm_fn, Some(object_val))
            }
            _ => unreachable!("Member function calls not yet supported in codegen"),
        };

        let mut args = Vec::new();

        for (idx, arg) in fn_call.args.drain(..).enumerate() {
            if self_arg.is_some() && idx == 0 {
                let arg = self.codegen_expression(arg, local_ctx.self_arg(true), &mut false)?;
                let arg = match arg {
                    Some(BasicValueEnum::ArrayValue(v)) => BasicMetadataValueEnum::ArrayValue(v),
                    Some(BasicValueEnum::IntValue(v)) => BasicMetadataValueEnum::IntValue(v),
                    Some(BasicValueEnum::FloatValue(v)) => BasicMetadataValueEnum::FloatValue(v),
                    Some(BasicValueEnum::PointerValue(v)) => {
                        BasicMetadataValueEnum::PointerValue(v)
                    }
                    Some(BasicValueEnum::StructValue(v)) => BasicMetadataValueEnum::StructValue(v),
                    Some(BasicValueEnum::VectorValue(v)) => BasicMetadataValueEnum::VectorValue(v),
                    None => return Err("Argument is None".into()),
                };
                args.push(arg);
                continue;
            }
            let arg = self.codegen_expression(arg, local_ctx.clone(), &mut false)?;
            let arg = match arg {
                Some(BasicValueEnum::ArrayValue(v)) => BasicMetadataValueEnum::ArrayValue(v),
                Some(BasicValueEnum::IntValue(v)) => BasicMetadataValueEnum::IntValue(v),
                Some(BasicValueEnum::FloatValue(v)) => BasicMetadataValueEnum::FloatValue(v),
                Some(BasicValueEnum::PointerValue(v)) => BasicMetadataValueEnum::PointerValue(v),
                Some(BasicValueEnum::StructValue(v)) => BasicMetadataValueEnum::StructValue(v),
                Some(BasicValueEnum::VectorValue(v)) => BasicMetadataValueEnum::VectorValue(v),
                None => return Err("Argument is None".into()),
            };
            args.push(arg);
        }

        let call = self.ctx.ir_builder.build_call(
            callee,
            args.as_slice(),
            (callee_name + "_call").as_str(),
        );

        if let Some(ret) = call.try_as_basic_value().left() {
            Ok(Some(ret))
        } else {
            Ok(None)
        }
    }

    fn codegen_binary_op(
        &self,
        left: TypedExpression<'gen>,
        right: TypedExpression<'gen>,
        op: &Operator,
        local_ctx: LocalCodegenContext<'gen>,
    ) -> Result<BasicValueEnum<'gen>, Box<dyn Error>> {
        debug!("generator::CodeGenerator::codegen_binary_op");
        let left = self.codegen_expression(left, local_ctx.clone(), &mut false)?;
        let right = self.codegen_expression(right, local_ctx.clone(), &mut false)?;
        let Some(left) = left else {
            return Err("Left side of binary op is None".into());
        };
        let Some(right) = right else {
            return Err("Right side of binary op is None".into());
        };
        if left.get_type() != right.get_type() {
            return Err("Left and right types don't match".into());
        }

        let result = match op {
            Operator::Plus => self.build_add(left, right),
            Operator::Minus => self.build_sub(left, right),
            Operator::Times => self.build_mul(left, right),
            Operator::Divide => self.build_div(left, right),
            Operator::Modulo => self.build_rem(left, right),
            Operator::Equals => self.build_eq(left, right),
            Operator::NotEquals => self.build_ne(left, right),
            Operator::LessThan => self.build_lt(left, right),
            Operator::LessOrEqual => self.build_le(left, right),
            Operator::GreaterThan => self.build_gt(left, right),
            Operator::GreaterOrEqual => self.build_ge(left, right),
            _ => unreachable!(),
        };
        Ok(result)
    }

    fn field_name_at(
        &self,
        struct_ty: Rc<CodegenType>,
        idx: usize,
        local_ctx: &LocalCodegenContext<'gen>,
    ) -> String {
        let TypeSignature::Struct(id) = struct_ty.ty.clone() else {
            unreachable!();
        };
        let struct_ty = local_ctx.structs.get(&id).unwrap();
        struct_ty
            .fields
            .iter()
            .find(|(_, f)| f.idx as usize == idx)
            .unwrap()
            .1
            .name
            .clone()
    }

    fn codegen_struct_init(
        &self,
        struct_init: TypedStructInitializer<'gen>,
        local_ctx: LocalCodegenContext<'gen>,
    ) -> Result<BasicValueEnum<'gen>, Box<dyn Error>> {
        debug!("generator::CodeGenerator::codegen_struct_init");
        /* let Some(struct_ty) = local_ctx.get_type_by_name(&struct_init.struct_name.clone()) else {
            return Err(format!("codegen_struct_init: Struct {} not found", struct_init.struct_name.clone()).into());
        }; */
        let Some(struct_codegen_ty) = local_ctx.types.get(&struct_init.struct_ty.sig()) else {
            return Err(format!("codegen_struct_init: Struct {:?} not found", struct_init.struct_ty.sig()).into());
        };
        let struct_name = struct_codegen_ty.name.clone();

        let mut values = Vec::new();
        for field in struct_init.fields.iter() {
            let idx = field.idx;
            let Some(field) = self.codegen_expression(field.value.clone(), local_ctx.clone(), &mut false)? else {
                return Err("Field is None".into());
            };
            values.push((idx, field));
        }

        let Some(llvm_ty) = &struct_codegen_ty.llvm_ty else {
            return Err("Invalid struct type".into());
        };

        let struct_ty = llvm_ty.struct_type()?;
        let global = if let Some(global) = self
            .ctx
            .llvm_module
            .get_global((struct_name.clone() + ".init").as_str())
        {
            global
        } else {
            let global = self.ctx.llvm_module.add_global(
                llvm_ty.struct_type()?,
                Some(AddressSpace::Global),
                (struct_name.clone() + ".init").as_str(),
            );
            global.set_initializer(&struct_ty.const_zero());
            global.set_constant(true);
            global
        };

        let temp_alloc = self
            .ctx
            .ir_builder
            .build_alloca(struct_ty, (struct_name.clone() + "_init_temp").as_str());

        self.ctx.ir_builder.build_memcpy(
            temp_alloc,
            8,
            global.as_pointer_value(),
            8,
            struct_ty.size_of().ok_or("Invalid size")?,
        )?;

        let field_types = struct_ty.get_field_types();
        for (idx, value) in values.iter() {
            let Some(gep) = self.ctx.ir_builder.build_struct_gep(
                temp_alloc,
                *idx,
                (struct_name.clone() + "." + &self.field_name_at(struct_codegen_ty.clone(), (*idx) as usize, &local_ctx) + "_init_gep").as_str(),
            ).ok() else {
                return Err("Invalid gep".into());
            };
            let Some(field_type) = field_types.get(*idx as usize) else {
                return Err("Invalid field type".into());
            };
            let field_type = field_type
                .ptr_type(AddressSpace::Generic)
                .as_basic_type_enum();
            let gep = self.build_ptr_cast(gep, field_type)?.into_pointer_value();

            self.ctx.ir_builder.build_store(gep, *value);
        }

        let load = self.ctx.ir_builder.build_load(
            temp_alloc,
            ("load_".to_owned() + &struct_name + "_init").as_str(),
        );

        Ok(load)
    }

    fn codegen_member_access(
        &self,
        member_access: TypedMemberAccess<'gen>,
        local_ctx: LocalCodegenContext<'gen>,
    ) -> Result<BasicValueEnum<'gen>, Box<dyn Error>> {
        debug!("generator::CodeGenerator::codegen_member_access");
        let TypedMemberAccess { object, member, .. } = member_access;

        let Some(object_type) = &object.ty else {
            return Err("Object type is None".into());
        };
        let object_type = object_type.sig();
        let Some(object) = self.codegen_expression(*object, local_ctx.clone(), &mut false)? else {
            return Err("Object is None".into());
        };

        match &object_type {
            TypeSignature::Primitive(_primitive) => todo!("codegen_member_access: Primitive"),
            TypeSignature::Struct(id) => {
                let member = (*member).clone();
                match member.expr {
                    TypedExpressionData::Identifier { name } => {
                        let structure = local_ctx.structs.get(id).ok_or_else(|| {
                            format!("Struct not found: {}:{}:{}", file!(), line!(), id)
                        })?;
                        let Some(member_idx) = structure.get_member_idx(&name) else {
                            return Err(format!("Member not found: {}", &name).into());
                        };
                        if object.is_pointer_value() {
                            /* let Some(member) = self.ctx.ir_builder.build_extract_value(
                                object.into_struct_value(),
                                member_idx,
                                name.as_str(),
                            ) else {
                                return Err("Member is None".into());
                            };
                            member. */
                            let Ok(member) = self.ctx.ir_builder.build_struct_gep(object.into_pointer_value(), member_idx, name.as_str()) else {
                                return Err("Member is None".into());
                            };

                            Ok(member.as_basic_value_enum())
                        } else {
                            let Some(member) = self.ctx.ir_builder.build_extract_value(
                                object.into_struct_value(),
                                member_idx,
                                name.as_str(),
                            ) else {
                                return Err("Member is None".into());
                            };
                            Ok(member)
                        }
                    }
                    _ => unreachable!(),
                }
            }
            TypeSignature::Function(_function) => unreachable!(),
            TypeSignature::Pointer(pointer) => {
                let member = (*member).clone();
                let target = pointer.target.as_ref();
                if let TypeSignature::Struct(id) = target {
                    let s = local_ctx.structs.get(id).ok_or_else(|| {
                        format!(
                            "member_access_ptr: Struct not found: {}:{}:{}",
                            file!(),
                            line!(),
                            id
                        )
                    })?;
                    match &member.expr {
                        TypedExpressionData::Identifier { name } => {
                            let Some(member_idx) = s.get_member_idx(&name) else {
                                return Err(format!("Member not found: {}", &name).into());
                            };

                            let Some(member) = self.ctx.ir_builder.build_struct_gep(
                                object.into_pointer_value(),
                                member_idx,
                                (s.name.clone() + "." + name.as_str()).as_str(),
                            ).ok() else {
                                return Err("Member is None".into());
                            };
                            Ok(member.as_basic_value_enum())
                        }
                        _ => unreachable!(),
                    }
                } else {
                    todo!("codegen_member_access")
                }
            }
            _ => unreachable!(),
        }
    }

    fn codegen_result_statement(
        &self,
        result_stmt: TypedResultStatement<'gen>,
        local_ctx: LocalCodegenContext<'gen>,
    ) -> Result<BasicValueEnum<'gen>, Box<dyn Error>> {
        debug!("generator::CodeGenerator::codegen_result_statement");
        let value = self.codegen_expression(result_stmt.value, local_ctx.clone(), &mut false)?;
        let Some(value) = value else {
            return Err("Value is None".into());
        };
        Ok(value)
    }
}
