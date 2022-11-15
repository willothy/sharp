use std::{
    borrow::Borrow,
    cell::{Ref, RefCell},
    collections::HashMap,
    error::Error,
    rc::Rc,
};

use inkwell::{
    types::AnyTypeEnum,
    values::{BasicValue, BasicValueEnum, InstructionValue},
};

use crate::{
    ast::Declaration,
    typechecker::{FunctionType, TypeCheckContext, TypeSignature},
};

use super::context::{to_basic, CodegenContext, CodegenName, CodegenType, LocalCodegenContext};

pub struct CodeGenerator<'gen> {
    pub ctx: CodegenContext<'gen>,
}

impl<'gen> CodeGenerator<'gen> {
    pub fn new(
        tc_ctx: &'gen TypeCheckContext<'gen>,
        llvm_ctx: &'gen inkwell::context::Context,
    ) -> Self {
        let module = llvm_ctx.create_module("main");
        let builder = llvm_ctx.create_builder();
        Self {
            ctx: CodegenContext::new(llvm_ctx, module, builder, tc_ctx),
        }
    }
    pub fn codegen_module(
        &self,
        module: &'gen crate::ast::Module,
    ) -> Result<inkwell::module::Module<'gen>, Box<dyn std::error::Error>> {
        let mut local_ctx = LocalCodegenContext {
            names: HashMap::new(),
            types: HashMap::new(),
        };

        self.ctx.add_primitive_types(&mut local_ctx);

        for (name, name_ctx) in &self.ctx.tc_ctx.types {
            let type_sig = name_ctx.borrow_mut().type_.clone();
            let llvm_t = self.ctx.to_llvm_ty(&type_sig)?;
            local_ctx.add_type(CodegenType::new(name.clone(), type_sig.clone(), llvm_t));
        }

        for (name, name_ctx) in &self.ctx.tc_ctx.names {
            let t = &name_ctx.var_type.borrow_mut().type_;
            let t = if let TypeSignature::Function(fn_t) = t {
                Rc::from(CodegenType::new(
                    name.clone(),
                    t.clone(),
                    self.ctx.function_to_llvm_ty(fn_t)?,
                ))
            } else {
                let Some(t) = local_ctx.types.get(t) else {
                    return Err(format!("Type not found: {:?}", t).into());
                };
                t.clone()
            };

            let name = CodegenName::new(name.clone(), t.clone());
            local_ctx.add_name(name);
        }

        for decl in &module.body {
            self.codegen_declaration(decl, local_ctx.clone())?;
        }

        Ok(self.ctx.llvm_module.clone())
    }

    pub fn codegen_declaration(
        &self,
        decl: &'gen Declaration,
        local_ctx: LocalCodegenContext<'gen>,
    ) -> Result<(), Box<dyn std::error::Error>> {
        match decl {
            Declaration::Function(func) => {
                self.codegen_function(func, local_ctx)?;
            }
            Declaration::Struct(strct) => {
                self.codegen_struct(strct, local_ctx)?;
            }
            _ => {}
        }

        Ok(())
    }

    fn codegen_function(
        &self,
        func: &'gen crate::ast::FunctionDeclaration,
        mut local_ctx: LocalCodegenContext<'gen>,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let Some(func_reg) = local_ctx.names.get(&func.name) else {
            return Err(format!("Function not found: {}", func.name).into());
        };

        let llvm_fn = self.ctx.llvm_module.add_function(
            &func.name.clone(),
            func_reg.ty.llvm_ty.into_function_type(),
            None,
        );

        for param in &func.params {
            let type_sig = self.ctx.tc_ctx.get_type(param.type_name.clone())?;
            let type_sig = &type_sig.borrow_mut().type_;
            let Some(param_type) = local_ctx.get_type(type_sig) else {
                return Err(format!("Type not found: {:?}", type_sig).into());
            };
            let param_name = CodegenName::new(param.name.clone(), param_type.clone());
            local_ctx.add_name(param_name);
        }

        let entry_block = self.ctx.llvm_ctx.append_basic_block(llvm_fn, "entry");
        self.ctx.ir_builder.position_at_end(entry_block);
        for stmt in &func.body.statements {
            self.codegen_statement(stmt, &mut local_ctx)?;
        }

        Ok(())
    }

    fn codegen_statement(
        &self,
        stmt: &'gen crate::ast::Statement,
        mut local_ctx: &mut LocalCodegenContext<'gen>,
    ) -> Result<(), Box<dyn std::error::Error>> {
        match stmt {
            crate::ast::Statement::Variable(var_stmt) => {
                self.codegen_variable_statement(var_stmt, &mut local_ctx)
            }
            crate::ast::Statement::Expression(_) => todo!(),
            crate::ast::Statement::Loop(_) => todo!(),
            crate::ast::Statement::Return(ret_stmt) => {
                self.codegen_return_statement(ret_stmt, local_ctx.clone())
            }
            crate::ast::Statement::Yield(_) => todo!(),
            crate::ast::Statement::Continue => todo!(),
            crate::ast::Statement::Break => todo!(),
        }
    }

    fn codegen_variable_statement(
        &self,
        var_stmt: &crate::ast::VarDeclaration,
        local_ctx: &mut LocalCodegenContext<'gen>,
    ) -> Result<(), Box<dyn Error>> {
        let Ok(type_reg) = self.ctx.tc_ctx.get_type(var_stmt.type_name.clone()) else {
            return Err(format!("Type not found: {}", var_stmt.type_name).into());
        };
        let type_sig = &type_reg.borrow_mut().type_;

        let Some(var_type) = local_ctx.get_type(type_sig) else {
            return Err(format!("Type not found: {:?}", var_stmt.type_name).into());
        };
        let var_type = var_type.clone();

        let mut var_name = CodegenName::new(var_stmt.name.clone(), var_type.clone());

        let store_ptr = self
            .ctx
            .ir_builder
            .build_alloca(to_basic(var_type.llvm_ty)?, &var_stmt.name);
        // Add store ptr to local context
        var_name.alloc = Some(store_ptr.clone());
        local_ctx.add_name(var_name);

        if let Some(init) = &var_stmt.initializer {
            let init = self.codegen_expression(init, local_ctx.clone())?;
            println!("init: {:?}", init);
            let var = self.ctx.ir_builder.build_store(store_ptr, init);
            println!("var: {:?}", var);
            /* let var = ctx
                .ir_builder
                .build_load(to_basic(var_type.llvm_ty)?, &var_stmt.name);
            ctx.ir_builder.build_store(var, init); */
        }

        Ok(())
    }

    fn codegen_expression(
        &self,
        init: &crate::ast::Expression,
        local_ctx: LocalCodegenContext<'gen>,
    ) -> Result<BasicValueEnum<'gen>, Box<dyn Error>> {
        match init {
            crate::ast::Expression::BinaryOp { left, right, op } => todo!(),
            crate::ast::Expression::LogicalOp { left, right, op } => todo!(),
            crate::ast::Expression::UnaryOp { expr, op } => todo!(),
            crate::ast::Expression::Identifier { name } => self.codegen_identifier(name, local_ctx),
            crate::ast::Expression::Literal { literal } => self.codegen_literal(literal, local_ctx),
            crate::ast::Expression::If { expr } => todo!(),
            crate::ast::Expression::Block { block } => todo!(),
            crate::ast::Expression::VarAssignment { var_assign } => todo!(),
            crate::ast::Expression::FnCall { fn_call } => todo!(),
            crate::ast::Expression::MemberAccess { member_access } => todo!(),
            crate::ast::Expression::StructInitializer { struct_init } => todo!(),
        }
    }

    fn codegen_literal(
        &self,
        literal: &crate::tokenizer::Literal,
        local_ctx: LocalCodegenContext<'gen>,
    ) -> Result<BasicValueEnum<'gen>, Box<dyn Error>> {
        let t = match literal {
            crate::tokenizer::Literal::String(_) => todo!(),
            crate::tokenizer::Literal::Int(i) => self
                .ctx
                .llvm_ctx
                .i64_type()
                .const_int(*i as u64, false)
                .as_basic_value_enum(),
            crate::tokenizer::Literal::Float(_) => todo!(),
            crate::tokenizer::Literal::Char(_) => todo!(),
            crate::tokenizer::Literal::Bool(_) => todo!(),
        };
        Ok(t)
    }

    fn codegen_struct(
        &self,
        strct: &crate::ast::StructDeclaration,
        local_ctx: LocalCodegenContext<'gen>,
    ) -> Result<(), Box<dyn std::error::Error>> {
        Ok(())
    }

    fn codegen_return_statement(
        &self,
        ret_stmt: &'gen crate::ast::ReturnStatement,
        local_ctx: LocalCodegenContext<'gen>,
    ) -> Result<(), Box<dyn Error>> {
        if let Some(ret_val) = &ret_stmt.value {
            let val = self.codegen_expression(ret_val, local_ctx.clone())?;
            match val {
                BasicValueEnum::ArrayValue(v) => self.ctx.ir_builder.build_return(Some(&v)),
                BasicValueEnum::IntValue(v) => self.ctx.ir_builder.build_return(Some(&v)),
                BasicValueEnum::FloatValue(v) => self.ctx.ir_builder.build_return(Some(&v)),
                BasicValueEnum::PointerValue(v) => self.ctx.ir_builder.build_return(Some(&v)),
                BasicValueEnum::StructValue(v) => self.ctx.ir_builder.build_return(Some(&v)),
                BasicValueEnum::VectorValue(v) => self.ctx.ir_builder.build_return(Some(&v)),
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
        let Some(name) = local_ctx.get_name(name) else {
            return Err(format!("Name not found: {}", name).into());
        };
        let Some(alloc) = name.alloc else {
            return Err(format!("Name alloc not found: {}", name.name).into());
        };
        let name = name.clone();
        let var = self.ctx.ir_builder.build_load(alloc, &name.name);
        Ok(var)
    }
}
