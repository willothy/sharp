use std::{
    borrow::{Borrow, BorrowMut},
    cell::{Ref, RefCell},
    collections::HashMap,
    error::Error,
    rc::Rc,
};

use inkwell::{
    attributes::Attribute,
    module::Linkage,
    types::{AnyType, AnyTypeEnum, BasicType, BasicTypeEnum},
    values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, InstructionValue, MetadataValue},
    AddressSpace,
};

use crate::{
    ast::Declaration,
    tokenizer::Operator,
    typechecker::{
        context::TypeSig,
        type_sig::{FunctionType, PrimitiveType, TypeSignature},
        typed_ast::{
            TypedDeclaration, TypedFunctionDeclaration, TypedFunctionDefinition, TypedIfExpression,
            TypedMemberAccess, TypedModule, TypedReturnStatement, TypedStatement,
            TypedStructDeclaration, TypedStructInitializer, TypedVarDeclaration,
            TypedYieldStatement,
        },
    },
};
use crate::{
    tokenizer::Literal,
    typechecker::{
        context::TypeCheckContext,
        typed_ast::{
            TypedExpression, TypedExpressionData, TypedFunctionCall, TypedLiteral,
            TypedVarAssignment,
        },
        TypeCheckModule,
    },
};

use super::context::{
    to_basic, CodegenContext, CodegenLLVMType, CodegenName, CodegenType, GetLLVMType,
    LocalCodegenContext,
};

pub struct CodeGenerator<'gen> {
    pub ctx: CodegenContext<'gen>,
}

impl<'gen> CodeGenerator<'gen> {
    pub fn new(
        tc_mod: &'gen TypeCheckModule<'gen>,
        llvm_ctx: &'gen inkwell::context::Context,
    ) -> Self {
        let module = llvm_ctx.create_module("main");
        let builder = llvm_ctx.create_builder();
        Self {
            ctx: CodegenContext::new(llvm_ctx, module, builder, tc_mod),
        }
    }
    pub fn codegen_module(
        &self,
    ) -> Result<inkwell::module::Module<'gen>, Box<dyn std::error::Error>> {
        let mut local_ctx = LocalCodegenContext {
            names: HashMap::new(),
            types: HashMap::new(),
            return_type: None,
            yield_type: None,
            current_fn: None,
        };

        self.ctx.add_primitive_types(&mut local_ctx);

        for (name, name_ctx) in &self.ctx.tc_mod.ctx.types {
            let type_sig = name_ctx.sig();
            let llvm_t = self.ctx.to_llvm_ty(&type_sig)?;
            local_ctx.add_type(CodegenType::new(name.clone(), type_sig.clone(), llvm_t));
        }

        for (name, name_ctx) in &self.ctx.tc_mod.ctx.names {
            let t = name_ctx.ty.sig();
            let t = if let TypeSignature::Function(fn_t) = t.clone() {
                Rc::from(CodegenType::new(
                    name.clone(),
                    t,
                    self.ctx.function_to_llvm_ty(&fn_t)?,
                ))
            } else {
                let Some(t) = local_ctx.types.get(&t) else {
                    return Err(format!("Type not found: {:?} {}:{}", t, file!(), line!()).into());
                };
                t.clone()
            };

            let name = CodegenName::new(name.clone(), t.clone());
            local_ctx.add_name(name);
        }

        // Codegen structs
        for decl in &self.ctx.tc_mod.module.body {
            if let TypedDeclaration::Struct(structure) = decl {
                self.codegen_struct(structure, &mut local_ctx)?;
            }
        }

        // Codegen function declarations
        for decl in &self.ctx.tc_mod.module.body {
            if let TypedDeclaration::FunctionDecl(func) = decl {
                self.codegen_fn_decl(func, local_ctx.clone())?;
            }
        }

        for decl in &self.ctx.tc_mod.module.body {
            if let TypedDeclaration::FunctionDef(func) = decl {
                self.codegen_fn_def(func, local_ctx.clone())?;
            }
        }

        Ok(self.ctx.llvm_module.clone())
    }

    fn codegen_fn_decl(
        &self,
        func: &'gen TypedFunctionDeclaration,
        local_ctx: LocalCodegenContext<'gen>,
    ) -> Result<(), Box<dyn Error>> {
        let Some(func_reg) = local_ctx.names.get(&func.name) else {
            return Err(format!("Function not found: {}", func.name).into());
        };

        let _llvm_fn = self.ctx.llvm_module.add_function(
            &func.name.clone(),
            func_reg.ty.llvm_ty.function()?,
            Some(Linkage::External),
        );

        Ok(())
    }

    fn codegen_fn_def(
        &self,
        func: &'gen TypedFunctionDefinition<'gen>,
        mut local_ctx: LocalCodegenContext<'gen>,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let Some(func_reg) = local_ctx.names.get(&func.name) else {
            return Err(format!("Function not found: {}", func.name).into());
        };

        let llvm_fn = self.ctx.llvm_module.add_function(
            &func.name.clone(),
            func_reg.ty.llvm_ty.function()?,
            None,
        );

        for (idx, param) in func.params.values().enumerate() {
            let type_sig = param.ty.sig();
            let Some(param_type) = local_ctx.get_base_type(&type_sig) else {
                return Err(format!("Type not found: {:?} {}:{}", type_sig, file!(), line!()).into());
            };
            let param_name =
                CodegenName::new_arg(param.name.clone(), param_type.clone(), idx as u32);
            local_ctx.add_name(param_name);
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

        for stmt in &func.body.statements {
            self.codegen_statement(stmt, &mut local_ctx)?;
        }

        Ok(())
    }

    fn codegen_statement(
        &self,
        stmt: &'gen TypedStatement<'gen>,
        mut local_ctx: &mut LocalCodegenContext<'gen>,
    ) -> Result<Option<BasicValueEnum<'gen>>, Box<dyn std::error::Error>> {
        match stmt {
            TypedStatement::Variable(var_stmt) => {
                self.codegen_variable_statement(var_stmt, &mut local_ctx)?;
                Ok(None)
            }
            TypedStatement::Expression(expr_stmt) => {
                let res = self.codegen_expression(expr_stmt, local_ctx.clone())?;
                Ok(res)
            }
            TypedStatement::Loop(_) => todo!(),
            TypedStatement::Return(ret_stmt) => {
                self.codegen_return_statement(ret_stmt, local_ctx.clone())?;
                Ok(None)
            }
            TypedStatement::Yield(yield_stmt) => Ok(Some(
                self.codegen_yield_statement(yield_stmt, local_ctx.clone())?,
            )),
            TypedStatement::Continue => todo!(),
            TypedStatement::Break => todo!(),
        }
    }

    fn codegen_variable_statement(
        &self,
        var_stmt: &'gen TypedVarDeclaration<'gen>,
        local_ctx: &mut LocalCodegenContext<'gen>,
    ) -> Result<(), Box<dyn Error>> {
        let type_sig = var_stmt.ty.sig();

        let Some(var_type) = local_ctx.get_base_type(&type_sig) else {
            return Err(format!("Type not found: {:?} {}:{}", var_stmt.ty, file!(), line!()).into());
        };
        let var_type = var_type.clone();

        let mut var_name = CodegenName::new(var_stmt.name.clone(), var_type.clone());

        let store_ptr = self
            .ctx
            .ir_builder
            .build_alloca(var_type.llvm_ty.basic()?, &var_stmt.name);
        // Add store ptr to local context
        var_name.alloc = Some(store_ptr.clone());
        local_ctx.add_name(var_name);

        if let Some(init) = &var_stmt.initializer {
            let init = self.codegen_expression(init, local_ctx.clone())?;
            let Some(init) = init else {
                return Err("Expected initializer".into());
            };
            //println!("init: {:?}", init);
            let var = self.ctx.ir_builder.build_store(store_ptr, init);
            //println!("var: {:?}", var);
            /* let var = ctx
                .ir_builder
                .build_load(to_basic(var_type.llvm_ty)?, &var_stmt.name);
            ctx.ir_builder.build_store(var, init); */
        }

        Ok(())
    }

    fn codegen_expression(
        &self,
        init: &'gen TypedExpression<'gen>,
        local_ctx: LocalCodegenContext<'gen>,
    ) -> Result<Option<BasicValueEnum<'gen>>, Box<dyn Error>> {
        let res = match init {
            TypedExpression {
                ty,
                expr: TypedExpressionData::BinaryOp { left, right, op },
            } => Some(self.codegen_binary_op(left, right, op, local_ctx)?),
            TypedExpression {
                ty,
                expr: TypedExpressionData::LogicalOp { left, right, op },
            } => {
                let Some(left) = self.codegen_expression(left, local_ctx.clone())? else {
                    return Err("Expected value from expression".into());
                };
                let Some(right) = self.codegen_expression(right, local_ctx.clone())? else {
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
                ty,
                expr: TypedExpressionData::UnaryOp { expr, op },
            } => match op {
                Operator::Plus => self.codegen_expression(expr, local_ctx.clone())?,
                Operator::Minus => {
                    // Negate

                    let Some(expr) = self.codegen_expression(expr, local_ctx.clone())? else {
                        return Err("Expected value from expression".into());
                    };

                    let neg = match expr {
                        BasicValueEnum::ArrayValue(_) => todo!(),
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
                        BasicValueEnum::PointerValue(_) => todo!(),
                        BasicValueEnum::StructValue(_) => todo!(),
                        BasicValueEnum::VectorValue(_) => todo!(),
                    };

                    Some(neg)
                }
                Operator::Times => {
                    // Dereference

                    let Some(expr) = self.codegen_expression(expr, local_ctx.clone())? else {
                        return Err("Expected value from expression".into());
                    };

                    let deref_load = self
                        .ctx
                        .ir_builder
                        .build_load(expr.into_pointer_value(), "deref");

                    Some(deref_load)
                }
                Operator::Not => todo!(),
                _ => return Err(format!("Invalid unary operator: {:?}", op).into()),
            },
            TypedExpression {
                ty,
                expr: TypedExpressionData::Identifier { name },
            } => Some(self.codegen_identifier(name, local_ctx)?),
            TypedExpression {
                ty,
                expr: TypedExpressionData::Literal { literal },
            } => Some(self.codegen_literal(literal, local_ctx)?),
            TypedExpression {
                ty,
                expr: TypedExpressionData::If { expr },
            } => {
                let TypedIfExpression {
                    condition,
                    body,
                    else_body,
                    yield_ty,
                } = expr;

                let Some(condition) = self.codegen_expression(condition, local_ctx.clone())? else {
                    return Err("Expected value from expression".into());
                };

                let Some(func) = local_ctx.current_fn else {
                    return Err("Expected function".into());
                };

                let current_block = self.ctx.ir_builder.get_insert_block().unwrap();

                let end_block = self.ctx.llvm_ctx.append_basic_block(func, "end");

                let then_block = self.ctx.llvm_ctx.prepend_basic_block(end_block, "then");
                self.ctx.ir_builder.position_at_end(then_block);
                let then_result = self.codegen_expression(body, local_ctx.clone())?;
                self.ctx.ir_builder.build_unconditional_branch(end_block);

                let (else_block, else_result) = if let Some(else_body) = else_body.as_ref() {
                    let else_block = self.ctx.llvm_ctx.prepend_basic_block(end_block, "else");
                    self.ctx.ir_builder.position_at_end(else_block);
                    let else_result = self.codegen_expression(else_body, local_ctx)?;
                    self.ctx.ir_builder.build_unconditional_branch(end_block);
                    (else_block, else_result)
                } else {
                    (end_block, None)
                };

                self.ctx.ir_builder.position_at_end(end_block);
                let res = if let Some(ty) = &body.ty {
                    let phi = self
                        .ctx
                        .ir_builder
                        .build_phi(self.ctx.to_llvm_basic_ty(&ty.sig())?, "if_result");
                    phi.add_incoming(&[
                        (&then_result.unwrap(), then_block),
                        (&else_result.unwrap(), else_block),
                    ]);
                    Some(phi.as_basic_value())
                } else {
                    None
                };

                self.ctx.ir_builder.position_at_end(current_block);
                self.ctx.ir_builder.build_conditional_branch(
                    condition.into_int_value(),
                    then_block,
                    else_block,
                );

                self.ctx.ir_builder.position_at_end(end_block);

                res
            }
            TypedExpression {
                ty,
                expr: TypedExpressionData::Block { block },
            } => {
                let mut res = None;
                let mut block_scope = local_ctx.clone();
                for statement in &block.statements {
                    res = self.codegen_statement(statement, &mut block_scope)?;
                }
                res
            }
            TypedExpression {
                ty,
                expr: TypedExpressionData::VarAssignment { var_assign },
            } => Some(self.codegen_var_assignment(var_assign, local_ctx)?),
            TypedExpression {
                ty,
                expr: TypedExpressionData::FnCall { fn_call },
            } => self.codegen_fn_call(fn_call, local_ctx)?,
            TypedExpression {
                ty,
                expr: TypedExpressionData::MemberAccess { member_access },
            } => Some(self.codegen_member_access(member_access, local_ctx)?),
            TypedExpression {
                ty,
                expr: TypedExpressionData::StructInitializer { struct_init },
            } => Some(self.codegen_struct_init(struct_init, local_ctx)?),
        };
        Ok(res)
    }

    fn codegen_literal(
        &self,
        literal: &'gen TypedLiteral<'gen>,
        local_ctx: LocalCodegenContext<'gen>,
    ) -> Result<BasicValueEnum<'gen>, Box<dyn Error>> {
        let t = match literal {
            TypedLiteral {
                ty,
                literal: Literal::Str(s, pos),
            } => {
                let s = self
                    .ctx
                    .ir_builder
                    .build_global_string_ptr(s, "str")
                    .as_pointer_value();
                s.as_basic_value_enum()
            }
            TypedLiteral {
                ty,
                literal: Literal::Int(i, pos),
            } => {
                let Some(ty) = local_ctx.get_base_type(&ty.sig()) else {
                    return Err(format!("Type not found: {:?} {}:{}", ty, file!(), line!()).into());
                };
                let ty = ty.clone().borrow_mut().llvm_ty.clone();
                let (value, sign) = if *i < 0 {
                    (i.abs() as u64, true)
                } else {
                    (*i as u64, false)
                };

                ty.int_type()?.const_int(value, sign).as_basic_value_enum()
            }
            TypedLiteral {
                ty,
                literal: Literal::Float(f, pos),
            } => {
                let Some(ty) = local_ctx.get_base_type(&ty.sig()) else {
                    return Err(format!("Type not found: {:?} {}:{}", ty, file!(), line!()).into());
                };
                let ty = ty.clone().borrow_mut().llvm_ty.clone();
                ty.float_type()?.const_float(*f).as_basic_value_enum()
            }
            TypedLiteral {
                ty,
                literal: Literal::Char(c, pos),
            } => todo!(),
            TypedLiteral {
                ty,
                literal: Literal::Bool(b, pos),
            } => {
                let Some(ty) = local_ctx.get_base_type(&ty.sig()) else {
                    return Err(format!("Type not found: {:?} {}:{}", ty, file!(), line!()).into());
                };
                let ty = ty.clone().borrow_mut().llvm_ty.clone();

                ty.int_type()?
                    .const_int(*b as u64, false)
                    .as_basic_value_enum()
            }
        };
        Ok(t)
    }

    fn codegen_struct(
        &self,
        structure: &'gen TypedStructDeclaration<'gen>,
        local_ctx: &mut LocalCodegenContext<'gen>,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let TypedStructDeclaration { name, fields, ty } = structure;
        let structure = self.ctx.llvm_ctx.opaque_struct_type(name);
        let fields = fields
            .iter()
            .map(|f| {
                let sig = f.ty.sig();
                let basic = self.ctx.to_llvm_basic_ty(&sig);
                basic
            })
            .collect::<Result<Vec<_>, String>>()?;
        let fields_slice = fields.as_slice();
        structure.set_body(fields_slice, false);

        local_ctx.types.insert(
            ty.sig(),
            Rc::new(CodegenType {
                name: name.clone(),
                ty: ty.sig(),
                llvm_ty: CodegenLLVMType::Basic(structure.as_basic_type_enum()),
            }),
        );

        // add struct to context

        /* let Some(s) = self.ctx.llvm_ctx.get_struct_type(name) else {
            return Err(format!("Type not found: {:?} {}:{}", ty, file!(), line!()).into());
        }; */

        // add struct to module
        /* self.ctx.llvm_module.get_struct_type(name); */

        Ok(())
    }

    fn codegen_return_statement(
        &self,
        ret_stmt: &'gen TypedReturnStatement<'gen>,
        local_ctx: LocalCodegenContext<'gen>,
    ) -> Result<(), Box<dyn Error>> {
        if let Some(ret_val) = &ret_stmt.value {
            let val = self.codegen_expression(ret_val, local_ctx.clone())?;
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
            let name = name.clone();
            let var = self.ctx.ir_builder.build_load(alloc, &name.name);
            Ok(var)
        }
    }

    fn codegen_var_assignment(
        &self,
        var_assign: &'gen TypedVarAssignment<'gen>,
        local_ctx: LocalCodegenContext<'gen>,
    ) -> Result<BasicValueEnum<'gen>, Box<dyn Error>> {
        match &var_assign.left.expr {
            TypedExpressionData::Identifier { name } => {
                let right = self.codegen_expression(&var_assign.right, local_ctx.clone())?;
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
                    self.ctx
                        .ir_builder
                        .build_alloca(name.ty.llvm_ty.basic()?, &name.name)
                };

                self.ctx.ir_builder.build_store(alloc, right);
                let load = self.ctx.ir_builder.build_load(alloc, &name.name);
                Ok(load)
            }
            TypedExpressionData::MemberAccess { member_access } => {
                let TypedMemberAccess {
                    object,
                    member,
                    computed,
                } = member_access;

                let mut members = vec![member];
                let mut curr_obj = object;
                while let TypedExpressionData::MemberAccess { member_access } = &curr_obj.expr {
                    let TypedMemberAccess {
                        object,
                        member,
                        computed,
                    } = member_access;
                    members.push(member);
                    curr_obj = object;
                }
                let var = if let TypedExpressionData::Identifier { name } = &curr_obj.expr {
                    let Some(var) = local_ctx.get_name(&name) else {
                        return Err(format!("Name not found: {}", &name).into());
                    };
                    var
                } else {
                    return Err("generator.rs:520 Invalid member access".into());
                };

                let Some(alloc) = var.alloc else {
                    return Err(format!("Name alloc not found: {}", var.name).into());
                };

                let mut curr_obj = alloc;

                for member in members {
                    let TypedExpressionData::Identifier { name } = &member.expr else {
                        return Err("generator.rs:528 Invalid member access".into());
                    };
                    let ty = var.ty.to_struct_type()?;
                    let Some(field) = ty.fields.get(name) else {
                        return Err(format!("Field not found: {}", name).into());
                    };
                    let Ok(gep) =
                        self.ctx
                            .ir_builder
                            .build_struct_gep(curr_obj, field.idx, &field.name) else {
                                return Err(format!("GEP not found: {}", field.name).into());
                            };
                    curr_obj = gep;
                }

                let right = self.codegen_expression(&var_assign.right, local_ctx.clone())?;
                let Some(right) = right else {
                    return Err("Right side of assignment is None".into());
                };
                println!("E1");
                self.ctx.ir_builder.build_store(curr_obj, right);
                println!("E2");
                let load = self.ctx.ir_builder.build_load(alloc, "var_assign_result");
                println!("E3");
                Ok(load)
            }
            TypedExpressionData::UnaryOp { expr, op } => {
                // left hand side dereference
                let Some(right) = self.codegen_expression(&var_assign.right, local_ctx.clone())? else {
                    return Err("Right side of assignment is None".into());
                };

                let Some(left) = self.codegen_expression(&expr, local_ctx.clone())? else {
                    return Err("Left side of assignment is None".into());
                };

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

    fn codegen_fn_call(
        &self,
        fn_call: &'gen TypedFunctionCall<'gen>,
        local_ctx: LocalCodegenContext<'gen>,
    ) -> Result<Option<BasicValueEnum<'gen>>, Box<dyn Error>> {
        let callee = match *fn_call.callee.clone() {
            TypedExpression {
                ty,
                expr: TypedExpressionData::Identifier { name },
            } => name,
            _ => unreachable!(),
        };

        let callee = self
            .ctx
            .llvm_module
            .get_function(&callee)
            .ok_or_else(|| format!("Callee {} not found", callee))?;

        let mut args = Vec::new();
        for arg in &fn_call.args {
            let arg = self.codegen_expression(arg, local_ctx.clone())?;
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

        let call = self
            .ctx
            .ir_builder
            .build_call(callee, args.as_slice(), "calltmp");

        if let Some(ret) = call.try_as_basic_value().left() {
            Ok(Some(ret))
        } else {
            Ok(None)
        }
    }

    fn codegen_binary_op(
        &self,
        left: &'gen TypedExpression<'gen>,
        right: &'gen TypedExpression<'gen>,
        op: &Operator,
        local_ctx: LocalCodegenContext<'gen>,
    ) -> Result<BasicValueEnum<'gen>, Box<dyn Error>> {
        let left = self.codegen_expression(left, local_ctx.clone())?;
        let right = self.codegen_expression(right, local_ctx.clone())?;
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
            _ => todo!(),
        };
        Ok(result)
    }

    fn codegen_struct_init(
        &self,
        struct_init: &'gen TypedStructInitializer<'gen>,
        local_ctx: LocalCodegenContext<'gen>,
    ) -> Result<BasicValueEnum<'gen>, Box<dyn Error>> {
        let struct_ty = &struct_init.struct_ty;
        let sig = struct_ty.sig();
        let Some(struct_ty) = local_ctx.get_base_type(&sig) else {
            return Err(format!("Type not found {}:{}", file!(), line!()).into());
        };

        let mut values = Vec::new();
        for (i, field) in struct_init.fields.iter().enumerate() {
            let Some(field) = self.codegen_expression(&field.value, local_ctx.clone())? else {
                return Err("Field is None".into());
            };
            values.push(field);
        }

        /* let init = match self.ctx.llvm_ctx.get_struct_type(&struct_ty.name) {
            Some(s) => s,
            None => struct_ty.llvm_ty.struct_type()?,
        }; */
        /* self.ctx.llvm_module.get_context().
        let init = self.ctx.llvm_ctx.const_struct(values.as_slice(), false); */
        //let init = struct_ty.llvm_ty.struct_type()?;

        let init = struct_ty
            .llvm_ty
            .struct_type()?
            .const_named_struct(values.as_slice());

        Ok(init.as_basic_value_enum())
    }

    fn codegen_member_access(
        &self,
        member_access: &'gen TypedMemberAccess<'gen>,
        local_ctx: LocalCodegenContext<'gen>,
    ) -> Result<BasicValueEnum<'gen>, Box<dyn Error>> {
        /* let object = self.codegen_expression(&member_access.object, local_ctx.clone())?;
        let Some(object) = object else {
            return Err("Object is None".into());
        }; */
        match &member_access.object.expr {
            TypedExpressionData::Identifier { name } => {
                let Some(object_reg) = local_ctx.get_name(&name) else {
                    return Err(format!("Name not found: {}", &name).into());
                };
                match &object_reg.ty.ty {
                    TypeSignature::Struct(Some(s)) => {
                        let member = (*member_access.member).clone();
                        match member.expr {
                            TypedExpressionData::Identifier { name } => {
                                let Some(member) = s.fields.get(&name) else {
                                    return Err(format!("Member not found: {}", &name).into());
                                };
                                //let member_ty = member.ty.sig();
                                /* let Some(member_ty) = local_ctx.get_type(&member_ty) else {
                                    return Err(format!("Type not found").into());
                                };
                                let Some(member_reg) = local_ctx.names.get(&name) else {
                                    return Err(format!("Name not found").into());
                                }; */
                                let alloc = if let Some(alloc) = object_reg.alloc {
                                    alloc
                                } else {
                                    return Err(format!("Name not allocated").into());
                                };

                                let Some(get_member) = self
                                    .ctx
                                    .ir_builder
                                    .build_struct_gep(alloc, member.idx, &member.name)
                                    .ok() else {
                                        return Err(format!("Member not found").into());
                                    };
                                //let load = self.ctx.ir_builder.build_load(get_member, &member.name);
                                Ok(get_member.as_basic_value_enum())
                            }
                            _ => unreachable!(),
                        }
                    }
                    _ => unreachable!(),
                }

                /* let alloc = if let Some(alloc) = name.alloc {
                    alloc
                } else {
                    self.ctx
                        .ir_builder
                        .build_alloca(to_basic(name.ty.llvm_ty)?, &name.name)
                };

                let load = self.ctx.ir_builder.build_load(alloc, &name.name);
                Ok(load) */
            }
            TypedExpressionData::MemberAccess {
                member_access: obj_member_access,
            } => {
                let obj_access =
                    self.codegen_member_access(obj_member_access, local_ctx.clone())?;

                let Some(ty) = &obj_member_access.object.ty else {
                    return Err("Type is None".into());
                };
                let sig = ty.sig();
                let Some(ty) = local_ctx.get_base_type(&sig) else {
                    return Err(format!("Type not found {}:{}", file!(), line!()).into());
                };
                if let TypeSignature::Struct(Some(s)) = sig {
                    let member = (*member_access.member).clone();
                    match member.expr {
                        TypedExpressionData::Identifier { name } => {
                            let Some(member) = s.fields.get(&name) else {
                                return Err(format!("Member not found: {}", &name).into());
                            };
                            //let member_ty = member.ty.sig();
                            /* let Some(member_ty) = local_ctx.get_type(&member_ty) else {
                                return Err(format!("Type not found").into());
                            };
                            let Some(member_reg) = local_ctx.names.get(&name) else {
                                return Err(format!("Name not found").into());
                            }; */
                            let Some(get_member) = self
                                .ctx
                                .ir_builder
                                .build_struct_gep(obj_access.into_pointer_value(), member.idx, &member.name)
                                .ok() else {
                                    return Err(format!("Member not found").into());
                                };
                            //let load = self.ctx.ir_builder.build_load(get_member, &member.name);
                            Ok(get_member.as_basic_value_enum())
                        }
                        _ => unreachable!(),
                    }
                } else {
                    unreachable!()
                }
            }
            _ => unimplemented!(),
        }
    }

    fn codegen_yield_statement(
        &self,
        yield_stmt: &'gen TypedYieldStatement<'gen>,
        local_ctx: LocalCodegenContext<'gen>,
    ) -> Result<BasicValueEnum<'gen>, Box<dyn Error>> {
        let value = self.codegen_expression(&yield_stmt.value, local_ctx.clone())?;
        let Some(value) = value else {
            return Err("Value is None".into());
        };
        Ok(value)
    }
}
