use std::{borrow::BorrowMut, collections::HashMap, error::Error, rc::Rc};

use inkwell::{
    module::Linkage,
    types::BasicType,
    values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, PointerValue},
    AddressSpace,
};

use crate::{
    debug, debugln,
    tokenizer::Literal,
    typechecker::{
        typed_ast::{
            TypedExpression, TypedExpressionData, TypedFunctionCall, TypedLiteral,
            TypedVarAssignment,
        },
        TypeCheckModule,
    },
};
use crate::{
    tokenizer::Operator,
    typechecker::{
        context::TypeSig,
        type_sig::TypeSignature,
        typed_ast::{
            TypedDeclaration, TypedFunctionDeclaration, TypedFunctionDefinition, TypedIfExpression,
            TypedMemberAccess, TypedResultStatement, TypedReturnStatement, TypedStatement,
            TypedStructDeclaration, TypedStructInitializer, TypedVarDeclaration,
        },
    },
};

use super::context::{
    CodegenContext, CodegenLLVMType, CodegenName, CodegenType, LocalCodegenContext,
};

pub struct CodeGenerator<'gen> {
    pub ctx: CodegenContext<'gen>,
}

impl<'gen> CodeGenerator<'gen> {
    pub fn new(
        tc_mod: &'gen TypeCheckModule<'gen>,
        llvm_ctx: &'gen inkwell::context::Context,
    ) -> Self {
        debug!("generator::CodeGenerator::new");
        let module = llvm_ctx.create_module("main");
        let builder = llvm_ctx.create_builder();
        Self {
            ctx: CodegenContext::new(llvm_ctx, module, builder, tc_mod),
        }
    }
    pub fn codegen_module(
        &self,
    ) -> Result<inkwell::module::Module<'gen>, Box<dyn std::error::Error>> {
        debug!("generator::CodeGenerator::codegen_module");
        let mut local_ctx = LocalCodegenContext {
            names: HashMap::new(),
            types: HashMap::new(),
            return_type: None,
            result_type: None,
            current_fn: None,
            structs: HashMap::new(),
        };

        self.ctx.add_primitive_types(&mut local_ctx);
        local_ctx.structs = self.ctx.tc_mod.ctx.structs.clone();

        /*local_ctx.types = self
        .ctx
        .tc_mod
        .ctx
        .types
        .iter()
        .map(|(k, v)| {
            let llvm_t = None; //self.ctx.to_llvm_ty(ty, local_ctx);
            (
                v.sig(),
                Rc::new(CodegenType::new(k.clone(), v.sig(), llvm_t)),
            )
        })
        .collect(); */

        // Codegen structs
        for decl in &self.ctx.tc_mod.module.body {
            if let TypedDeclaration::Struct(structure) = decl {
                self.codegen_struct(structure, &mut local_ctx)?;
            }
        }

        for (name, name_ctx) in &self.ctx.tc_mod.ctx.names {
            let t = name_ctx.ty.sig();
            let t = if let TypeSignature::Function(fn_t) = t.clone() {
                Rc::from(CodegenType::new(
                    name.clone(),
                    t,
                    Some(self.ctx.function_to_llvm_ty(&fn_t, &local_ctx.structs)?),
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

    fn codegen_struct(
        &self,
        structure: &'gen TypedStructDeclaration<'gen>,
        local_ctx: &mut LocalCodegenContext<'gen>,
    ) -> Result<(), Box<dyn std::error::Error>> {
        debug!("generator::CodeGenerator::codegen_struct");
        let TypedStructDeclaration {
            name,
            fields,
            ty,
            id,
        } = structure;
        let struct_t = self.ctx.to_llvm_ty(&ty.sig(), &local_ctx.structs)?;
        println!("struct_t: {:?}", struct_t);
        /* let structure = self.ctx.llvm_ctx.opaque_struct_type(name);
        let mut fds = Vec::new();
        for field in fields {
            let sig = field.ty.sig();
            let basic = self.ctx.to_llvm_ty(&sig, &local_ctx.structs)?;
            fds.push(basic.basic()?);
        }

        let fields_slice = fds.as_slice();
        structure.set_body(fields_slice, false); */

        local_ctx.types.insert(
            ty.sig(),
            Rc::new(CodegenType {
                name: name.clone(),
                ty: ty.sig(),
                llvm_ty: Some(struct_t),
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

    fn codegen_fn_decl(
        &self,
        func: &'gen TypedFunctionDeclaration,
        local_ctx: LocalCodegenContext<'gen>,
    ) -> Result<(), Box<dyn Error>> {
        debug!("generator::CodeGenerator::codegen_fn_decl");
        let Some(func_reg) = local_ctx.names.get(&func.name) else {
            return Err(format!("Function not found: {}", func.name).into());
        };

        let Some(llvm_ty) = func_reg.ty.llvm_ty.clone() else {
            return Err(format!("Function not found: {}", func.name).into());
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
        func: &'gen TypedFunctionDefinition<'gen>,
        mut local_ctx: LocalCodegenContext<'gen>,
    ) -> Result<(), Box<dyn std::error::Error>> {
        debug!("generator::CodeGenerator::codegen_fn_def");
        let Some(func_reg) = local_ctx.names.get(&func.name) else {
            return Err(format!("Function not found: {}", func.name).into());
        };

        let Some(llvm_ty) = func_reg.ty.llvm_ty.clone() else {
            return Err(format!("Function not found: {}", func.name).into());
        };

        let llvm_fn =
            self.ctx
                .llvm_module
                .add_function(&func.name.clone(), llvm_ty.function()?, None);

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
        debug!("generator::CodeGenerator::codegen_statement");
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
            TypedStatement::Result(result_stmt) => Ok(Some(
                self.codegen_result_statement(result_stmt, local_ctx.clone())?,
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
        debug!("generator::CodeGenerator::codegen_variable_statement");
        let type_sig = var_stmt.ty.sig();

        let Some(base_type) = local_ctx.get_base_type(&type_sig) else {
            return Err(format!("Base Type not found: {:?} {}:{}", var_stmt.ty, file!(), line!()).into());
        };
        let base_type_name = base_type.name.clone();

        /* let Some(base_llvm_type) = self.ctx.llvm_ctx.get_struct_type(&base_type_name) else {
            return Err(format!("Struct Type not found: {:?} {}:{}", &base_type_name, file!(), line!()).into());
        }; */
        let base_llvm_type = self.ctx.to_llvm_ty(&type_sig, &local_ctx.structs)?;

        let ptr_depth = local_ctx.get_ptr_depth(&type_sig);

        let var_type = if ptr_depth > 0 {
            Rc::new(CodegenType {
                name: type_sig.string_repr(),
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
        var_name.alloc = Some(store_ptr.clone());
        local_ctx.add_name(var_name);

        if let Some(init) = &var_stmt.initializer {
            let init = self.codegen_expression(init, local_ctx.clone())?;
            let Some(init) = init else {
                return Err("Expected initializer".into());
            };
            debugln!();
            self.ctx.ir_builder.build_store(store_ptr, init);
        }

        Ok(())
    }

    fn codegen_expression(
        &self,
        init: &'gen TypedExpression<'gen>,
        local_ctx: LocalCodegenContext<'gen>,
    ) -> Result<Option<BasicValueEnum<'gen>>, Box<dyn Error>> {
        debug!("generator::CodeGenerator::codegen_expression");
        let res = match init {
            TypedExpression {
                ty: _,
                expr: TypedExpressionData::BinaryOp { left, right, op },
            } => Some(self.codegen_binary_op(left, right, op, local_ctx)?),
            TypedExpression {
                ty: _,
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
                ty: _,
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
                ty: _,
                expr: TypedExpressionData::Identifier { name },
            } => Some(self.codegen_identifier(name, local_ctx)?),
            TypedExpression {
                ty: _,
                expr: TypedExpressionData::Literal { literal },
            } => Some(self.codegen_literal(literal, local_ctx)?),
            TypedExpression {
                ty: _,
                expr: TypedExpressionData::If { expr },
            } => {
                let TypedIfExpression {
                    condition,
                    body,
                    else_body,
                    result_ty: _,
                } = expr;

                let Some(condition) = self.codegen_expression(condition, local_ctx.clone())? else {
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
                let then_result = self.codegen_expression(body, local_ctx.clone())?;
                self.ctx.ir_builder.build_unconditional_branch(end_block);
                let then_block = self.ctx.ir_builder.get_insert_block().unwrap();

                self.ctx.ir_builder.position_at_end(else_block);

                let else_result = if let Some(else_body) = else_body.as_ref() {
                    let else_result = self.codegen_expression(else_body, local_ctx.clone())?;
                    self.ctx.ir_builder.build_unconditional_branch(end_block);
                    else_block = self.ctx.ir_builder.get_insert_block().unwrap();
                    else_result
                } else {
                    self.ctx.ir_builder.build_unconditional_branch(end_block);
                    None
                };

                self.ctx.ir_builder.position_at_end(end_block);
                let res = if let Some(ty) = &body.ty {
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
                ty: _,
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
                ty: _,
                expr: TypedExpressionData::VarAssignment { var_assign },
            } => Some(self.codegen_var_assignment(var_assign, local_ctx)?),
            TypedExpression {
                ty: _,
                expr: TypedExpressionData::FnCall { fn_call },
            } => self.codegen_fn_call(fn_call, local_ctx)?,
            TypedExpression {
                ty: _,
                expr: TypedExpressionData::MemberAccess { member_access },
            } => Some(self.codegen_member_access(member_access, local_ctx)?),
            TypedExpression {
                ty: _,
                expr: TypedExpressionData::StructInitializer { struct_init },
            } => Some(self.codegen_struct_init(struct_init, local_ctx)?),
            TypedExpression {
                ty: _,
                expr: TypedExpressionData::SizeOfExpr { ty },
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
        literal: &'gen TypedLiteral<'gen>,
        local_ctx: LocalCodegenContext<'gen>,
    ) -> Result<BasicValueEnum<'gen>, Box<dyn Error>> {
        debug!("generator::CodeGenerator::codegen_literal");
        let t = match literal {
            TypedLiteral {
                ty: _,
                literal: Literal::Str(s, _pos),
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
                    return Err(format!("Type not found: {:?} {}", ty, pos).into());
                };
                let Some(ty) = ty.clone().borrow_mut().llvm_ty.clone() else {
                    return Err(format!("Type not found: {:?} {}", ty, pos).into());
                };
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
                    return Err(format!("Type not found: {:?} {pos}", ty).into());
                };
                let Some(ty) = ty.clone().borrow_mut().llvm_ty.clone() else {
                    return Err(format!("Type not found: {:?} {}", ty, pos).into());
                };
                ty.float_type()?.const_float(*f).as_basic_value_enum()
            }
            TypedLiteral {
                ty: _,
                literal: Literal::Char(_c, _pos),
            } => todo!(),
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
                    .const_int(*b as u64, false)
                    .as_basic_value_enum()
            }
        };
        Ok(t)
    }

    fn codegen_return_statement(
        &self,
        ret_stmt: &'gen TypedReturnStatement<'gen>,
        local_ctx: LocalCodegenContext<'gen>,
    ) -> Result<(), Box<dyn Error>> {
        debug!("generator::CodeGenerator::codegen_return_statement");
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
        debug!("generator::CodeGenerator::codegen_identifier");
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
        debug!("generator::CodeGenerator::codegen_var_assignment");
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
                    let Some(llvm_ty) = &name.ty.llvm_ty else {
                        return Err(format!("LLVM type not found: {}", &name.name).into());
                    };
                    self.ctx
                        .ir_builder
                        .build_alloca(llvm_ty.basic()?, &name.name)
                };

                debugln!();
                self.ctx.ir_builder.build_store(alloc, right);
                let load = self.ctx.ir_builder.build_load(alloc, &name.name);
                Ok(load)
            }
            TypedExpressionData::MemberAccess { member_access } => {
                let TypedMemberAccess {
                    object,
                    member,
                    computed: _,
                } = member_access;
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

                let Some(right) = self.codegen_expression(&var_assign.right, local_ctx.clone())? else {
                    return Err("Right side of assignment is None".into());
                };

                let Some(object) = self.codegen_expression(object, local_ctx.clone())? else {
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

                debugln!();
                let _store = self.ctx.ir_builder.build_store(member_ptr, right);

                Ok(member_ptr.as_basic_value_enum())
            }
            TypedExpressionData::UnaryOp { expr, op: _ } => {
                // left hand side dereference
                let Some(right) = self.codegen_expression(&var_assign.right, local_ctx.clone())? else {
                    return Err("Right side of assignment is None".into());
                };

                let Some(left) = self.codegen_lhs_expression(expr.as_ref(), local_ctx)? else {
                    return Err("Left side of assignment is None".into());
                };

                let Some(size) = right.get_type().size_of() else {
                    return Err("Size of right is None".into());
                };

                if let BasicValueEnum::PointerValue(p) = right {
                    if p.is_const() {
                        self.ctx.ir_builder.build_memcpy(
                            left,
                            64,
                            right.into_pointer_value(),
                            64,
                            size,
                        )?;
                        println!("after 2");
                        let load = self.ctx.ir_builder.build_load(left, "deref_assign_result");
                        return Ok(load);
                    }
                }
                println!("left: {:#?}", left);
                println!("right: {:#?}", right);
                self.ctx.ir_builder.build_store(left, right);
                let load = self.ctx.ir_builder.build_load(left, "deref_assign_result");
                Ok(load)
            }
            _ => return Err("Invalid left side of assignment".into()),
        }
    }

    pub(crate) fn codegen_lhs_expression(
        &self,
        expr: &'gen TypedExpression<'gen>,
        local_ctx: LocalCodegenContext<'gen>,
    ) -> Result<Option<PointerValue<'gen>>, Box<dyn Error>> {
        debug!("generator::CodeGenerator::codegen_lhs_expression");
        match &expr.expr {
            TypedExpressionData::UnaryOp { expr, op } => {
                /* let Some(val) = self.codegen_expression(expr, local_ctx.clone())? else {
                    return Err("Invalid left side of assignment".into());
                }; */

                match op {
                    Operator::Times => Ok(self.codegen_lhs_expression(expr, local_ctx)?), //Ok(Some(val.into_pointer_value())),
                    _ => Err("Invalid left side of assignment".into()),
                }
            }
            TypedExpressionData::Identifier { name: _name } => {
                /* let Some(name) = local_ctx.get_name(&name) else {
                    return Err(format!("Name not found: {}", &name).into());
                };
                let name = name.clone();

                let alloc = if let Some(alloc) = name.alloc {
                    alloc
                } else {
                    self.ctx
                        .ir_builder
                        .build_alloca(name.ty.llvm_ty.pointer_type()?, &name.name)
                };

                let alloc = self
                    .ctx
                    .ir_builder
                    .build_load(alloc, &name.name)
                    .into_pointer_value();

                Ok(Some(alloc)) */
                let Some(val) = self.codegen_expression(&expr, local_ctx)? else {
                    return Err("Expression is None".into());
                };
                Ok(Some(val.into_pointer_value()))
            }
            TypedExpressionData::MemberAccess { member_access } => {
                let val = self.codegen_lhs_member_access(member_access, local_ctx)?;
                Ok(Some(val.into_pointer_value()))
            }
            /* Ok(Some(
                self.codegen_member_access(member_access, local_ctx)?
                    .into_pointer_value(),
            )) */,
            _ => Err("Invalid left side of assignment".into()),
        }
    }

    fn codegen_lhs_member_access(
        &self,
        member_access: &'gen TypedMemberAccess<'gen>,
        local_ctx: LocalCodegenContext<'gen>,
    ) -> Result<BasicValueEnum<'gen>, Box<dyn Error>> {
        debug!("generator::CodeGenerator::codegen_lhs_member_access");
        let TypedMemberAccess {
            object,
            member,
            computed: _,
        } = member_access;

        let Some(object_type) = &object.ty else {
            return Err("Object type is None".into());
        };
        let object_type = object_type.sig();
        let Some(object) = self.codegen_expression(object, local_ctx.clone())? else {
            return Err("Object is None".into());
        };

        match &object_type {
            TypeSignature::Primitive(_primitive) => todo!(),
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
                    _ => todo!(),
                }
            }
            TypeSignature::Function(_function) => todo!(),
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
                            let Some(member_idx) = s.get_member_idx(&name) else {
                                return Err(format!("Member not found: {}", &name).into());
                            };
                            let Some(member) = self.ctx.ir_builder.build_struct_gep(
                                object.into_pointer_value(),
                                member_idx,
                                (s.name.clone() + "." + &name).as_str(),
                            ).ok() else {
                                return Err("Member is None".into());
                            };
                            Ok(member.as_basic_value_enum())
                        }
                        _ => todo!(),
                    }
                } else {
                    todo!("codegen_member_access")
                }
            }
            _ => unreachable!(),
        }
    }

    fn codegen_fn_call(
        &self,
        fn_call: &'gen TypedFunctionCall<'gen>,
        local_ctx: LocalCodegenContext<'gen>,
    ) -> Result<Option<BasicValueEnum<'gen>>, Box<dyn Error>> {
        debug!("generator::CodeGenerator::codegen_fn_call");
        let callee = match *fn_call.callee.clone() {
            TypedExpression {
                ty: _,
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
        debug!("generator::CodeGenerator::codegen_binary_op");
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
        debug!("generator::CodeGenerator::codegen_struct_init");
        /* let struct_ty = &struct_init.struct_ty;
        let sig = struct_ty.sig();
        let Some(struct_ty) = local_ctx.get_base_type(&sig) else {
            return Err(format!("Type not found {}:{}", file!(), line!()).into());
        }; */
        let Some(struct_ty) = local_ctx.get_type_by_name(&struct_init.struct_name.clone()) else {
            return Err("Struct type not found".into());
        };
        println!("STRUCT TY: {:?}", struct_ty);

        let mut values = Vec::new();
        for field in struct_init.fields.iter() {
            let Some(field) = self.codegen_expression(&field.value, local_ctx.clone())? else {
                return Err("Field is None".into());
            };
            values.push(field);
        }

        let Some(llvm_ty) = &struct_ty.llvm_ty else {
            return Err("Invalid struct type".into());
        };
        println!("LLVM TY: {:?}", llvm_ty);
        let init = llvm_ty.struct_type()?.const_named_struct(values.as_slice());
        let global = self.ctx.llvm_module.add_global(
            llvm_ty.struct_type()?,
            Some(AddressSpace::Generic),
            (struct_init.struct_name.clone() + ".init").as_str(),
        );
        global.set_initializer(&init);
        global.set_constant(true);

        Ok(global.as_pointer_value().as_basic_value_enum())
    }

    fn codegen_member_access(
        &self,
        member_access: &'gen TypedMemberAccess<'gen>,
        local_ctx: LocalCodegenContext<'gen>,
    ) -> Result<BasicValueEnum<'gen>, Box<dyn Error>> {
        debug!("generator::CodeGenerator::codegen_member_access");
        let TypedMemberAccess {
            object,
            member,
            computed: _,
        } = member_access;

        let Some(object_type) = &object.ty else {
            return Err("Object type is None".into());
        };
        let object_type = object_type.sig();
        let Some(object) = self.codegen_expression(object, local_ctx.clone())? else {
            return Err("Object is None".into());
        };

        match &object_type {
            TypeSignature::Primitive(_primitive) => todo!(),
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
                    _ => todo!(),
                }
            }
            TypeSignature::Function(_function) => todo!(),
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
                            let Some(member_idx) = s.get_member_idx(&name) else {
                                return Err(format!("Member not found: {}", &name).into());
                            };
                            let Some(member) = self.ctx.ir_builder.build_struct_gep(
                                object.into_pointer_value(),
                                member_idx,
                                name.as_str(),
                            ).ok() else {
                                return Err("Member is None".into());
                            };
                            Ok(member.as_basic_value_enum())
                        }
                        _ => todo!(),
                    }
                } else {
                    todo!("codegen_member_access")
                }
            }
            _ => unreachable!(),
        }

        //
        //

        /*
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
                                    return Err(format!("Member not found: {1} {0}", line!() , &name).into());
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
                                        return Err(format!("Member not found ({0})", line!() ).into());
                                    };
                                //let load = self.ctx.ir_builder.build_load(get_member, &member.name);
                                Ok(get_member.as_basic_value_enum())
                            }
                            _ => unreachable!(),
                        }
                    }
                    TypeSignature::Pointer(p) => {
                        let target = &p.target;
                        match target.as_ref() {
                            TypeSignature::Struct(Some(s)) => {
                                let member = (*member_access.member).clone();
                                match member.expr {
                                    TypedExpressionData::Identifier { name } => {
                                        println!("Struct: {:?}", s);
                                        println!("Member access: {}", name);
                                        let Some(member) = s.fields.get(&name) else {
                                            return Err(format!("Codegen error: Member not found: {}", &name).into());
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

                                        /* let Some(get_member) = self
                                        .ctx
                                        .ir_builder
                                        .build_struct_gep(alloc, member.idx, &member.name)
                                        .ok() else {
                                            return Err(format!("Failed to build GEP ({0})", line!() ).into());
                                        }; */
                                        let get_member = alloc;
                                        //let load = self.ctx.ir_builder.build_load(get_member, &member.name);
                                        Ok(get_member.as_basic_value_enum())
                                    }
                                    _ => unreachable!(),
                                }
                            }
                            _ => todo!(),
                        }
                        //todo!()
                    }
                    _ => unreachable!(),
                }
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
                let Some(_ty) = local_ctx.get_base_type(&sig) else {
                    return Err(format!("Type not found {}:{}", file!(), line!()).into());
                };
                if let TypeSignature::Struct(Some(s)) = sig {
                    let member = (*member_access.member).clone();
                    match member.expr {
                        TypedExpressionData::Identifier { name } => {
                            let Some(member) = s.fields.get(&name) else {
                                return Err(format!("Member not found: {1} {0}", line!() , &name).into());
                            };

                            let Some(get_member) = self
                                .ctx
                                .ir_builder
                                .build_struct_gep(obj_access.into_pointer_value(), member.idx, &member.name)
                                .ok() else {
                                    return Err(format!("Member not found ({0})", line!() ).into());
                                };
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
        */
    }

    fn codegen_result_statement(
        &self,
        result_stmt: &'gen TypedResultStatement<'gen>,
        local_ctx: LocalCodegenContext<'gen>,
    ) -> Result<BasicValueEnum<'gen>, Box<dyn Error>> {
        debug!("generator::CodeGenerator::codegen_result_statement");
        let value = self.codegen_expression(&result_stmt.value, local_ctx.clone())?;
        let Some(value) = value else {
            return Err("Value is None".into());
        };
        Ok(value)
    }
}
