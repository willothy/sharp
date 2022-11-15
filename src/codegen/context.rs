// Author: Will Hopkins

use std::{collections::HashMap, rc::Rc};

use inkwell::{
    types::{AnyTypeEnum, BasicMetadataTypeEnum, BasicTypeEnum},
    values::PointerValue,
};

use crate::typechecker::{self, FunctionType, PrimitiveType, StructType, TypeSignature};

pub struct CodegenContext<'ctx> {
    pub llvm_ctx: &'ctx inkwell::context::Context,
    pub llvm_module: inkwell::module::Module<'ctx>,
    pub ir_builder: inkwell::builder::Builder<'ctx>,
    pub tc_ctx: &'ctx typechecker::TypeCheckContext<'ctx>,
}

#[derive(Debug, Clone)]
pub struct LocalCodegenContext<'ctx> {
    pub names: HashMap<String, CodegenName<'ctx>>,
    pub types: HashMap<TypeSignature<'ctx>, Rc<CodegenType<'ctx>>>,
}

impl<'ctx> LocalCodegenContext<'ctx> {
    pub fn add_name(&mut self, name: CodegenName<'ctx>) {
        self.names.insert(name.name.clone(), name);
    }

    pub fn get_name(&self, name: &str) -> Option<&CodegenName<'ctx>> {
        self.names.get(name)
    }

    pub fn add_type(&mut self, t: CodegenType<'ctx>) {
        self.types.insert(t.ty.clone(), Rc::from(t));
    }

    pub fn get_type(&self, sig: &TypeSignature<'ctx>) -> Option<&Rc<CodegenType<'ctx>>> {
        self.types.get(sig)
    }

    pub fn get_type_by_name(&self, name: &str) -> Option<&Rc<CodegenType<'ctx>>> {
        self.types
            .values()
            .find(|t| t.name.as_str() == name)
            .map(|t| t)
    }
}

impl<'ctx> CodegenContext<'ctx> {
    pub fn new(
        llvm_ctx: &'ctx inkwell::context::Context,
        module: inkwell::module::Module<'ctx>,
        builder: inkwell::builder::Builder<'ctx>,
        tc_context: &'ctx typechecker::TypeCheckContext<'ctx>,
    ) -> Self {
        let mut ctx = Self {
            llvm_ctx,
            llvm_module: module,
            ir_builder: builder,
            tc_ctx: tc_context,
        };
        ctx
    }

    pub fn add_primitive_types(&self, local_ctx: &mut LocalCodegenContext<'ctx>) {
        use PrimitiveType::*;
        use TypeSignature::*;

        local_ctx.add_type(CodegenType::new(
            "i32".into(),
            Primitive(I32),
            self.llvm_ctx.i32_type().into(),
        ));
        local_ctx.add_type(CodegenType::new(
            "i64".into(),
            Primitive(I64),
            self.llvm_ctx.i64_type().into(),
        ));
        local_ctx.add_type(CodegenType::new(
            "f32".into(),
            Primitive(F32),
            self.llvm_ctx.f32_type().into(),
        ));
        local_ctx.add_type(CodegenType::new(
            "f64".into(),
            Primitive(F64),
            self.llvm_ctx.f64_type().into(),
        ));
        local_ctx.add_type(CodegenType::new(
            "bool".into(),
            Primitive(Bool),
            self.llvm_ctx.bool_type().into(),
        ));
        local_ctx.add_type(CodegenType::new(
            "char".into(),
            Primitive(Char),
            self.llvm_ctx.i8_type().into(),
        ));
    }

    pub fn primitive_to_llvm_ty(
        &self,
        prim: &PrimitiveType,
    ) -> Result<BasicTypeEnum<'ctx>, String> {
        match prim {
            PrimitiveType::I32 => Ok(self.llvm_ctx.i32_type().into()),
            PrimitiveType::I64 => Ok(self.llvm_ctx.i64_type().into()),
            PrimitiveType::F32 => Ok(self.llvm_ctx.f32_type().into()),
            PrimitiveType::F64 => Ok(self.llvm_ctx.f64_type().into()),
            PrimitiveType::Bool => Ok(self.llvm_ctx.bool_type().into()),
            PrimitiveType::Char => Ok(self.llvm_ctx.i8_type().into()),
            _ => Err(format!("Unsupported primitive type: {:?}", prim)),
        }
    }

    pub fn function_to_llvm_ty(&self, f: &FunctionType<'ctx>) -> Result<AnyTypeEnum<'ctx>, String> {
        let mut param_types: Vec<BasicMetadataTypeEnum> = Vec::new();
        for param in f.params.values() {
            let param_type = self
                .to_llvm_basic_ty(&param.param_type.borrow_mut().type_.clone())?
                .into();
            param_types.push(param_type);
        }

        let return_type = if let Some(ret) = &f.return_type {
            let t = ret.borrow_mut().type_.clone();
            self.to_llvm_ty(&t)?
        } else {
            self.llvm_ctx.void_type().into()
        };

        match return_type {
            AnyTypeEnum::ArrayType(array_t) => Ok(AnyTypeEnum::FunctionType(
                array_t.fn_type(param_types.as_slice(), false),
            )),
            AnyTypeEnum::FloatType(float_t) => Ok(AnyTypeEnum::FunctionType(
                float_t.fn_type(param_types.as_slice(), false),
            )),
            AnyTypeEnum::FunctionType(fn_t) => Err("Function type cannot be returned".into()),
            AnyTypeEnum::IntType(int_t) => Ok(AnyTypeEnum::FunctionType(
                int_t.fn_type(param_types.as_slice(), false),
            )),
            AnyTypeEnum::PointerType(ptr_t) => Ok(AnyTypeEnum::FunctionType(
                ptr_t.fn_type(param_types.as_slice(), false),
            )),
            AnyTypeEnum::StructType(struct_t) => Ok(AnyTypeEnum::FunctionType(
                struct_t.fn_type(param_types.as_slice(), false),
            )),
            AnyTypeEnum::VectorType(vector_t) => Ok(AnyTypeEnum::FunctionType(
                vector_t.fn_type(param_types.as_slice(), false),
            )),
            AnyTypeEnum::VoidType(void_t) => Ok(AnyTypeEnum::FunctionType(
                void_t.fn_type(param_types.as_slice(), false),
            )),
        }
    }

    pub fn struct_to_llvm_ty(&self, s: &StructType<'ctx>) -> Result<BasicTypeEnum<'ctx>, String> {
        let mut field_types: Vec<BasicTypeEnum<'ctx>> = Vec::new();
        for field in s.fields.values() {
            let field_type = self.to_llvm_basic_ty(&field.field_type.borrow_mut().type_.clone())?;
            field_types.push(field_type);
        }

        Ok(self
            .llvm_ctx
            .struct_type(field_types.as_slice(), false)
            .into())
    }

    pub fn to_llvm_basic_ty(
        &self,
        ty: &TypeSignature<'ctx>,
    ) -> Result<BasicTypeEnum<'ctx>, String> {
        match ty {
            TypeSignature::Primitive(prim) => self.primitive_to_llvm_ty(&prim),
            TypeSignature::Struct(s) => {
                let Some(s) = s else {
                    return Err("Struct type not found".into());
                };
                self.struct_to_llvm_ty(s)
            }
            _ => Err(format!("Unsupported type: {:?}", ty)),
        }
    }

    pub fn basic_to_any(&self, ty: BasicTypeEnum<'ctx>) -> AnyTypeEnum<'ctx> {
        match ty {
            BasicTypeEnum::ArrayType(array_t) => array_t.into(),
            BasicTypeEnum::FloatType(float_t) => float_t.into(),
            BasicTypeEnum::IntType(int_t) => int_t.into(),
            BasicTypeEnum::PointerType(ptr_t) => ptr_t.into(),
            BasicTypeEnum::StructType(struct_t) => struct_t.into(),
            BasicTypeEnum::VectorType(vector_t) => vector_t.into(),
        }
    }

    pub fn to_llvm_ty(&self, ty: &TypeSignature<'ctx>) -> Result<AnyTypeEnum<'ctx>, String> {
        use TypeSignature::*;
        match ty {
            Primitive(primitive) => {
                let res = self.primitive_to_llvm_ty(&primitive)?;
                Ok(self.basic_to_any(res))
            }
            Struct(struct_type) => {
                let Some(struct_type) = struct_type else {
                    return Err("Struct type not found".into());
                };
                let res = self.struct_to_llvm_ty(&struct_type)?;
                Ok(self.basic_to_any(res))
            }
            Function(fn_type) => self.function_to_llvm_ty(&fn_type),
            Empty => Err("Empty type".into()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CodegenName<'ctx> {
    pub name: String,
    pub ty: Rc<CodegenType<'ctx>>,
    pub alloc: Option<PointerValue<'ctx>>,
}

impl<'ctx> CodegenName<'ctx> {
    pub fn new(name: String, ty: Rc<CodegenType<'ctx>>) -> Self {
        Self {
            name,
            ty,
            alloc: None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct CodegenType<'ctx> {
    pub name: String,
    pub ty: TypeSignature<'ctx>,
    pub llvm_ty: AnyTypeEnum<'ctx>,
}

impl<'ctx> CodegenType<'ctx> {
    pub fn new(name: String, ty: TypeSignature<'ctx>, llvm_ty: AnyTypeEnum<'ctx>) -> Self {
        Self { name, ty, llvm_ty }
    }
}

pub fn to_basic(ty: AnyTypeEnum) -> Result<BasicTypeEnum, String> {
    let t = match ty {
        AnyTypeEnum::ArrayType(array_t) => array_t.into(),
        AnyTypeEnum::FloatType(float_t) => float_t.into(),
        AnyTypeEnum::FunctionType(fn_t) => {
            return Err("Function type cannot be converted to basic type".into())
        }
        AnyTypeEnum::IntType(int_t) => int_t.into(),
        AnyTypeEnum::PointerType(ptr_t) => ptr_t.into(),
        AnyTypeEnum::StructType(struct_t) => struct_t.into(),
        AnyTypeEnum::VectorType(vector_t) => vector_t.into(),
        AnyTypeEnum::VoidType(void_t) => {
            return Err("Void type cannot be converted to basic type".into())
        }
    };
    Ok(t)
}
