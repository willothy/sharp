// Author: Will Hopkins

use std::{collections::HashMap, rc::Rc};

use inkwell::{
    types::{AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, PointerType, VoidType},
    values::{BasicValueEnum, FunctionValue, PointerValue},
    AddressSpace,
};

use crate::typechecker::{
    self,
    type_sig::{self, FunctionType, PrimitiveType, StructType, TypeSignature},
};

pub struct CodegenContext<'ctx> {
    pub llvm_ctx: &'ctx inkwell::context::Context,
    pub llvm_module: inkwell::module::Module<'ctx>,
    pub ir_builder: inkwell::builder::Builder<'ctx>,
    pub tc_mod: &'ctx typechecker::TypeCheckModule<'ctx>,
}

#[derive(Debug, Clone)]
pub struct LocalCodegenContext<'ctx> {
    pub names: HashMap<String, CodegenName<'ctx>>,
    pub types: HashMap<TypeSignature<'ctx>, Rc<CodegenType<'ctx>>>,
    pub return_type: Option<TypeSignature<'ctx>>,
    pub yield_type: Option<TypeSignature<'ctx>>,
    pub current_fn: Option<FunctionValue<'ctx>>,
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

    pub fn get_base_type(&self, sig: &TypeSignature<'ctx>) -> Option<Rc<CodegenType<'ctx>>> {
        if let TypeSignature::Pointer(p) = sig {
            let Some(t) = self.get_base_type(&*p.target) else {
                return None;
            };
            let Ok(llvm_ty) = t.llvm_ty.basic() else {
                return None;
            };
            let llvm_ty = llvm_ty.ptr_type(AddressSpace::Generic);
            let llvm_ty = CodegenLLVMType::from(llvm_ty);
            Some(Rc::new(CodegenType {
                name: "pointer_ty".into(),
                ty: sig.clone(),
                llvm_ty,
            }))
        } else {
            self.types.get(sig).cloned()
        }
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
        tc_mod: &'ctx typechecker::TypeCheckModule<'ctx>,
    ) -> Self {
        let mut ctx = Self {
            llvm_ctx,
            llvm_module: module,
            ir_builder: builder,
            tc_mod,
        };
        ctx
    }

    pub fn add_primitive_types(&self, local_ctx: &mut LocalCodegenContext<'ctx>) {
        use PrimitiveType::*;
        use TypeSignature::*;

        local_ctx.add_type(CodegenType::new(
            "i8".into(),
            Primitive(I8),
            self.llvm_ctx.i8_type().as_basic_type_enum().into(),
        ));
        local_ctx.add_type(CodegenType::new(
            "i16".into(),
            Primitive(I16),
            self.llvm_ctx.i16_type().as_basic_type_enum().into(),
        ));
        local_ctx.add_type(CodegenType::new(
            "i32".into(),
            Primitive(I32),
            self.llvm_ctx.i32_type().as_basic_type_enum().into(),
        ));
        local_ctx.add_type(CodegenType::new(
            "i64".into(),
            Primitive(I64),
            self.llvm_ctx.i64_type().as_basic_type_enum().into(),
        ));
        local_ctx.add_type(CodegenType::new(
            "f32".into(),
            Primitive(F32),
            self.llvm_ctx.f32_type().as_basic_type_enum().into(),
        ));
        local_ctx.add_type(CodegenType::new(
            "f64".into(),
            Primitive(F64),
            self.llvm_ctx.f64_type().as_basic_type_enum().into(),
        ));
        local_ctx.add_type(CodegenType::new(
            "bool".into(),
            Primitive(Bool),
            self.llvm_ctx.bool_type().as_basic_type_enum().into(),
        ));
        local_ctx.add_type(CodegenType::new(
            "char".into(),
            Primitive(Char),
            self.llvm_ctx.i8_type().as_basic_type_enum().into(),
        ));
        // void
        local_ctx.add_type(CodegenType::new(
            "void".into(),
            Void,
            self.llvm_ctx.void_type().into(),
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
            PrimitiveType::Str => Ok(self
                .llvm_ctx
                .const_string("".as_bytes(), true)
                .get_type()
                .get_element_type()
                .ptr_type(inkwell::AddressSpace::Generic)
                .as_basic_type_enum()),
            PrimitiveType::I8 => Ok(self.llvm_ctx.i8_type().into()),
            PrimitiveType::I16 => Ok(self.llvm_ctx.i16_type().into()),
            //_ => Err(format!("Unsupported primitive type: {:?}", prim)),
        }
    }

    pub fn function_to_llvm_ty(
        &self,
        f: &FunctionType<'ctx>,
    ) -> Result<CodegenLLVMType<'ctx>, String> {
        let mut param_types: Vec<BasicMetadataTypeEnum> = Vec::new();
        for param in f.params.values() {
            let param_type = self
                .to_llvm_basic_ty(&param.ty.borrow_mut().sig.clone())?
                .into();
            param_types.push(param_type);
        }

        let return_type = if let Some(ret) = &f.return_type {
            let t = ret.borrow_mut().sig.clone();
            self.to_llvm_ty(&t)?
        } else {
            self.llvm_ctx.void_type().into()
        };

        let variadic = f.variadic;

        if let CodegenLLVMType::Basic(return_type) = return_type {
            match return_type {
                BasicTypeEnum::ArrayType(array_t) => Ok(CodegenLLVMType::Function(
                    array_t.fn_type(param_types.as_slice(), variadic),
                )),
                BasicTypeEnum::FloatType(float_t) => Ok(CodegenLLVMType::Function(
                    float_t.fn_type(param_types.as_slice(), variadic),
                )),
                BasicTypeEnum::IntType(int_t) => Ok(CodegenLLVMType::Function(
                    int_t.fn_type(param_types.as_slice(), variadic),
                )),
                BasicTypeEnum::PointerType(ptr_t) => Ok(CodegenLLVMType::Function(
                    ptr_t.fn_type(param_types.as_slice(), variadic),
                )),
                BasicTypeEnum::StructType(struct_t) => Ok(CodegenLLVMType::Function(
                    struct_t.fn_type(param_types.as_slice(), variadic),
                )),
                BasicTypeEnum::VectorType(vector_t) => Ok(CodegenLLVMType::Function(
                    vector_t.fn_type(param_types.as_slice(), variadic),
                )),
            }
        } else if let CodegenLLVMType::Void(void_return) = return_type {
            Ok(void_return.fn_type(param_types.as_slice(), variadic).into())
        } else {
            return Err("Function type cannot be returned".into());
        }
    }

    pub fn struct_to_llvm_ty(&self, s: &StructType<'ctx>) -> Result<BasicTypeEnum<'ctx>, String> {
        let mut field_types: Vec<BasicTypeEnum<'ctx>> = Vec::new();
        for field in s.fields.values() {
            let field_type = self.to_llvm_basic_ty(&field.ty.borrow_mut().sig.clone())?;
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

    pub fn to_llvm_ty(&self, ty: &TypeSignature<'ctx>) -> Result<CodegenLLVMType<'ctx>, String> {
        use TypeSignature::*;
        match ty {
            Primitive(primitive) => {
                let res = self.primitive_to_llvm_ty(&primitive)?;
                Ok(res.into())
            }
            Struct(struct_type) => {
                let Some(struct_type) = struct_type else {
                    return Err("Struct type not found".into());
                };
                let res = self.struct_to_llvm_ty(&struct_type)?;
                Ok(res.into())
            }
            Function(fn_type) => Ok(self.function_to_llvm_ty(&fn_type)?.into()),
            Void => Ok(self.llvm_ctx.void_type().into()),
            Pointer(ptr_type) => Ok(self.ptr_to_llvm_ty(&ptr_type)?.into()),
        }
    }

    pub fn ptr_to_llvm_ty(
        &self,
        ptr_type: &type_sig::PointerType<'ctx>,
    ) -> Result<CodegenLLVMType<'ctx>, String> {
        match &*ptr_type.target {
            TypeSignature::Primitive(prim) => match prim {
                PrimitiveType::I8 => Ok(self
                    .llvm_ctx
                    .i8_type()
                    .ptr_type(AddressSpace::Generic)
                    .into()),
                PrimitiveType::I16 => Ok(self
                    .llvm_ctx
                    .i16_type()
                    .ptr_type(AddressSpace::Generic)
                    .into()),
                PrimitiveType::I32 => Ok(self
                    .llvm_ctx
                    .i32_type()
                    .ptr_type(AddressSpace::Generic)
                    .into()),
                PrimitiveType::I64 => Ok(self
                    .llvm_ctx
                    .i64_type()
                    .ptr_type(AddressSpace::Generic)
                    .into()),
                PrimitiveType::F32 => Ok(self
                    .llvm_ctx
                    .f32_type()
                    .ptr_type(AddressSpace::Generic)
                    .into()),
                PrimitiveType::F64 => Ok(self
                    .llvm_ctx
                    .f64_type()
                    .ptr_type(AddressSpace::Generic)
                    .into()),
                PrimitiveType::Bool => Ok(self
                    .llvm_ctx
                    .bool_type()
                    .ptr_type(AddressSpace::Generic)
                    .into()),
                PrimitiveType::Char => todo!(),
                _ => Err("Unsupported primitive type".into()),
            },
            TypeSignature::Struct(Some(struct_ptr)) => {
                let struct_type = self.struct_to_llvm_ty(&struct_ptr)?;
                Ok(struct_type.ptr_type(AddressSpace::Generic).into())
            }
            TypeSignature::Function(fn_type) => {
                todo!()
            }
            TypeSignature::Pointer(ptr) => {
                let target_type = self.ptr_to_llvm_ty(&ptr)?;

                Ok(target_type.basic()?.ptr_type(AddressSpace::Generic).into())
            }
            TypeSignature::Void => unreachable!(),
            _ => Err("Unsupported codegen type".into()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CodegenName<'ctx> {
    pub name: String,
    pub ty: Rc<CodegenType<'ctx>>,
    pub alloc: Option<PointerValue<'ctx>>,
    pub is_arg: bool,
    pub arg_idx: Option<u32>,
}

impl<'ctx> CodegenName<'ctx> {
    pub fn new(name: String, ty: Rc<CodegenType<'ctx>>) -> Self {
        Self {
            name,
            ty,
            alloc: None,
            is_arg: false,
            arg_idx: None,
        }
    }

    pub fn new_arg(name: String, ty: Rc<CodegenType<'ctx>>, idx: u32) -> Self {
        Self {
            name,
            ty,
            alloc: None,
            is_arg: true,
            arg_idx: Some(idx),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CodegenType<'ctx> {
    pub name: String,
    pub ty: TypeSignature<'ctx>,
    pub llvm_ty: CodegenLLVMType<'ctx>,
}

impl<'ctx> CodegenType<'ctx> {
    pub fn new(name: String, ty: TypeSignature<'ctx>, llvm_ty: CodegenLLVMType<'ctx>) -> Self {
        Self { name, ty, llvm_ty }
    }

    pub fn to_struct_type(&self) -> Result<type_sig::StructType<'ctx>, String> {
        if let TypeSignature::Struct(Some(s)) = &self.ty {
            Ok(s.clone())
        } else {
            Err("Type is not a struct".into())
        }
    }
}

#[derive(Debug, Clone)]
pub enum CodegenLLVMType<'ctx> {
    Basic(BasicTypeEnum<'ctx>),
    Void(VoidType<'ctx>),
    Function(inkwell::types::FunctionType<'ctx>),
}

impl<'ctx> CodegenLLVMType<'ctx> {
    pub fn basic(&self) -> Result<BasicTypeEnum<'ctx>, String> {
        match self {
            CodegenLLVMType::Basic(basic) => Ok(*basic),
            _ => Err(format!(
                "Type is not basic: {:?} {}:{}",
                self,
                file!(),
                line!()
            )),
        }
    }

    pub fn void(&self) -> Result<VoidType<'ctx>, String> {
        match self {
            CodegenLLVMType::Void(void) => Ok(*void),
            _ => Err("Not a void type".into()),
        }
    }

    pub fn function(&self) -> Result<inkwell::types::FunctionType<'ctx>, String> {
        match self {
            CodegenLLVMType::Function(function) => Ok(*function),
            _ => Err("Not a function type".into()),
        }
    }

    pub fn int_type(&self) -> Result<inkwell::types::IntType<'ctx>, String> {
        match self {
            CodegenLLVMType::Basic(basic) => match basic {
                BasicTypeEnum::IntType(int) => Ok(*int),
                _ => Err("Not an int type".into()),
            },
            _ => Err(format!(
                "Type is not basic: {:?} {}:{}",
                self,
                file!(),
                line!()
            )),
        }
    }

    pub fn float_type(&self) -> Result<inkwell::types::FloatType<'ctx>, String> {
        match self {
            CodegenLLVMType::Basic(basic) => match basic {
                BasicTypeEnum::FloatType(float) => Ok(*float),
                _ => Err("Not a float type".into()),
            },
            _ => Err(format!(
                "Type is not basic: {:?} {}:{}",
                self,
                file!(),
                line!()
            )),
        }
    }

    pub fn pointer_type(&self) -> Result<inkwell::types::PointerType<'ctx>, String> {
        match self {
            CodegenLLVMType::Basic(basic) => match basic {
                BasicTypeEnum::PointerType(ptr) => Ok(*ptr),
                _ => Err("Not a pointer type".into()),
            },
            _ => Err(format!(
                "Type is not basic: {:?} {}:{}",
                self,
                file!(),
                line!()
            )),
        }
    }

    pub fn struct_type(&self) -> Result<inkwell::types::StructType<'ctx>, String> {
        match self {
            CodegenLLVMType::Basic(basic) => match basic {
                BasicTypeEnum::StructType(struct_t) => Ok(*struct_t),
                _ => Err("Not a struct type".into()),
            },
            _ => Err(format!(
                "Type is not basic: {:?} {}:{}",
                self,
                file!(),
                line!()
            )),
        }
    }

    pub fn array_type(&self) -> Result<inkwell::types::ArrayType<'ctx>, String> {
        match self {
            CodegenLLVMType::Basic(basic) => match basic {
                BasicTypeEnum::ArrayType(array_t) => Ok(*array_t),
                _ => Err("Not an array type".into()),
            },
            _ => Err(format!(
                "Type is not basic: {:?} {}:{}",
                self,
                file!(),
                line!()
            )),
        }
    }

    pub fn vector_type(&self) -> Result<inkwell::types::VectorType<'ctx>, String> {
        match self {
            CodegenLLVMType::Basic(basic) => match basic {
                BasicTypeEnum::VectorType(vector_t) => Ok(*vector_t),
                _ => Err("Not a vector type".into()),
            },
            _ => Err(format!(
                "Type is not basic: {:?} {}:{}",
                self,
                file!(),
                line!()
            )),
        }
    }
}

impl<'ctx> From<AnyTypeEnum<'ctx>> for CodegenLLVMType<'ctx> {
    fn from(t: AnyTypeEnum<'ctx>) -> Self {
        match t {
            AnyTypeEnum::ArrayType(v) => Self::Basic(v.as_basic_type_enum()),
            AnyTypeEnum::FloatType(v) => Self::Basic(v.as_basic_type_enum()),
            AnyTypeEnum::FunctionType(v) => Self::Function(v),
            AnyTypeEnum::IntType(v) => Self::Basic(v.as_basic_type_enum()),
            AnyTypeEnum::PointerType(v) => Self::Basic(v.as_basic_type_enum()),
            AnyTypeEnum::StructType(v) => Self::Basic(v.as_basic_type_enum()),
            AnyTypeEnum::VectorType(v) => Self::Basic(v.as_basic_type_enum()),
            AnyTypeEnum::VoidType(v) => Self::Void(v),
        }
    }
}

impl<'ctx> From<BasicTypeEnum<'ctx>> for CodegenLLVMType<'ctx> {
    fn from(t: BasicTypeEnum<'ctx>) -> Self {
        Self::Basic(t)
    }
}

impl<'ctx> From<inkwell::types::FunctionType<'ctx>> for CodegenLLVMType<'ctx> {
    fn from(t: inkwell::types::FunctionType<'ctx>) -> Self {
        Self::Function(t)
    }
}

impl<'ctx> From<PointerType<'ctx>> for CodegenLLVMType<'ctx> {
    fn from(t: PointerType<'ctx>) -> Self {
        Self::Basic(t.as_basic_type_enum())
    }
}

impl<'ctx> From<VoidType<'ctx>> for CodegenLLVMType<'ctx> {
    fn from(t: VoidType<'ctx>) -> Self {
        Self::Void(t)
    }
}

pub trait GetLLVMType {
    fn llvm_type(&self) -> CodegenLLVMType;
}

impl<'ctx> GetLLVMType for Rc<CodegenType<'ctx>> {
    fn llvm_type(&self) -> CodegenLLVMType<'ctx> {
        self.llvm_ty.clone()
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
