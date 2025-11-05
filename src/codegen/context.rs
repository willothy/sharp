// Author: Will Hopkins

use std::{collections::HashMap, rc::Rc};

use inkwell::{
    basic_block::BasicBlock,
    module::Linkage,
    types::{AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, PointerType, VoidType},
    values::{FunctionValue, PointerValue},
    AddressSpace,
};

use crate::{
    debug,
    typechecker::{
        context::{StructId, TypeSig},
        type_sig::{self, FunctionType, PrimitiveType, StructType, TypeSignature},
        TypeCheckerOutput,
    },
};

pub trait GetOrAddFunction<'a> {
    fn get_or_add_function(
        &self,
        name: &str,
        ty: inkwell::types::FunctionType<'a>,
    ) -> FunctionValue;
}

impl<'a> GetOrAddFunction<'a> for inkwell::module::Module<'a> {
    fn get_or_add_function(
        &self,
        name: &str,
        ty: inkwell::types::FunctionType<'a>,
    ) -> FunctionValue {
        if let Some(f) = self.get_function(name) {
            return f;
        }

        self.add_function(name, ty, Some(Linkage::External))
    }
}

pub struct CodegenContext<'ctx> {
    pub llvm_ctx: &'ctx inkwell::context::Context,
    pub llvm_module: inkwell::module::Module<'ctx>,
    pub ir_builder: inkwell::builder::Builder<'ctx>,
    pub tc_mod: TypeCheckerOutput<'ctx>,
}

#[derive(Debug, Clone)]
pub struct LocalCodegenContext<'ctx> {
    pub names: HashMap<String, CodegenName<'ctx>>,
    pub types: HashMap<TypeSignature<'ctx>, Rc<CodegenType<'ctx>>>,
    pub structs: HashMap<StructId, StructType<'ctx>>,
    pub return_type: Option<TypeSignature<'ctx>>,
    pub result_type: Option<TypeSignature<'ctx>>,
    pub current_fn: Option<FunctionValue<'ctx>>,
    pub loop_ctx: Option<LoopContext<'ctx>>,
    pub impl_ctx: Option<ImplCtx<'ctx>>,
    pub is_self_arg: bool,
}

#[derive(Debug, Clone)]
pub struct LoopContext<'ctx> {
    pub body_block: BasicBlock<'ctx>,
    pub exit_block: BasicBlock<'ctx>,
}

#[derive(Debug, Clone)]
pub struct ImplCtx<'ctx> {
    pub self_type: TypeSignature<'ctx>,
    pub self_name: String,
}

impl<'ctx> LocalCodegenContext<'ctx> {
    pub fn with_impl(
        &self,
        impl_ctx: Option<ImplCtx<'ctx>>,
        self_arg: bool,
    ) -> Result<Self, String> {
        let ctx = Self {
            impl_ctx,
            is_self_arg: self_arg,
            ..self.clone()
        };

        /* let Some(impl_ctx) = &ctx.impl_ctx else  {
            return Err("impl_ctx is None".into());
        };
        let TypeSignature::Struct(id) = impl_ctx.self_type else {
            panic!("impl_ctx.self_type is not a struct");
        };
        let Some(struct_ty) = ctx.structs.get(&id) else {
            panic!("impl_ctx.self_type is not a struct");
        };
        let struct_name = impl_ctx.self_name.clone();
        for (name, method) in &struct_ty.methods.clone() {
            let TypeSignature::Function(f) = method.fn_ty.sig() else {
                panic!("impl_ctx.self_type is not a struct");
            };

            ctx.add_name(CodegenName {
                name: full_name.clone(),
                ty: Rc::new(CodegenType::new(
                    full_name.clone(),
                    method.fn_ty.sig(),
                    Some(codegen_ctx.function_to_llvm_ty(&f, &self.structs).unwrap()),
                )),
                alloc: None,
                is_arg: false,
                arg_idx: None,
            });
        } */

        Ok(ctx)
    }

    pub fn self_arg(&self, has: bool) -> Self {
        Self {
            is_self_arg: has,
            ..self.clone()
        }
    }

    pub fn add_name(&mut self, name: String, reg: CodegenName<'ctx>) {
        self.names.insert(name, reg);
    }

    pub fn get_name(&self, name: &str) -> Option<&CodegenName<'ctx>> {
        self.names.get(name)
    }

    pub fn add_type(&mut self, t: CodegenType<'ctx>) {
        self.types.insert(t.ty.clone(), Rc::from(t));
    }

    pub fn get_base_type(&self, sig: &TypeSignature<'ctx>) -> Option<Rc<CodegenType<'ctx>>> {
        if let TypeSignature::Pointer(p) = sig {
            let Some(t) = self.get_base_type(&p.target) else {
                return None;
            };
            Some(t)
            /* let Ok(llvm_ty) = t.llvm_ty.basic() else {
                return None;
            };
            let llvm_ty = llvm_ty.ptr_type(AddressSpace::Generic);
            let llvm_ty = CodegenLLVMType::from(llvm_ty);
            Some(Rc::new(CodegenType {
                name: "pointer_ty".into(),
                ty: sig.clone(),
                llvm_ty,
            })) */
        } else {
            self.types.get(sig).cloned()
        }
    }

    pub fn get_ptr_depth(&self, sig: &TypeSignature<'ctx>) -> usize {
        if let TypeSignature::Pointer(p) = sig {
            1 + self.get_ptr_depth(&p.target)
        } else {
            0
        }
    }
}

impl<'ctx> CodegenContext<'ctx> {
    pub fn new(
        llvm_ctx: &'ctx inkwell::context::Context,
        module: inkwell::module::Module<'ctx>,
        builder: inkwell::builder::Builder<'ctx>,
        tc_mod: TypeCheckerOutput<'ctx>,
    ) -> Self {
        Self {
            llvm_ctx,
            llvm_module: module,
            ir_builder: builder,
            tc_mod,
        }
    }

    pub fn add_primitive_types(&self, local_ctx: &mut LocalCodegenContext<'ctx>) {
        use PrimitiveType::*;
        use TypeSignature::*;

        local_ctx.add_type(CodegenType::new(
            "i8".into(),
            Primitive(I8),
            Some(self.llvm_ctx.i8_type().as_basic_type_enum().into()),
        ));
        local_ctx.add_type(CodegenType::new(
            "i16".into(),
            Primitive(I16),
            Some(self.llvm_ctx.i16_type().as_basic_type_enum().into()),
        ));
        local_ctx.add_type(CodegenType::new(
            "i32".into(),
            Primitive(I32),
            Some(self.llvm_ctx.i32_type().as_basic_type_enum().into()),
        ));
        local_ctx.add_type(CodegenType::new(
            "i64".into(),
            Primitive(I64),
            Some(self.llvm_ctx.i64_type().as_basic_type_enum().into()),
        ));
        local_ctx.add_type(CodegenType::new(
            "f32".into(),
            Primitive(F32),
            Some(self.llvm_ctx.f32_type().as_basic_type_enum().into()),
        ));
        local_ctx.add_type(CodegenType::new(
            "f64".into(),
            Primitive(F64),
            Some(self.llvm_ctx.f64_type().as_basic_type_enum().into()),
        ));
        local_ctx.add_type(CodegenType::new(
            "bool".into(),
            Primitive(Bool),
            Some(self.llvm_ctx.bool_type().as_basic_type_enum().into()),
        ));
        local_ctx.add_type(CodegenType::new(
            "char".into(),
            Primitive(Char),
            Some(self.llvm_ctx.i8_type().as_basic_type_enum().into()),
        ));
        // void
        local_ctx.add_type(CodegenType::new(
            "void".into(),
            Void,
            Some(self.llvm_ctx.void_type().into()),
        ));
    }

    pub fn primitive_to_llvm_ty(
        &self,
        prim: &PrimitiveType,
    ) -> Result<BasicTypeEnum<'ctx>, String> {
        debug!(format!(
            "codegen::context::primitive_to_llvm_ty: {:?}",
            prim
        ));
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
        structs: &HashMap<StructId, StructType<'ctx>>,
    ) -> Result<CodegenLLVMType<'ctx>, String> {
        debug!("function_to_llvm_ty");
        let mut param_types: Vec<(u32, BasicMetadataTypeEnum)> = Vec::new();
        for param in f.params.values() {
            let param_type = self
                .to_llvm_ty(&param.ty.borrow_mut().sig.clone(), structs)?
                .basic()?
                .into();
            param_types.push((param.idx, param_type));
        }
        param_types.sort_by(|a, b| a.0.cmp(&b.0));
        let param_types: Vec<BasicMetadataTypeEnum> =
            param_types.drain(..).map(|(_, t)| t).collect();

        let return_type = if let Some(ret) = &f.return_type {
            let t = ret.borrow_mut().sig.clone();
            self.to_llvm_ty(&t, structs)?
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

    pub fn struct_to_llvm_ty(
        &self,
        s: &StructType<'ctx>,
        structs: &HashMap<StructId, StructType<'ctx>>,
    ) -> Result<BasicTypeEnum<'ctx>, String> {
        let opaque_struct_t = if let Some(t) = self.llvm_ctx.get_struct_type(s.name.as_str()) {
            debug!(format!("Struct {:?}: already defined", t.get_name()));
            return Ok(t.into());
        } else {
            debug!(format!("Struct {:?}: not defined", s.name));
            self.llvm_ctx.opaque_struct_type(s.name.as_str())
        };

        let mut field_types: Vec<(BasicTypeEnum<'ctx>, u32)> = Vec::new();
        for field in s.fields.values() {
            let field_type = match field.ty.sig() {
                TypeSignature::Primitive(p) => match p {
                    PrimitiveType::I8 => self.llvm_ctx.i8_type().into(),
                    PrimitiveType::I16 => self.llvm_ctx.i16_type().into(),
                    PrimitiveType::I32 => self.llvm_ctx.i32_type().into(),
                    PrimitiveType::I64 => self.llvm_ctx.i64_type().into(),
                    PrimitiveType::F32 => self.llvm_ctx.f32_type().into(),
                    PrimitiveType::F64 => self.llvm_ctx.f64_type().into(),
                    PrimitiveType::Bool => self.llvm_ctx.bool_type().into(),
                    PrimitiveType::Char => self.llvm_ctx.i8_type().into(),
                    PrimitiveType::Str => unimplemented!("str not implemented"),
                },
                TypeSignature::Struct(sid) => {
                    if sid == s.id {
                        return Err(format!(
                            "struct_to_llvm_ty: Struct cannot contain itself: {:?}",
                            s.id
                        ));
                    } else {
                        let Some(s) = structs.get(&sid) else {
                            return Err(format!("struct_to_llvm_ty: Struct not found: {:?}", sid))
                        };

                        self.struct_to_llvm_ty(s, structs)?
                    }
                }
                TypeSignature::Function(_) => todo!(),
                TypeSignature::Pointer(ptr) => {
                    let base = ptr.base_type();
                    let depth = ptr.depth();
                    let base_llvm;
                    if let TypeSignature::Struct(id) = base {
                        if id == s.id {
                            base_llvm = opaque_struct_t;
                        } else {
                            let Some(s) = structs.get(&id) else {
                                return Err(format!("struct_to_llvm_ty: Struct not found: {:?}", id))
                            };

                            if let Some(t) = self.llvm_ctx.get_struct_type(s.name.as_str()) {
                                base_llvm = t;
                            } else {
                                base_llvm = self.struct_to_llvm_ty(s, structs)?.into_struct_type();
                            }
                        }
                    } else {
                        base_llvm = self.to_llvm_ty(&base, structs)?.struct_type()?;
                    }
                    let mut full_type = base_llvm.as_basic_type_enum();
                    for _ in 0..depth {
                        full_type = full_type
                            .ptr_type(inkwell::AddressSpace::Generic)
                            .as_basic_type_enum();
                    }
                    full_type.into()
                    /* // TODO!!!
                    self.llvm_ctx
                        .i32_type()
                        .ptr_type(inkwell::AddressSpace::Generic)
                        .into() */
                }
                TypeSignature::Void => unreachable!("Cannot be void"),
            };
            // self.to_llvm_ty(&field.ty.sig(), structs)?.basic()?
            field_types.push((field_type, field.idx));
        }

        field_types.sort_by(|a, b| a.1.cmp(&b.1));
        let field_types = field_types.drain(..).map(|(t, _)| t).collect::<Vec<_>>();

        opaque_struct_t.set_body(field_types.as_slice(), false);

        Ok(opaque_struct_t.into())
        /* let Some(s) = structs.get(&s.id) else {
            return Err(format!("struct_to_llvm_ty: Struct not found: {:?}", s.id))
        }; */
    }

    pub fn to_llvm_ty(
        &self,
        ty: &TypeSignature<'ctx>,
        structs: &HashMap<StructId, StructType<'ctx>>,
    ) -> Result<CodegenLLVMType<'ctx>, String> {
        debug!(format!("codegen::context::to_llvm_ty: {:?}", ty));
        use TypeSignature::*;
        match ty {
            Primitive(primitive) => {
                let res = self.primitive_to_llvm_ty(&primitive)?;
                Ok(res.into())
            }
            Struct(id) => {
                let Some(struct_type) = structs.get(id) else {
                    return Err(format!("to_llvm_ty: Struct not found: {:?}", id))
                };

                let res = self.struct_to_llvm_ty(&struct_type, structs)?;
                Ok(res.into())
            }
            Function(fn_type) => Ok(self.function_to_llvm_ty(&fn_type, structs)?.into()),
            Void => Ok(self.llvm_ctx.void_type().into()),
            Pointer(ptr_type) => Ok(self.ptr_to_llvm_ty(&ptr_type, structs)?.into()),
        }
    }

    pub fn ptr_to_llvm_ty(
        &self,
        ptr_type: &type_sig::PointerType<'ctx>,
        structs: &HashMap<StructId, StructType<'ctx>>,
    ) -> Result<CodegenLLVMType<'ctx>, String> {
        debug!(format!("codegen::context::ptr_to_llvm_ty: {:?}", ptr_type));
        match ptr_type.target.as_ref() {
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
            TypeSignature::Struct(id) => {
                let Some(struct_ptr) = structs.get(id) else {
                    return Err(format!("ptr_to_llvm_ty: Struct not found: {:?}", id))
                };

                let struct_type = self.struct_to_llvm_ty(&struct_ptr, structs)?;
                Ok(struct_type.ptr_type(AddressSpace::Generic).into())
            }
            TypeSignature::Function(_) => {
                todo!()
            }
            TypeSignature::Pointer(ptr) => {
                let target_type = self.ptr_to_llvm_ty(&ptr, structs)?;

                Ok(target_type.basic()?.ptr_type(AddressSpace::Generic).into())
            }
            TypeSignature::Void => unreachable!(),
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
    pub llvm_ty: Option<CodegenLLVMType<'ctx>>,
}

impl<'ctx> CodegenType<'ctx> {
    pub fn new(
        name: String,
        ty: TypeSignature<'ctx>,
        llvm_ty: Option<CodegenLLVMType<'ctx>>,
    ) -> Self {
        Self { name, ty, llvm_ty }
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

    #[allow(unused)]
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
                other => Err(format!("Not a struct type: {:?}", other)),
            },
            _ => Err(format!(
                "Type is not basic: {:?} {}:{}",
                self,
                file!(),
                line!()
            )),
        }
    }

    #[allow(unused)]
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

    #[allow(unused)]
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

pub trait GetLLVMType<'ctx> {
    fn llvm_type(&self) -> Result<CodegenLLVMType<'ctx>, String>;
}

impl<'ctx> GetLLVMType<'ctx> for Rc<CodegenType<'ctx>> {
    fn llvm_type(&self) -> Result<CodegenLLVMType<'ctx>, String> {
        match &self.llvm_ty {
            Some(ty) => Ok(ty.clone()),
            None => Err(format!(
                "Type has no LLVM type: {:?} {}:{}",
                self,
                file!(),
                line!()
            )),
        }
    }
}
