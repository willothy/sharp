use std::{cell::RefCell, collections::HashMap, rc::Rc};

use serde::{Deserialize, Serialize};

use super::type_sig::{Name, PointerType, PrimitiveType, StructType, Type, TypeSignature};
use serde_with::serde_as;
pub type StructId = usize;

#[serde_as]
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ModuleTypeCheckCtx<'ctx> {
    #[serde_as(as = "Vec<(_, _)>")]
    pub functions: HashMap<String, Name<'ctx>>,
    #[serde_as(as = "Vec<(_, _)>")]
    pub types: HashMap<String, TypeRef<'ctx>>,
    #[serde_as(as = "Vec<(_, _)>")]
    pub struct_types: HashMap<StructId, StructType<'ctx>>,
}

#[derive(Debug, Clone)]
pub struct LocalTypecheckContext<'ctx> {
    pub names: HashMap<String, Name<'ctx>>,
    pub return_type: Option<TypeRef<'ctx>>,
    pub result_type: Option<TypeRef<'ctx>>,
    pub in_loop: bool,
    pub impl_ctx: Option<ImplContext<'ctx>>,
}

#[derive(Debug, Clone)]
pub struct ImplContext<'ctx> {
    pub struct_ty: TypeRef<'ctx>,
    //pub trait_ty: TypeRef<'ctx>,
}

impl<'ctx> From<&ModuleTypeCheckCtx<'ctx>> for LocalTypecheckContext<'ctx> {
    fn from(ctx: &ModuleTypeCheckCtx<'ctx>) -> Self {
        Self {
            names: ctx.functions.clone(),
            return_type: None,
            result_type: None,
            in_loop: false,
            impl_ctx: None,
        }
    }
}

impl<'ctx> LocalTypecheckContext<'ctx> {
    pub fn expect_result(&self, ty: Option<TypeRef<'ctx>>) -> Self {
        let mut new = self.clone();
        new.result_type = ty;
        new
    }

    pub fn enter_loop(&self) -> Self {
        let mut new = self.clone();
        new.in_loop = true;
        new
    }

    pub fn new() -> Self {
        Self {
            names: HashMap::new(),
            return_type: None,
            result_type: None,
            in_loop: false,
            impl_ctx: None,
        }
    }

    pub fn impl_method(struct_ty: TypeRef<'ctx>) -> Self {
        let mut new = Self::new();
        new.impl_ctx = Some(ImplContext { struct_ty });
        new
    }
}

pub type TypeRef<'type_ref> = Rc<RefCell<Type<'type_ref>>>;

pub trait TypeSig<'t> {
    fn sig(&self) -> TypeSignature<'t>;
}

impl<'type_reg> TypeSig<'type_reg> for TypeRef<'type_reg> {
    fn sig(&self) -> TypeSignature<'type_reg> {
        self.borrow().sig.clone()
    }
}

impl<'ctx> ModuleTypeCheckCtx<'ctx> {
    pub fn new() -> Self {
        let mut ctx = Self {
            functions: HashMap::new(),
            types: HashMap::new(),
            struct_types: HashMap::new(),
        };
        ctx.load_primitives();
        ctx
    }

    pub fn with_types(other: &Self) -> Self {
        let ctx = Self {
            functions: HashMap::new(),
            types: other.types.clone(),
            struct_types: other.struct_types.clone(),
        };
        ctx
    }

    fn load_primitives(&mut self) {
        // Add primitive types
        self.types.insert(
            "i8".into(),
            new_type(Type::new(TypeSignature::Primitive(PrimitiveType::I8))),
        );
        self.types.insert(
            "i16".into(),
            new_type(Type::new(TypeSignature::Primitive(PrimitiveType::I16))),
        );
        self.types.insert(
            "i32".into(),
            new_type(Type::new(TypeSignature::Primitive(PrimitiveType::I32))),
        );
        self.types.insert(
            "i64".into(),
            new_type(Type::new(TypeSignature::Primitive(PrimitiveType::I64))),
        );
        self.types.insert(
            "f32".into(),
            new_type(Type::new(TypeSignature::Primitive(PrimitiveType::F32))),
        );
        self.types.insert(
            "f64".into(),
            new_type(Type::new(TypeSignature::Primitive(PrimitiveType::F64))),
        );
        self.types.insert(
            "bool".into(),
            new_type(Type::new(TypeSignature::Primitive(PrimitiveType::Bool))),
        );
        self.types.insert(
            "char".into(),
            new_type(Type::new(TypeSignature::Primitive(PrimitiveType::Char))),
        );
        self.types.insert(
            "str".into(),
            new_type(Type::new(TypeSignature::Primitive(PrimitiveType::Str))),
        );
        self.types
            .insert("void".into(), new_type(Type::new(TypeSignature::Void)));
    }

    pub fn get_type(&self, type_name: String) -> Result<TypeRef<'ctx>, String> {
        let mut ptr_dim = 0;
        for c in type_name.chars() {
            if c == '*' {
                ptr_dim += 1;
            } else {
                break;
            }
        }

        let base_name = type_name[ptr_dim..].to_string();

        if ptr_dim == 0 {
            let Some(type_) = self.types.get(&base_name.clone()) else {
                return Err(format!(
                    "{} is not a valid type {}:{}",
                    type_name,
                    file!(),
                    line!()
                ))
            };

            Ok(type_.clone())
        } else {
            let base_t = self.get_type(base_name)?;
            let mut ptr_type = base_t.clone();
            for _ in 0..ptr_dim {
                ptr_type = new_type(Type::new(TypeSignature::Pointer(PointerType {
                    target: Box::from(ptr_type.sig()),
                })));
            }
            Ok(ptr_type)
        }
    }
}

pub fn new_type(t: Type) -> TypeRef {
    Rc::from(RefCell::from(t))
}
