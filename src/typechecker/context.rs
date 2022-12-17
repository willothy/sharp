use std::{cell::RefCell, collections::HashMap, rc::Rc};

use linked_hash_map::LinkedHashMap;

use crate::ast;

use super::type_sig::{Name, PointerType, PrimitiveType, StructType, Type, TypeSignature};

pub type TypeId = usize;

#[derive(Debug, Clone)]
pub struct TypeCheckContext<'ctx> {
    pub names: HashMap<String, Name<'ctx>>,
    pub types: HashMap<String, TypeRef<'ctx>>,
    pub structs: HashMap<TypeId, StructType<'ctx>>,
    pub submodules: HashMap<String, TypeCheckContext<'ctx>>,
    pub primitives: HashMap<String, TypeRef<'ctx>>,
    pub last_struct_id: TypeId,
}

#[derive(Debug, Clone)]
pub struct LocalTypecheckContext<'ctx> {
    pub names: HashMap<String, Name<'ctx>>,
    pub return_type: Option<TypeRef<'ctx>>,
    pub result_type: Option<TypeRef<'ctx>>,
    pub in_loop: bool,
}

impl<'ctx> From<&TypeCheckContext<'ctx>> for LocalTypecheckContext<'ctx> {
    fn from(ctx: &TypeCheckContext<'ctx>) -> Self {
        Self {
            names: ctx.names.clone(),
            return_type: None,
            result_type: None,
            in_loop: false,
        }
    }
}

impl<'ctx> LocalTypecheckContext<'ctx> {
    pub fn new() -> Self {
        Self {
            names: HashMap::new(),
            return_type: None,
            result_type: None,
            in_loop: false,
        }
    }

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

impl<'ctx> TypeCheckContext<'ctx> {
    pub fn new() -> Self {
        let mut ctx = Self {
            names: HashMap::new(),
            types: HashMap::new(),
            structs: HashMap::new(),
            submodules: HashMap::new(),
            primitives: HashMap::new(),
            last_struct_id: 0,
        };
        ctx.load_primitives();
        ctx
    }

    fn load_primitives(&mut self) {
        // Add primitive types
        self.primitives.insert(
            "i8".into(),
            new_type(Type::new(TypeSignature::Primitive(PrimitiveType::I8))),
        );
        self.primitives.insert(
            "i16".into(),
            new_type(Type::new(TypeSignature::Primitive(PrimitiveType::I16))),
        );
        self.primitives.insert(
            "i32".into(),
            new_type(Type::new(TypeSignature::Primitive(PrimitiveType::I32))),
        );
        self.primitives.insert(
            "i64".into(),
            new_type(Type::new(TypeSignature::Primitive(PrimitiveType::I64))),
        );
        self.primitives.insert(
            "f32".into(),
            new_type(Type::new(TypeSignature::Primitive(PrimitiveType::F32))),
        );
        self.primitives.insert(
            "f64".into(),
            new_type(Type::new(TypeSignature::Primitive(PrimitiveType::F64))),
        );
        self.primitives.insert(
            "bool".into(),
            new_type(Type::new(TypeSignature::Primitive(PrimitiveType::Bool))),
        );
        self.primitives.insert(
            "char".into(),
            new_type(Type::new(TypeSignature::Primitive(PrimitiveType::Char))),
        );
        self.primitives.insert(
            "str".into(),
            new_type(Type::new(TypeSignature::Primitive(PrimitiveType::Str))),
        );
        self.primitives
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
            if let Some(t) = self.primitives.get(&base_name) {
                return Ok(t.clone());
            }
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

    pub fn add_type(&mut self, type_name: String, type_: TypeRef<'ctx>) -> Result<(), String> {
        if self.types.contains_key(&type_name) {
            return Err(format!(
                "Type {} already exists {}:{}",
                type_name,
                file!(),
                line!()
            ));
        }
        self.types.insert(type_name, type_);
        Ok(())
    }

    pub fn update_type(&mut self, type_name: String, type_: TypeRef<'ctx>) -> Result<(), String> {
        if !self.types.contains_key(&type_name) {
            return Err(format!(
                "Type {} does not exist {}:{}",
                type_name,
                file!(),
                line!()
            ));
        }
        self.types.insert(type_name, type_);
        Ok(())
    }

    pub fn types(&self) -> impl Iterator<Item = (&String, &TypeRef<'ctx>)> {
        self.types.iter()
    }

    pub(crate) fn get_next_struct_id(&mut self) -> usize {
        self.last_struct_id += 1;
        self.last_struct_id
    }
}

pub fn new_type(t: Type) -> TypeRef {
    Rc::from(RefCell::from(t))
}
