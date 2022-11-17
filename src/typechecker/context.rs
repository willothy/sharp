use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::ast;

use super::type_sig::{Name, PrimitiveType, Type, TypeSignature};

#[derive(Debug, Clone)]
pub struct TypeCheckContext<'ctx> {
    pub names: HashMap<String, Name<'ctx>>,
    pub types: HashMap<String, TypeRef<'ctx>>,
    pub submodules: HashMap<String, TypeCheckContext<'ctx>>,
    pub primitives: HashMap<String, TypeRef<'ctx>>,
}

#[derive(Debug, Clone)]
pub struct LocalTypecheckContext<'ctx> {
    pub names: HashMap<String, Name<'ctx>>,
    pub return_type: Option<TypeRef<'ctx>>,
    pub yield_type: Option<TypeRef<'ctx>>,
    pub in_loop: bool,
}

impl<'ctx> From<&TypeCheckContext<'ctx>> for LocalTypecheckContext<'ctx> {
    fn from(ctx: &TypeCheckContext<'ctx>) -> Self {
        Self {
            names: ctx.names.clone(),
            return_type: None,
            yield_type: None,
            in_loop: false,
        }
    }
}

impl<'ctx> LocalTypecheckContext<'ctx> {
    pub fn new() -> Self {
        Self {
            names: HashMap::new(),
            return_type: None,
            yield_type: None,
            in_loop: false,
        }
    }

    pub fn with_yield(&self, ty: TypeRef<'ctx>) -> Self {
        let mut new = self.clone();
        new.yield_type = Some(ty);
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
            submodules: HashMap::new(),
            primitives: HashMap::new(),
        };
        ctx.load_primitives();
        ctx
    }

    fn load_primitives(&mut self) {
        // Add primitive types
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
    }

    /* pub fn enter_fn(self, fn_type: TypeRef<'ctx>) -> Self {
        let mut ctx = self.clone();
        let fn_type = &*fn_type.borrow_mut();
        match &fn_type.sig {
            TypeSignature::Function(func) => {
                ctx.return_type = match &func.return_type {
                    Some(return_type) => Some(return_type.clone()),
                    None => None,
                };
                for param in func.params.values() {
                    ctx.names.insert(
                        param.name.clone(),
                        Name {
                            ty: param.ty.clone(),
                        },
                    );
                }
            }
            _ => panic!("Expected function type"),
        };

        ctx
    }

    pub fn exit_fn(self) -> Self {
        let mut ctx = self;
        ctx.return_type = None;
        ctx
    }

    pub fn allow_yield(&mut self, yield_type: TypeRef<'ctx>) -> &mut Self {
        self.yield_type = Some(yield_type);
        self
    }

    pub fn disallow_yield(self) -> Self {
        let mut ctx = self;
        ctx.yield_type = None;
        ctx
    } */

    pub fn get_type(&self, type_name: String) -> Result<TypeRef<'ctx>, String> {
        if let Some(t) = self.primitives.get(&type_name) {
            return Ok(t.clone());
        }
        match self.types.get(&type_name) {
            Some(type_) => Ok(type_.clone()),
            None => Err(format!("{} is not a valid type", type_name)),
        }
    }
}

pub fn box_some<T>(t: T) -> Box<Option<T>> {
    Box::from(Some(t))
}

pub fn box_none<T>() -> Box<Option<T>> {
    Box::new(None)
}

pub fn new_type(t: Type) -> TypeRef {
    Rc::from(RefCell::from(t))
}
