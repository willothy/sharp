use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::ast;

use super::type_sig::{Name, PrimitiveType, Type, TypeSignature};

#[derive(Debug, Clone)]
pub struct TypeCheckContext<'ctx> {
    pub names: HashMap<String, Name<'ctx>>,
    pub types: HashMap<String, TypeRef<'ctx>>,
    pub submodules: HashMap<String, TypeCheckContext<'ctx>>,
    pub return_type: Option<TypeRef<'ctx>>,
    pub yield_type: Option<TypeRef<'ctx>>,
    pub primitives: HashMap<String, TypeRef<'ctx>>,
}

pub type TypeRef<'type_ref> = Rc<RefCell<Type<'type_ref>>>;

impl<'ctx> TypeCheckContext<'ctx> {
    pub fn new() -> Self {
        let mut ctx = Self {
            names: HashMap::new(),
            types: HashMap::new(),
            submodules: HashMap::new(),
            return_type: None,
            yield_type: None,
            primitives: HashMap::new(),
        };
        ctx
    }

    pub fn enter_fn(self, fn_type: TypeRef<'ctx>) -> Self {
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
                            var_type: param.param_type.clone(),
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
    }

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

pub fn load_module_ctx<'ctx>(module: &ast::Module) -> Result<TypeCheckContext<'ctx>, String> {
    let mut ctx = TypeCheckContext::new();

    // Add primitive types
    ctx.primitives.insert(
        "i32".into(),
        new_type(Type::new(TypeSignature::Primitive(PrimitiveType::I32))),
    );
    ctx.primitives.insert(
        "i64".into(),
        new_type(Type::new(TypeSignature::Primitive(PrimitiveType::I64))),
    );
    ctx.primitives.insert(
        "f32".into(),
        new_type(Type::new(TypeSignature::Primitive(PrimitiveType::F32))),
    );
    ctx.primitives.insert(
        "f64".into(),
        new_type(Type::new(TypeSignature::Primitive(PrimitiveType::F64))),
    );
    ctx.primitives.insert(
        "bool".into(),
        new_type(Type::new(TypeSignature::Primitive(PrimitiveType::Bool))),
    );
    ctx.primitives.insert(
        "char".into(),
        new_type(Type::new(TypeSignature::Primitive(PrimitiveType::Char))),
    );

    Ok(ctx)
}
