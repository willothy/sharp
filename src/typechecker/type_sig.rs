use std::{cell::RefCell, collections::HashMap, hash::Hash, rc::Rc};

use crate::ast::FunctionDefinition;

use super::{
    context::{TypeId, TypeRef},
    typed_ast::TypedFunctionDefinition,
};

#[derive(Debug, Clone)]
pub struct Type<'t> {
    pub sig: TypeSignature<'t>,
}

impl<'t> PartialEq for Type<'t> {
    fn eq(&self, other: &Self) -> bool {
        self.sig == other.sig
    }
}

impl<'t> Eq for Type<'t> {}

impl<'t> Type<'t> {
    pub fn new(sig: TypeSignature<'t>) -> Self {
        Self { sig }
    }

    pub fn empty() -> Self {
        Self {
            sig: TypeSignature::Void,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PointerType<'t> {
    pub target: Box<TypeSignature<'t>>,
}

impl<'t> PointerType<'t> {
    pub fn base_type(&self) -> TypeSignature<'t> {
        self.target.get_base_type()
    }

    pub fn depth(&self) -> usize {
        let mut depth = 1;
        let mut current = self.target.as_ref();
        while let TypeSignature::Pointer(p) = current {
            depth += 1;
            current = p.target.as_ref();
        }
        depth
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeSignature<'t> {
    Primitive(PrimitiveType),
    Struct(TypeId),
    Function(FunctionType<'t>),
    Pointer(PointerType<'t>),
    Void,
}

impl<'t> TypeSignature<'t> {
    pub fn is_void(&self) -> bool {
        matches!(self, TypeSignature::Void)
    }

    pub fn is_primitive(&self) -> bool {
        matches!(self, TypeSignature::Primitive(_))
    }

    pub fn is_struct(&self) -> bool {
        matches!(self, TypeSignature::Struct(_))
    }

    pub fn is_function(&self) -> bool {
        matches!(self, TypeSignature::Function(_))
    }

    pub fn struct_id(&self) -> Result<TypeId, String> {
        if let TypeSignature::Struct(id) = self {
            Ok(*id)
        } else {
            Err(format!("Type {:?} is not a struct", self))
        }
    }

    pub fn get_return_type(&'t self) -> Result<Option<TypeRef>, String> {
        match self {
            TypeSignature::Function(f) => Ok(f.return_type.clone()),
            _ => Err(format!("Type {:?} is not a function", self)),
        }
    }

    pub fn get_ptr_inner_ty(&self) -> TypeSignature<'t> {
        if let TypeSignature::Pointer(p) = self {
            p.target.as_ref().clone()
        } else {
            self.clone()
        }
    }

    pub fn wrap_in_ptr(&self) -> TypeSignature<'t> {
        TypeSignature::Pointer(PointerType {
            target: Box::new(self.clone()),
        })
    }

    pub fn get_base_type(&self) -> TypeSignature<'t> {
        match self {
            TypeSignature::Pointer(p) => p.target.get_base_type(),
            _ => self.clone(),
        }
    }

    pub fn string_repr(&self) -> String {
        let mut repr = String::new();
        let mut current = self;
        while let TypeSignature::Pointer(p) = current {
            repr += format!("*{}", p.target.string_repr()).as_str();
            current = &p.target;
        }
        match current {
            TypeSignature::Primitive(p) => {
                repr += match p {
                    PrimitiveType::I8 => "i8",
                    PrimitiveType::I16 => "i16",
                    PrimitiveType::I32 => "i32",
                    PrimitiveType::I64 => "i64",
                    PrimitiveType::F32 => "f32",
                    PrimitiveType::F64 => "f64",
                    PrimitiveType::Bool => "bool",
                    PrimitiveType::Char => "char",
                    PrimitiveType::Str => "str",
                }
            }
            TypeSignature::Struct(id) => repr += format!("struct {}", id).as_str(),
            TypeSignature::Function(f) => repr += "fn",
            TypeSignature::Void => repr += "void",
            _ => panic!(),
        }
        repr
    }
}

impl<'t> Hash for Type<'t> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.sig.hash(state);
    }
}

impl<'t> Hash for TypeSignature<'t> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            TypeSignature::Primitive(p) => p.hash(state),
            TypeSignature::Struct(s) => s.hash(state),
            TypeSignature::Function(f) => f.hash(state),
            TypeSignature::Void => 0.hash(state),
            TypeSignature::Pointer(p) => p.hash(state),
        }
    }
}

impl<'t> Hash for PointerType<'t> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.target.hash(state);
    }
}

impl Hash for PrimitiveType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
    }
}

impl<'t> Hash for StructType<'t> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

impl<'t> Hash for FunctionType<'t> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        if let Some(ret_type) = &self.return_type {
            let return_type = ret_type.borrow_mut().clone();
            return_type.hash(state);
        }
        self.params.iter().for_each(|(_, v)| v.hash(state));
    }
}

impl<'fn_param> Hash for TypedFunctionParameter<'fn_param> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.ty.borrow_mut().hash(state);
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PrimitiveType {
    I8,
    I16,
    I32,
    I64,
    F32,
    F64,
    Bool,
    Char,
    Str,
}

impl TryFrom<String> for PrimitiveType {
    type Error = String;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        match value.as_str() {
            "i32" => Ok(PrimitiveType::I32),
            "i64" => Ok(PrimitiveType::I64),
            "f32" => Ok(PrimitiveType::F32),
            "f64" => Ok(PrimitiveType::F64),
            "bool" => Ok(PrimitiveType::Bool),
            "char" => Ok(PrimitiveType::Char),
            "string" => Ok(PrimitiveType::Str),
            _ => Err(format!("{} is not a valid primitive type", value)),
        }
    }
}

#[derive(Debug, Clone)]
pub struct StructType<'struct_type> {
    pub name: String,
    pub fields: HashMap<String, TypedStructField<'struct_type>>,
    pub methods: HashMap<String, TypedFunctionDefinition<'struct_type>>,
    pub id: usize,
}
impl<'struct_type> StructType<'struct_type> {
    pub(crate) fn get_member_idx(&self, name: &String) -> Option<u32> {
        self.fields.get(name).map(|f| f.idx)
    }
}

impl<'struct_type> PartialEq for StructType<'struct_type> {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl<'struct_type> Eq for StructType<'struct_type> {}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedStructField<'field> {
    pub name: String,
    pub ty: TypeRef<'field>,
    pub idx: u32,
}

#[derive(Debug, Clone)]
pub struct FunctionType<'fn_type> {
    pub return_type: Option<TypeRef<'fn_type>>,
    pub params: HashMap<String, TypedFunctionParameter<'fn_type>>,
    pub variadic: bool,
}

impl<'fn_type> PartialEq for FunctionType<'fn_type> {
    fn eq(&self, other: &Self) -> bool {
        if let Some(ret_type) = &self.return_type {
            let return_type = ret_type.borrow_mut().clone();
            if let Some(other_ret_type) = &other.return_type {
                let other_return_type = other_ret_type.borrow_mut().clone();
                if return_type != other_return_type {
                    return false;
                }
            } else {
                return false;
            }
        } else if other.return_type.is_some() {
            return false;
        }

        self.params.iter().all(|(k, v)| {
            if let Some(other_v) = other.params.get(k) {
                v == other_v
            } else {
                false
            }
        })
    }
}

impl<'fn_type> Eq for FunctionType<'fn_type> {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedFunctionParameter<'fn_param> {
    pub name: String,
    pub ty: TypeRef<'fn_param>,
    pub idx: u32,
}

#[derive(Debug, Clone)]
pub struct Name<'name> {
    pub ty: TypeRef<'name>,
}
