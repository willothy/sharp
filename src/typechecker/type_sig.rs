use std::{collections::HashMap, hash::Hash};

use super::context::TypeRef;

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
            sig: TypeSignature::Empty,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeSignature<'t> {
    Primitive(PrimitiveType),
    Struct(Option<StructType<'t>>),
    Function(FunctionType<'t>),
    Empty,
}

impl<'t> TypeSignature<'t> {
    pub fn is_empty(&self) -> bool {
        matches!(self, TypeSignature::Empty)
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

    pub fn get_return_type(&'t self) -> Result<Option<TypeRef>, String> {
        match self {
            TypeSignature::Function(f) => Ok(f.return_type.clone()),
            _ => Err(format!("Type {:?} is not a function", self)),
        }
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
            TypeSignature::Empty => 0.hash(state),
        }
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
