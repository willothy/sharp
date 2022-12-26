use std::{collections::HashMap, hash::Hash};

use serde::{Deserialize, Serialize};
use serde_with::serde_as;

use crate::tokenizer::Attribute;

use super::{
    context::{StructId, TypeRef},
    typed_ast::TypedFunctionDefinition,
};

#[derive(Debug, Clone, Serialize, Deserialize)]
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
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
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

    fn can_cast_to_prim(&self, p2: &PrimitiveType) -> bool {
        match p2 {
            PrimitiveType::I8 => true,
            PrimitiveType::I16 => true,
            PrimitiveType::I32 => true,
            PrimitiveType::I64 => true,
            PrimitiveType::F32 => false,
            PrimitiveType::F64 => false,
            PrimitiveType::Bool => false,
            PrimitiveType::Char => false,
            PrimitiveType::Str => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum TypeSignature<'t> {
    Primitive(PrimitiveType),
    Struct(StructId),
    Function(FunctionType<'t>),
    Pointer(PointerType<'t>),
    Void,
}

impl<'t> TypeSignature<'t> {
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

    pub fn is_struct_type(&self) -> bool {
        match self {
            TypeSignature::Struct(_) => true,
            _ => false,
        }
    }

    pub fn struct_id(&self) -> Option<StructId> {
        match self {
            TypeSignature::Struct(s) => Some(*s),
            _ => None,
        }
    }

    pub fn get_base_type(&self) -> TypeSignature<'t> {
        match self {
            TypeSignature::Pointer(p) => p.target.get_base_type(),
            _ => self.clone(),
        }
    }

    pub fn string_repr(&self, structs: &HashMap<StructId, StructType<'t>>) -> String {
        match self {
            TypeSignature::Primitive(p) => match p {
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
            .into(),
            TypeSignature::Struct(id) => {
                format!("{}", structs.get(id).unwrap().name)
            }
            TypeSignature::Function(_) => "fn".into(),
            TypeSignature::Void => "void".into(),
            TypeSignature::Pointer(p) => {
                format!("*{}", p.target.string_repr(structs))
            }
        }
    }

    pub fn can_cast_to(&self, other: &Self) -> bool {
        match (self, other) {
            (TypeSignature::Primitive(p1), TypeSignature::Primitive(p2)) => p1.can_cast_to_prim(p2),
            (TypeSignature::Pointer(_), TypeSignature::Pointer(_)) => true,
            (TypeSignature::Pointer(p1), TypeSignature::Primitive(p2)) => p1.can_cast_to_prim(p2),
            (TypeSignature::Primitive(p1), TypeSignature::Pointer(_)) => p1.can_cast_to_ptr(),
            _ => false,
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

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
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

impl PrimitiveType {
    pub fn can_cast_to_prim(&self, other: &Self) -> bool {
        match (self, other) {
            (PrimitiveType::I8, PrimitiveType::I8) => true,
            (PrimitiveType::I8, PrimitiveType::I16) => true,
            (PrimitiveType::I8, PrimitiveType::I32) => true,
            (PrimitiveType::I8, PrimitiveType::I64) => true,
            (PrimitiveType::I8, PrimitiveType::F32) => true,
            (PrimitiveType::I8, PrimitiveType::F64) => true,
            (PrimitiveType::I16, PrimitiveType::I16) => true,
            (PrimitiveType::I16, PrimitiveType::I32) => true,
            (PrimitiveType::I16, PrimitiveType::I64) => true,
            (PrimitiveType::I16, PrimitiveType::F32) => true,
            (PrimitiveType::I16, PrimitiveType::F64) => true,
            (PrimitiveType::I32, PrimitiveType::I32) => true,
            (PrimitiveType::I32, PrimitiveType::I64) => true,
            (PrimitiveType::I32, PrimitiveType::F32) => true,
            (PrimitiveType::I32, PrimitiveType::F64) => true,
            (PrimitiveType::I64, PrimitiveType::I64) => true,
            (PrimitiveType::I64, PrimitiveType::F32) => true,
            (PrimitiveType::I64, PrimitiveType::F64) => true,
            (PrimitiveType::F32, PrimitiveType::F32) => true,
            (PrimitiveType::F32, PrimitiveType::F64) => true,
            (PrimitiveType::F64, PrimitiveType::F64) => true,
            (PrimitiveType::Bool, PrimitiveType::Bool) => true,
            (PrimitiveType::Char, PrimitiveType::Char) => true,
            (PrimitiveType::Str, PrimitiveType::Str) => true,
            _ => false,
        }
    }

    pub fn can_cast_to_ptr(&self) -> bool {
        match self {
            PrimitiveType::I8 => true,
            PrimitiveType::I16 => true,
            PrimitiveType::I32 => true,
            PrimitiveType::I64 => true,
            PrimitiveType::F32 => false,
            PrimitiveType::F64 => false,
            PrimitiveType::Bool => false,
            PrimitiveType::Char => false,
            PrimitiveType::Str => false,
        }
    }
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

#[serde_as]
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StructType<'struct_type> {
    pub name: String,
    #[serde_as(as = "Vec<(_, _)>")]
    pub fields: HashMap<String, TypedStructField<'struct_type>>,
    #[serde_as(as = "Vec<(_, _)>")]
    pub methods: HashMap<String, TypedFunctionDefinition<'struct_type>>,
    pub id: usize,
}
impl<'struct_type> StructType<'struct_type> {
    pub(crate) fn get_member_idx(&self, name: &String) -> Option<u32> {
        for field in &self.fields {
            if field.1.name == *name {
                return Some(field.1.idx);
            }
        }
        None
    }
}

impl<'struct_type> PartialEq for StructType<'struct_type> {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl<'struct_type> Eq for StructType<'struct_type> {}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TypedStructField<'field> {
    pub name: String,
    pub ty: TypeRef<'field>,
    pub idx: u32,
}

#[serde_as]
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FunctionType<'fn_type> {
    pub return_type: Option<TypeRef<'fn_type>>,
    #[serde_as(as = "Vec<(_, _)>")]
    pub params: HashMap<String, TypedFunctionParameter<'fn_type>>,
    pub variadic: bool,
    pub has_self_param: bool,
    pub name: String,
    pub attrs: Vec<Attribute>,
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

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TypedFunctionParameter<'fn_param> {
    pub name: String,
    pub ty: TypeRef<'fn_param>,
    pub idx: u32,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Name<'name> {
    pub ty: TypeRef<'name>,
}
