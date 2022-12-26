use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use crate::{
    ast::ModulePath,
    lowering::ModuleId,
    tokenizer::{AssignmentOperator, Attribute, Literal, Operator},
};

use super::{
    context::{ModuleTypeCheckCtx, StructId, TypeRef},
    type_sig::{FunctionType, TypedFunctionParameter, TypedStructField},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypedModule<'ast> {
    pub fn_defs: Vec<TypedFunctionDefinition<'ast>>,
    pub fn_decls: Vec<TypedFunctionDeclaration<'ast>>,
    pub structs: Vec<TypedStructDeclaration<'ast>>,
    pub submodules: HashMap<String, ModuleId>,
    pub dependencies: Vec<TypedImport<'ast>>,
    pub parent: Option<ModuleId>,
    pub exports: HashMap<String, TypedExport<'ast>>,
    pub path: ModulePath,
    pub name: String,
    pub id: ModuleId,
    pub ctx: ModuleTypeCheckCtx<'ast>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypedExport<'export> {
    pub name: String,
    pub ty: TypedExportType<'export>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TypedExportType<'export> {
    Function(FunctionType<'export>),
    Struct(StructId /* StructType<'export> */),
    Module(ModuleId),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypedImport<'a> {
    pub name: String,
    pub source_module: ModuleId,
    pub local: bool,
    pub ty: TypedExportType<'a>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TypedDeclaration<'ast> {
    FunctionDef(TypedFunctionDefinition<'ast>),
    FunctionDecl(TypedFunctionDeclaration<'ast>),
    Struct(TypedStructDeclaration<'ast>),
    #[allow(unused)]
    Module(TypedModule<'ast>),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypedFunctionDefinition<'ast> {
    pub name: String,
    pub ret_ty: Option<TypeRef<'ast>>,
    pub params: HashMap<String, TypedFunctionParameter<'ast>>,
    pub body: TypedBlock<'ast>,
    pub fn_ty: TypeRef<'ast>,
    pub variadic: bool,
    pub has_self_param: bool,
    pub attrs: Vec<Attribute>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypedFunctionDeclaration<'ast> {
    pub name: String,
    pub ret_ty: Option<TypeRef<'ast>>,
    pub params: HashMap<String, TypedFunctionParameter<'ast>>,
    pub fn_ty: TypeRef<'ast>,
    pub variadic: bool,
    pub attrs: Vec<Attribute>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypedStructDeclaration<'ast> {
    pub name: String,
    pub fields: Vec<TypedStructField<'ast>>,
    pub methods: HashMap<String, TypedFunctionDefinition<'ast>>,
    pub ty: TypeRef<'ast>,
    pub id: usize,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypedStructInitializer<'ast> {
    pub struct_name: String,
    pub struct_ty: TypeRef<'ast>,
    pub fields: Vec<TypedStructInitializerField<'ast>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypedStructInitializerField<'ast> {
    pub field_name: String,
    pub value: TypedExpression<'ast>,
    pub idx: u32,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypedFunctionCall<'ast> {
    pub callee: Box<TypedExpression<'ast>>,
    pub args: Vec<TypedExpression<'ast>>,
    pub fn_ty: TypeRef<'ast>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypedExpression<'ast> {
    pub ty: Option<TypeRef<'ast>>,
    pub expr: TypedExpressionData<'ast>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TypedExpressionData<'ast> {
    BinaryOp {
        left: Box<TypedExpression<'ast>>,
        right: Box<TypedExpression<'ast>>,
        op: Operator,
    },
    LogicalOp {
        left: Box<TypedExpression<'ast>>,
        right: Box<TypedExpression<'ast>>,
        op: Operator,
    },
    UnaryOp {
        expr: Box<TypedExpression<'ast>>,
        op: Operator,
    },
    Identifier {
        name: String,
    },
    Literal {
        literal: TypedLiteral<'ast>,
    },
    If {
        expr: TypedIfExpression<'ast>,
    },
    Block {
        block: TypedBlock<'ast>,
    },
    VarAssignment {
        var_assign: TypedVarAssignment<'ast>,
    },
    FnCall {
        fn_call: TypedFunctionCall<'ast>,
    },
    MemberAccess {
        member_access: TypedMemberAccess<'ast>,
    },
    StructInitializer {
        struct_init: TypedStructInitializer<'ast>,
    },
    SizeOfExpr {
        ty: TypeRef<'ast>,
    },
    AsExpr {
        expr: Box<TypedExpression<'ast>>,
        ty: TypeRef<'ast>,
    },
}

impl<'ast> TypedExpressionData<'ast> {
    pub fn variant_str(&self) -> String {
        match self {
            TypedExpressionData::BinaryOp { .. } => "Binary Op".into(),
            TypedExpressionData::LogicalOp { .. } => "Logical Op".into(),
            TypedExpressionData::UnaryOp { .. } => "Unary Op".into(),
            TypedExpressionData::Identifier { .. } => "Identifier".into(),
            TypedExpressionData::Literal { .. } => "Literal".into(),
            TypedExpressionData::If { .. } => "If".into(),
            TypedExpressionData::Block { .. } => "Block".into(),
            TypedExpressionData::VarAssignment { .. } => "Assignment".into(),
            TypedExpressionData::FnCall { .. } => "Fn Call".into(),
            TypedExpressionData::MemberAccess { .. } => "MemberAccess".into(),
            TypedExpressionData::StructInitializer { .. } => "Struct Init".into(),
            TypedExpressionData::SizeOfExpr { .. } => "Size Of".into(),
            TypedExpressionData::AsExpr { .. } => "As".into(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypedLiteral<'ast> {
    pub ty: TypeRef<'ast>,
    pub literal: Literal,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypedMemberAccess<'ast> {
    pub object: Box<TypedExpression<'ast>>,
    pub member: Box<TypedExpression<'ast>>,
    pub computed: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TypedStatement<'ast> {
    Variable(TypedVarDeclaration<'ast>),
    Expression(TypedExpression<'ast>),
    Loop(TypedLoopStatement<'ast>),
    Return(TypedReturnStatement<'ast>),
    Result(TypedResultStatement<'ast>),
    Continue,
    Break,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypedVarAssignment<'ast> {
    pub operator: AssignmentOperator,
    pub left: Box<TypedExpression<'ast>>,
    pub right: Box<TypedExpression<'ast>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypedVarDeclaration<'ast> {
    pub name: String,
    pub ty: TypeRef<'ast>,
    pub initializer: Option<TypedExpression<'ast>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypedIfExpression<'ast> {
    pub condition: Box<TypedExpression<'ast>>,
    pub body: Box<TypedExpression<'ast>>,
    pub else_body: Box<Option<TypedExpression<'ast>>>,
    pub result_ty: Option<TypeRef<'ast>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypedLoopStatement<'ast> {
    pub body: TypedBlock<'ast>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypedReturnStatement<'ast> {
    pub value: Option<TypedExpression<'ast>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypedResultStatement<'ast> {
    pub value: TypedExpression<'ast>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypedBlock<'ast> {
    pub statements: Vec<TypedStatement<'ast>>,
    pub ty: Option<TypeRef<'ast>>,
}
