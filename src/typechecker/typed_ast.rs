use std::collections::HashMap;

use crate::tokenizer::{AssignmentOperator, Literal, Operator};

use super::{
    context::TypeRef,
    type_sig::{TypedFunctionParameter, TypedStructField},
};

#[derive(Debug, PartialEq, Clone)]
pub struct TypedModule<'ast> {
    pub body: Vec<TypedDeclaration<'ast>>,
    //pub submodules: Vec<Module<'ast>>,
    //pub requirements: Vec<Module<'ast>>,
    pub name: String,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypedDeclaration<'ast> {
    FunctionDef(TypedFunctionDefinition<'ast>),
    FunctionDecl(TypedFunctionDeclaration<'ast>),
    Struct(TypedStructDeclaration<'ast>),
    #[allow(unused)]
    Module(TypedModule<'ast>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypedFunctionDefinition<'ast> {
    pub name: String,
    pub ret_ty: Option<TypeRef<'ast>>,
    pub params: HashMap<String, TypedFunctionParameter<'ast>>,
    pub body: TypedBlock<'ast>,
    pub fn_ty: TypeRef<'ast>,
    pub variadic: bool,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypedFunctionDeclaration<'ast> {
    pub name: String,
    pub ret_ty: Option<TypeRef<'ast>>,
    pub params: HashMap<String, TypedFunctionParameter<'ast>>,
    pub fn_ty: TypeRef<'ast>,
    pub variadic: bool,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypedStructDeclaration<'ast> {
    pub name: String,
    pub fields: Vec<TypedStructField<'ast>>,
    pub ty: TypeRef<'ast>,
    pub id: usize,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypedStructInitializer<'ast> {
    pub struct_name: String,
    pub struct_ty: TypeRef<'ast>,
    pub fields: Vec<TypedStructInitializerField<'ast>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypedStructInitializerField<'ast> {
    pub field_name: String,
    pub value: TypedExpression<'ast>,
    pub idx: u32,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypedFunctionCall<'ast> {
    pub callee: Box<TypedExpression<'ast>>,
    pub args: Vec<TypedExpression<'ast>>,
    pub fn_ty: TypeRef<'ast>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypedExpression<'ast> {
    pub ty: Option<TypeRef<'ast>>,
    pub expr: TypedExpressionData<'ast>,
}

#[derive(Debug, PartialEq, Clone)]
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
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypedLiteral<'ast> {
    pub ty: TypeRef<'ast>,
    pub literal: Literal,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypedMemberAccess<'ast> {
    pub object: Box<TypedExpression<'ast>>,
    pub member: Box<TypedExpression<'ast>>,
    pub computed: bool,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypedStatement<'ast> {
    Variable(TypedVarDeclaration<'ast>),
    Expression(TypedExpression<'ast>),
    Loop(TypedLoopStatement<'ast>),
    Return(TypedReturnStatement<'ast>),
    Result(TypedResultStatement<'ast>),
    Continue,
    Break,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypedVarAssignment<'ast> {
    pub operator: AssignmentOperator,
    pub left: Box<TypedExpression<'ast>>,
    pub right: Box<TypedExpression<'ast>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypedVarDeclaration<'ast> {
    pub name: String,
    pub ty: TypeRef<'ast>,
    pub initializer: Option<TypedExpression<'ast>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypedIfExpression<'ast> {
    pub condition: Box<TypedExpression<'ast>>,
    pub body: Box<TypedExpression<'ast>>,
    pub else_body: Box<Option<TypedExpression<'ast>>>,
    pub result_ty: Option<TypeRef<'ast>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypedLoopStatement<'ast> {
    pub body: TypedBlock<'ast>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypedReturnStatement<'ast> {
    pub value: Option<TypedExpression<'ast>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypedResultStatement<'ast> {
    pub value: TypedExpression<'ast>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypedBlock<'ast> {
    pub statements: Vec<TypedStatement<'ast>>,
    pub ty: Option<TypeRef<'ast>>,
}
