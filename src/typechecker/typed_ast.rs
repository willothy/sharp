use crate::tokenizer::{AssignmentOperator, Literal, Operator};

use super::type_sig::TypeSignature;

#[derive(Debug, PartialEq, Clone)]
pub struct Module<'ast> {
    pub body: Vec<Declaration<'ast>>,
    pub submodules: Vec<Module<'ast>>,
    pub requirements: Vec<Module<'ast>>,
    pub name: String,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Declaration<'ast> {
    Function(FunctionDeclaration<'ast>),
    Struct(StructDeclaration<'ast>),
    Module(Module<'ast>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDeclaration<'ast> {
    pub name: String,
    pub return_type: Option<TypeSignature<'ast>>,
    pub params: Vec<FunctionParameter<'ast>>,
    pub body: Block<'ast>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructDeclaration<'ast> {
    pub name: String,
    pub fields: Vec<StructField<'ast>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructField<'ast> {
    pub name: String,
    pub ty: TypeSignature<'ast>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructInitializer<'ast> {
    pub struct_name: String,
    pub fields: Vec<StructInitializerField<'ast>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructInitializerField<'ast> {
    pub field_name: String,
    pub value: Expression<'ast>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionCall<'ast> {
    pub callee: Box<Expression<'ast>>,
    pub args: Vec<Expression<'ast>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionParameter<'ast> {
    pub name: String,
    pub ty: TypeSignature<'ast>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression<'ast> {
    BinaryOp {
        left: Box<Expression<'ast>>,
        right: Box<Expression<'ast>>,
        op: Operator,
        ty: TypeSignature<'ast>,
    },
    LogicalOp {
        left: Box<Expression<'ast>>,
        right: Box<Expression<'ast>>,
        op: Operator,
    },
    UnaryOp {
        expr: Box<Expression<'ast>>,
        op: Operator,
    },
    Identifier {
        name: String,
    },
    Literal {
        literal: Literal,
    },
    If {
        expr: IfExpression<'ast>,
    },
    Block {
        block: Block<'ast>,
    },
    VarAssignment {
        var_assign: VarAssignment<'ast>,
    },
    FnCall {
        fn_call: FunctionCall<'ast>,
    },
    MemberAccess {
        member_access: MemberAccess<'ast>,
    },
    StructInitializer {
        struct_init: StructInitializer<'ast>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub struct MemberAccess<'ast> {
    pub object: Box<Expression<'ast>>,
    pub member: Box<Expression<'ast>>,
    pub computed: bool,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement<'ast> {
    Variable(VarDeclaration<'ast>),
    Expression(Expression<'ast>),
    Loop(LoopStatement<'ast>),
    Return(ReturnStatement<'ast>),
    Yield(YieldStatement<'ast>),
    Continue,
    Break,
}

#[derive(Debug, PartialEq, Clone)]
pub struct VarAssignment<'ast> {
    pub operator: AssignmentOperator,
    pub left: Box<Expression<'ast>>,
    pub right: Box<Expression<'ast>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct VarDeclaration<'ast> {
    pub name: String,
    pub ty: TypeSignature<'ast>,
    pub initializer: Option<Expression<'ast>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IfExpression<'ast> {
    pub condition: Box<Expression<'ast>>,
    pub body: Box<Statement<'ast>>,
    pub else_body: Box<Option<Statement<'ast>>>,
    pub yield_ty: Option<TypeSignature<'ast>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct LoopStatement<'ast> {
    pub body: Block<'ast>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ReturnStatement<'ast> {
    pub value: Option<Expression<'ast>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct YieldStatement<'ast> {
    pub value: Expression<'ast>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Block<'ast> {
    pub statements: Vec<Statement<'ast>>,
}
