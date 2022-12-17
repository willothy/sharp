use crate::tokenizer::{AssignmentOperator, Literal, TokenPosition};

use super::tokenizer::Operator;

#[derive(Debug, PartialEq, Clone)]
pub struct NodeSpan {
    pub start: TokenPosition,
    pub end: TokenPosition,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Module {
    pub body: Vec<Declaration>,
    pub submodules: Vec<Module>,
    pub requirements: Vec<Module>,
    pub name: String,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Declaration {
    FunctionDef(FunctionDefinition),
    FunctionDecl(FunctionDeclaration),
    Struct(StructDeclaration),
    #[allow(unused)]
    // TODO: Implement modules
    Module(Module),
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDefinition {
    pub name: String,
    pub return_type: Option<String>,
    pub params: Vec<FunctionParameter>,
    pub body: Block,
    pub variadic: bool,
    pub span: NodeSpan,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDeclaration {
    pub name: String,
    pub return_type: Option<String>,
    pub params: Vec<FunctionParameter>,
    pub variadic: bool,
    //pub span: NodeSpan,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructDeclaration {
    pub name: String,
    pub fields: Vec<StructField>,
    pub methods: Vec<FunctionDefinition>,
    pub span: NodeSpan,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructField {
    pub name: String,
    pub type_name: String,
    pub idx: u32,
    pub span: NodeSpan,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructInitializer {
    pub struct_name: String,
    pub fields: Vec<StructInitializerField>,
    pub span: NodeSpan,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructInitializerField {
    pub field_name: String,
    pub value: Expression,
    pub idx: u32,
    pub span: NodeSpan,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionCall {
    pub callee: Box<Expression>,
    pub args: Vec<Expression>,
    pub span: NodeSpan,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionParameter {
    pub name: String,
    pub type_name: String,
    pub idx: u32,
    pub span: NodeSpan,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    BinaryOp {
        left: Box<Expression>,
        right: Box<Expression>,
        op: Operator,
        span: NodeSpan,
    },
    LogicalOp {
        left: Box<Expression>,
        right: Box<Expression>,
        op: Operator,
        span: NodeSpan,
    },
    UnaryOp {
        expr: Box<Expression>,
        op: Operator,
        span: NodeSpan,
    },
    Identifier {
        name: String,
        span: NodeSpan,
    },
    Literal {
        literal: Literal,
    },
    If {
        expr: IfExpression,
    },
    Block {
        block: Block,
    },
    VarAssignment {
        var_assign: VarAssignment,
    },
    FnCall {
        fn_call: FunctionCall,
    },
    MemberAccess {
        member_access: MemberAccess,
    },
    StructInitializer {
        struct_init: StructInitializer,
    },
    SizeOfExpr {
        expr: Box<Expression>,
        span: NodeSpan,
    },
    AsExpr {
        expr: Box<Expression>,
        type_name: String,
        span: NodeSpan,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub struct MemberAccess {
    pub object: Box<Expression>,
    pub member: Box<Expression>,
    pub computed: bool,
    pub span: NodeSpan,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Variable(VarDeclaration),
    Expression(Expression),
    Loop(LoopStatement),
    Return(ReturnStatement),
    Result(ResultStatement),
    Continue,
    Break,
}

#[derive(Debug, PartialEq, Clone)]
pub struct VarAssignment {
    pub operator: AssignmentOperator,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
    pub span: NodeSpan,
}

#[derive(Debug, PartialEq, Clone)]
pub struct VarDeclaration {
    pub name: String,
    pub type_name: String,
    pub initializer: Option<Expression>,
    pub span: NodeSpan,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IfExpression {
    pub condition: Box<Expression>,
    pub body: Box<Expression>,
    pub else_body: Box<Option<Expression>>,
    pub span: NodeSpan,
}

#[derive(Debug, PartialEq, Clone)]
pub struct LoopStatement {
    pub body: Block,
    pub span: NodeSpan,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ReturnStatement {
    pub value: Option<Expression>,
    pub span: NodeSpan,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ResultStatement {
    pub value: Expression,
    pub span: NodeSpan,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub span: NodeSpan,
}
