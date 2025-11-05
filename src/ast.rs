use std::{cell::RefCell, rc::Rc};

use crate::tokenizer::{AssignmentOperator, Attribute, Literal, TokenPosition};

use super::tokenizer::Operator;

#[derive(Debug, PartialEq, Clone)]
pub struct NodeSpan {
    pub start: TokenPosition,
    pub end: TokenPosition,
}

impl std::fmt::Display for NodeSpan {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.start)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Module {
    pub fn_defs: Vec<FunctionDefinition>,
    pub fn_decls: Vec<FunctionDeclaration>,
    pub structs: Vec<StructDeclaration>,
    pub submodules: Vec<Rc<RefCell<Module>>>,
    pub dependencies: Vec<Use>,
    pub parent: Option<Rc<RefCell<Module>>>,
    pub name: String,
    pub path: ModulePath,
}

pub type ModulePath = Vec<String>;

pub trait FmtPath {
    fn fmt_path(&self) -> String;
    fn fmt_path_no_main(&self) -> String;
}

impl FmtPath for ModulePath {
    fn fmt_path(&self) -> String {
        self.join("::")
    }

    fn fmt_path_no_main(&self) -> String {
        let mut path = self.fmt_path();
        if path.starts_with("main::") {
            path = path[6..].to_string();
        }
        path
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Use {
    pub item_path: ModulePath,
    pub local: bool,
    pub span: NodeSpan,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDefinition {
    pub name: String,
    pub return_type: Option<String>,
    pub params: Vec<FunctionParameter>,
    pub body: Block,
    pub variadic: bool,
    pub span: NodeSpan,
    pub attrs: Vec<Attribute>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDeclaration {
    pub name: String,
    pub return_type: Option<String>,
    pub params: Vec<FunctionParameter>,
    pub variadic: bool,
    pub attrs: Vec<Attribute>,
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
    ScopeResolution {
        scope_resolution: ScopeResolution,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub struct ScopeResolution {
    pub object: Box<Expression>,
    pub member: Box<Expression>,
    pub span: NodeSpan,
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
