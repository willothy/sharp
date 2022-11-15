use crate::tokenizer::{AssignmentOperator, Literal};

use super::tokenizer::{Keyword, Operator, Symbol, TokenKind};

#[derive(Debug, PartialEq, Clone)]
pub struct Module {
    pub body: Vec<Declaration>,
    pub submodules: Vec<Module>,
    pub requirements: Vec<Module>,
    pub name: String,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Declaration {
    Function(FunctionDeclaration),
    Struct(StructDeclaration),
    Module(Module),
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDeclaration {
    pub name: String,
    pub return_type: Option<String>,
    pub params: Vec<FunctionParameter>,
    pub body: Block,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructDeclaration {
    pub name: String,
    pub fields: Vec<StructField>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructField {
    pub name: String,
    pub type_name: String,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructInitializer {
    pub struct_name: String,
    pub fields: Vec<StructInitializerField>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructInitializerField {
    pub field_name: String,
    pub value: Expression,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionCall {
    pub callee: Box<Expression>,
    pub args: Vec<Expression>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionParameter {
    pub name: String,
    pub type_name: String,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    BinaryOp {
        left: Box<Expression>,
        right: Box<Expression>,
        op: Operator,
    },
    LogicalOp {
        left: Box<Expression>,
        right: Box<Expression>,
        op: Operator,
    },
    UnaryOp {
        expr: Box<Expression>,
        op: Operator,
    },
    Identifier {
        name: String,
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
}

#[derive(Debug, PartialEq, Clone)]
pub struct MemberAccess {
    pub object: Box<Expression>,
    pub member: Box<Expression>,
    pub computed: bool,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Variable(VarDeclaration),
    Expression(Expression),
    Loop(LoopStatement),
    Return(ReturnStatement),
    Yield(YieldStatement),
    Continue,
    Break,
}

#[derive(Debug, PartialEq, Clone)]
pub struct VarAssignment {
    pub operator: AssignmentOperator,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct VarDeclaration {
    pub name: String,
    pub type_name: String,
    pub initializer: Option<Expression>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IfExpression {
    pub condition: Box<Expression>,
    pub body: Box<Statement>,
    pub else_body: Box<Option<Statement>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct LoopStatement {
    pub body: Block,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ReturnStatement {
    pub value: Option<Expression>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct YieldStatement {
    pub value: Expression,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Block {
    pub statements: Vec<Statement>,
}

/* pub fn parse_function(tokens: &Vec<Token>) -> IResult<usize, FunctionDeclaration> {
    todo!()
}

pub fn parse(mut tokens: Vec<Token>) -> IResult<Vec<Token>, Vec<FunctionDeclaration>> {
    let mut functions: Vec<FunctionDeclaration> = Vec::new();

    let mut iter_tokens = &mut tokens.iter();

    let mut to_skip: usize = 0;
    while let Some(token) = iter_tokens.skip(to_skip).next() {
        match token {
            Token::Keyword(Keyword::Function) => {
                let Ok((consumed, function)) = parse_function(&tokens) else {
                    return Err(nom::Err::Error(nom::error::Error::new(
                        tokens,
                        nom::error::ErrorKind::Tag,
                    )));
                };
                functions.push(function);
                to_skip = consumed;
            }
            _ => {
                return Err(nom::Err::Error(nom::error::Error::new(
                    tokens,
                    nom::error::ErrorKind::Tag,
                )));
            }
        }
    }

    Ok((tokens, functions))
}
 */

pub fn parse_file(
    tokens: &Vec<TokenKind>,
) -> Result<Vec<FunctionDeclaration>, Box<dyn std::error::Error>> {
    let mut functions: Vec<FunctionDeclaration> = Vec::new();

    todo!()
}
