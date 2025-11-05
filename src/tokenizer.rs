// Author: Will Hopkins

use std::fmt::{Display, Formatter};

use nom::{
    branch::alt,
    bytes::{
        complete::{tag, take_until},
        streaming::is_not,
    },
    character::{
        self,
        complete::{alpha1, alphanumeric1, multispace0},
    },
    combinator::{map, opt, recognize},
    multi::{many0_count, separated_list1},
    sequence::{pair, preceded, terminated, tuple},
    Err, IResult,
};
use nom_locate::{position, LocatedSpan};
use serde::{Deserialize, Serialize};

pub type Span<'a> = LocatedSpan<&'a str>;

#[derive(Debug, PartialEq, Clone)]
pub struct Token<'t> {
    pub kind: TokenKind,
    pub position: Span<'t>,
}

impl<'t> Display for Token<'t> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} at {}", self.kind, self.position)
    }
}

impl<'t> PartialEq<TokenKind> for Token<'t> {
    fn eq(&self, other: &TokenKind) -> bool {
        self.kind == *other
    }
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct TokenPosition {
    pub line: usize,
    pub column: usize,
    pub offset: usize,
}

impl TokenPosition {
    pub fn new() -> Self {
        Self {
            line: 0,
            column: 0,
            offset: 0,
        }
    }
}

impl From<Span<'_>> for TokenPosition {
    fn from(span: Span<'_>) -> Self {
        Self {
            line: span.location_line() as usize,
            column: span.get_column() as usize,
            offset: span.location_offset(),
        }
    }
}

impl Display for TokenPosition {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "line {}, column {}, offset {}",
            self.line, self.column, self.offset
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    Identifier(String),
    Attributes(Vec<Attribute>),
    Literal(Literal),
    Operator(Operator),
    Keyword(Keyword),
    Symbol(Symbol),
    Comment,
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct Attribute {
    pub val: String,
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum Literal {
    Str(String, TokenPosition),
    Int(i64, TokenPosition),
    Float(f64, TokenPosition),
    Char(char, TokenPosition),
    Bool(bool, TokenPosition),
}

impl Literal {
    pub fn position(&self) -> TokenPosition {
        match self {
            Literal::Str(_, p) => p.clone(),
            Literal::Int(_, p) => p.clone(),
            Literal::Float(_, p) => p.clone(),
            Literal::Char(_, p) => p.clone(),
            Literal::Bool(_, p) => p.clone(),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Keyword {
    Let,
    If,
    Else,
    Loop,
    Break,
    Return,
    Function,
    Continue,
    Yield,
    Struct,
    Use,
    Module,
    Impl,
    Self_,
    //As,
    SizeOf,
}

#[derive(Debug, PartialEq, Clone, Copy, Serialize, Deserialize)]
pub enum Operator {
    Assign(AssignmentOperator),
    As,
    Plus,
    Minus,
    Times,
    Divide,
    Modulo,
    Power,
    Equals,
    NotEquals,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
    And,
    Or,
    Not,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseNot,
    BitwiseLeftShift,
    BitwiseRightShift,
}

impl Operator {
    pub fn op_type(&self) -> OperatorType {
        match self {
            Operator::Assign(_) => OperatorType::Assignment,
            Operator::Plus => OperatorType::Additive,
            Operator::Minus => OperatorType::Additive,
            Operator::Times => OperatorType::Multiplicative,
            Operator::Divide => OperatorType::Multiplicative,
            Operator::Modulo => OperatorType::Multiplicative,
            Operator::Power => OperatorType::Multiplicative,
            Operator::Equals => OperatorType::Equality,
            Operator::NotEquals => OperatorType::Equality,
            Operator::LessThan => OperatorType::Relational,
            Operator::LessOrEqual => OperatorType::Relational,
            Operator::GreaterThan => OperatorType::Relational,
            Operator::GreaterOrEqual => OperatorType::Relational,
            Operator::And => OperatorType::LogicalAnd,
            Operator::Or => OperatorType::LogicalOr,
            Operator::Not => OperatorType::LogicalNot,
            Operator::BitwiseAnd => OperatorType::Bitwise,
            Operator::BitwiseOr => OperatorType::Bitwise,
            Operator::BitwiseXor => OperatorType::Bitwise,
            Operator::BitwiseNot => OperatorType::Bitwise,
            Operator::BitwiseLeftShift => OperatorType::Bitwise,
            Operator::BitwiseRightShift => OperatorType::Bitwise,
            Operator::As => OperatorType::Cast,
        }
    }

    pub fn is_unary(&self) -> bool {
        let t = self.op_type();
        if t == OperatorType::LogicalNot || t == OperatorType::Additive {
            return true;
        }
        match self {
            Operator::Plus | Operator::Minus | Operator::Not | Operator::Times => true,
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum OperatorType {
    Assignment,
    Additive,
    Multiplicative,
    LogicalAnd,
    LogicalOr,
    LogicalNot,
    Equality,
    Relational,
    Bitwise,
    Cast,
}

impl Display for AssignmentOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            AssignmentOperator::AddAssign => write!(f, "+="),
            AssignmentOperator::SubAssign => write!(f, "-="),
            AssignmentOperator::MulAssign => write!(f, "*="),
            AssignmentOperator::DivAssign => write!(f, "/="),
            AssignmentOperator::ModAssign => write!(f, "%="),
            AssignmentOperator::Assign => write!(f, "="),
        }
    }
}

impl std::fmt::Display for Operator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Operator::Assign(op) => {
                write!(f, "{}", op)
            }
            Operator::Plus => write!(f, "+"),
            Operator::Minus => write!(f, "-"),
            Operator::Times => write!(f, "*"),
            Operator::Divide => write!(f, "/"),
            Operator::Modulo => write!(f, "%"),
            Operator::Power => write!(f, "^"),
            Operator::Equals => write!(f, "=="),
            Operator::NotEquals => write!(f, "!="),
            Operator::LessThan => write!(f, "<"),
            Operator::LessOrEqual => write!(f, "<="),
            Operator::GreaterThan => write!(f, ">"),
            Operator::GreaterOrEqual => write!(f, ">="),
            Operator::And => write!(f, "&&"),
            Operator::Or => write!(f, "||"),
            Operator::Not => write!(f, "!"),
            Operator::BitwiseAnd => write!(f, "&"),
            Operator::BitwiseOr => write!(f, "|"),
            Operator::BitwiseXor => write!(f, "^"),
            Operator::BitwiseNot => write!(f, "~"),
            Operator::BitwiseLeftShift => write!(f, "<<"),
            Operator::BitwiseRightShift => write!(f, ">>"),
            Operator::As => write!(f, "as"),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy, Serialize, Deserialize)]
pub enum AssignmentOperator {
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    Assign,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Symbol {
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,
    Semicolon,
    DoubleColon,
    Colon,
    Comma,
    Dot,
    RightArrow,
    LeftArrow,
    Ellipsis,
}

fn keyword<'a>(input: Span<'a>) -> IResult<Span<'a>, TokenKind> {
    match alt((
        tag("let"),
        tag("if"),
        tag("else"),
        tag("loop"),
        tag("break"),
        tag("return"),
        tag("continue"),
        tag("struct"),
        tag("yield"),
        tag("fn"),
        tag("use"),
        tag("mod"),
        //tag("as"),
        //tag("self"),
        tag("impl"),
        tag("sizeof"),
    ))(input)
    {
        Ok((input, keyword)) => Ok((
            input,
            match keyword.to_string().as_str() {
                "let" => TokenKind::Keyword(Keyword::Let),
                "if" => TokenKind::Keyword(Keyword::If),
                "else" => TokenKind::Keyword(Keyword::Else),
                "loop" => TokenKind::Keyword(Keyword::Loop),
                "break" => TokenKind::Keyword(Keyword::Break),
                "return" => TokenKind::Keyword(Keyword::Return),
                "continue" => TokenKind::Keyword(Keyword::Continue),
                "struct" => TokenKind::Keyword(Keyword::Struct),
                "yield" => TokenKind::Keyword(Keyword::Yield),
                "fn" => TokenKind::Keyword(Keyword::Function),
                "use" => TokenKind::Keyword(Keyword::Use),
                "mod" => TokenKind::Keyword(Keyword::Module),
                "self" => TokenKind::Keyword(Keyword::Self_),
                "impl" => TokenKind::Keyword(Keyword::Impl),
                "sizeof" => TokenKind::Keyword(Keyword::SizeOf),
                _ => {
                    return Err(Err::Error(nom::error::Error::new(
                        input,
                        nom::error::ErrorKind::Tag,
                    )))
                }
            },
        )),
        Err(err) => Err(err),
    }
}

fn ops1<'a>(input: Span<'a>) -> IResult<Span<'a>, Span> {
    alt((
        tag("+"),
        tag("-"),
        tag("*"),
        tag("/"),
        tag("%"),
        tag("^"),
        tag("=="),
        tag("!="),
        tag("="),
        tag("+="),
        tag("-="),
        tag("*="),
        tag("/="),
        tag("<"),
        tag("<="),
    ))(input)
}

fn ops2<'a>(input: Span<'a>) -> IResult<Span<'a>, Span> {
    alt((
        tag(">"),
        tag(">="),
        tag("&&"),
        tag("||"),
        tag("!"),
        tag("&"),
        tag("|"),
        tag("^"),
        tag("~"),
        tag("<<"),
        tag(">>"),
        tag("%="),
        tag("as"),
    ))(input)
}

fn operator<'a>(input: Span<'a>) -> IResult<Span<'a>, TokenKind> {
    match alt((ops1, ops2))(input) {
        Ok((input, operator)) => Ok((
            input,
            match operator.to_string().as_str() {
                "+" => TokenKind::Operator(Operator::Plus),
                "-" => TokenKind::Operator(Operator::Minus),
                "*" => TokenKind::Operator(Operator::Times),
                "/" => TokenKind::Operator(Operator::Divide),
                "%" => TokenKind::Operator(Operator::Modulo),
                "**" => TokenKind::Operator(Operator::Power),
                "==" => TokenKind::Operator(Operator::Equals),
                "!=" => TokenKind::Operator(Operator::NotEquals),
                "<" => TokenKind::Operator(Operator::LessThan),
                "<=" => TokenKind::Operator(Operator::LessOrEqual),
                ">" => TokenKind::Operator(Operator::GreaterThan),
                ">=" => TokenKind::Operator(Operator::GreaterOrEqual),
                "&&" => TokenKind::Operator(Operator::And),
                "||" => TokenKind::Operator(Operator::Or),
                "!" => TokenKind::Operator(Operator::Not),
                "&" => TokenKind::Operator(Operator::BitwiseAnd),
                "|" => TokenKind::Operator(Operator::BitwiseOr),
                "^" => TokenKind::Operator(Operator::BitwiseXor),
                "~" => TokenKind::Operator(Operator::BitwiseNot),
                "<<" => TokenKind::Operator(Operator::BitwiseLeftShift),
                ">>" => TokenKind::Operator(Operator::BitwiseRightShift),
                "=" => TokenKind::Operator(Operator::Assign(AssignmentOperator::Assign)),
                "+=" => TokenKind::Operator(Operator::Assign(AssignmentOperator::AddAssign)),
                "-=" => TokenKind::Operator(Operator::Assign(AssignmentOperator::SubAssign)),
                "*=" => TokenKind::Operator(Operator::Assign(AssignmentOperator::MulAssign)),
                "/=" => TokenKind::Operator(Operator::Assign(AssignmentOperator::DivAssign)),
                "%=" => TokenKind::Operator(Operator::Assign(AssignmentOperator::ModAssign)),
                "as" => TokenKind::Operator(Operator::As),
                _ => {
                    return Err(Err::Error(nom::error::Error::new(
                        input,
                        nom::error::ErrorKind::Tag,
                    )))
                }
            },
        )),
        Err(err) => Err(err),
    }
}

fn symbol<'a>(input: Span<'a>) -> IResult<Span<'a>, TokenKind> {
    match alt((
        tag("("),
        tag(")"),
        tag("{"),
        tag("}"),
        tag("["),
        tag("]"),
        tag(";"),
        tag("::"),
        tag(":"),
        tag("..."),
        tag(","),
        tag("."),
        tag("->"),
        tag("<-"),
    ))(input)
    {
        Ok((input, symbol)) => Ok((
            input,
            match symbol.to_string().as_str() {
                "(" => TokenKind::Symbol(Symbol::OpenParen),
                ")" => TokenKind::Symbol(Symbol::CloseParen),
                "{" => TokenKind::Symbol(Symbol::OpenBrace),
                "}" => TokenKind::Symbol(Symbol::CloseBrace),
                "[" => TokenKind::Symbol(Symbol::OpenBracket),
                "]" => TokenKind::Symbol(Symbol::CloseBracket),
                ";" => TokenKind::Symbol(Symbol::Semicolon),
                "::" => TokenKind::Symbol(Symbol::DoubleColon),
                ":" => TokenKind::Symbol(Symbol::Colon),
                "," => TokenKind::Symbol(Symbol::Comma),
                "." => TokenKind::Symbol(Symbol::Dot),
                "..." => TokenKind::Symbol(Symbol::Ellipsis),
                "->" => TokenKind::Symbol(Symbol::RightArrow),
                "<-" => TokenKind::Symbol(Symbol::LeftArrow),
                _ => {
                    return Err(Err::Error(nom::error::Error::new(
                        input,
                        nom::error::ErrorKind::Tag,
                    )))
                }
            },
        )),
        Err(err) => Err(err),
    }
}

fn float_literal<'a>(input: Span<'a>) -> IResult<Span<'a>, TokenKind> {
    /* let (input, digits) = nom::character::complete::digit0(input)?;
    let (input, _) = tag(".")(input)?;
    let (input, decimal_digits) = nom::character::complete::digit1(input)?; */
    let (input, (digits, _, decimal_digits)) = tuple((
        nom::character::complete::digit0,
        tag("."),
        nom::character::complete::digit1,
    ))(input)?;
    let float_literal = format!("{}.{}", digits, decimal_digits);
    let Ok(val) = float_literal.parse() else {
        return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Float)));
    };
    Ok((input, TokenKind::Literal(Literal::Float(val, input.into()))))
}

fn integer_literal<'a>(input: Span<'a>) -> IResult<Span<'a>, TokenKind> {
    let (input, digits) = nom::character::complete::digit1(input)?;
    let Ok(val) = digits.parse() else {
        return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Digit)));
    };
    Ok((input, TokenKind::Literal(Literal::Int(val, input.into()))))
}

fn string_literal<'a>(input: Span<'a>) -> IResult<Span<'a>, TokenKind> {
    let (input, (_, text, _)) = tuple((tag("\""), opt(is_not("\"")), tag("\"")))(input)?;
    let text = if let Some(text) = text {
        let mut text = text.to_string();
        text = text.replace("\\n", "\n"); // Newline
        text = text.replace("\\t", "\t"); // Tab
        text = text.replace("\\r", "\r"); // Carriage return
        text = text.replace("\\\"", "\""); // Double quote
        text = text.replace("\\'", "'"); // Single quote
        text = text.replace("\\\"", "\""); // Double quote
        text
    } else {
        "".to_owned()
    };

    Ok((input, TokenKind::Literal(Literal::Str(text, input.into()))))
}

fn char_literal<'a>(input: Span<'a>) -> IResult<Span<'a>, TokenKind> {
    let (input, (_, character, _)) = tuple((
        tag("\'"),
        character::complete::satisfy(|c| c != '\''),
        tag("\'"),
    ))(input)?;
    Ok((
        input,
        TokenKind::Literal(Literal::Char(character, input.into())),
    ))
}

fn boolean_literal<'a>(input: Span<'a>) -> IResult<Span<'a>, TokenKind> {
    alt((
        map(tag("true"), |_| {
            TokenKind::Literal(Literal::Bool(true, input.into()))
        }),
        map(tag("false"), |_| {
            TokenKind::Literal(Literal::Bool(false, input.into()))
        }),
    ))(input)
}

fn comment<'a>(input: Span<'a>) -> IResult<Span<'a>, TokenKind> {
    // single or multiline comments
    let (input, result) = alt((tag("//"), tag("/*")))(input)?;
    match result.fragment() {
        &"//" => {
            let (input, _) = take_until("\n")(input)?;
            Ok((input, TokenKind::Comment))
        }
        &"/*" => {
            let (input, _) = tuple((take_until("*/"), tag("*/")))(input)?;
            Ok((input, TokenKind::Comment))
        }
        _ => Err(Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        ))),
    }
}

fn identifier<'a>(input: Span<'a>) -> IResult<Span<'a>, TokenKind> {
    /* let (input, first_char) = character::complete::alpha1(input)?;
    let (input, rest) = character::complete::alphanumeric0(input)?;
    let identifier = format!("{}{}", first_char, rest);
    Ok((input, Token::Identifier(identifier))) */
    //let (input, first_char) = alt((alpha1, tag("_")))(input)?;

    let (input, ident) = recognize(pair(
        alt((alpha1, tag("_"))),
        many0_count(alt((alphanumeric1, tag("_")))),
    ))(input)?;

    Ok((input, TokenKind::Identifier(ident.to_string())))
}

fn attribute<'a>(input: Span<'a>) -> IResult<Span<'a>, TokenKind> {
    let (input, _) = tag("#(")(input)?;
    /* let (input, list) = separated_list1(
        terminated(tag(","), multispace0),
        ,
    )(input)?; */

    // Comma separated list, optional spaces, no trailing comma, indentifiers can be any alphanumeric character, including underscores and dashes
    let (input, list) = separated_list1(
        terminated(tag(","), multispace0),
        recognize(pair(
            alt((alpha1, tag("_"))),
            many0_count(alt((alphanumeric1, tag("_"), tag("-")))),
        )),
    )(input)?;
    let (input, _) = tag(")")(input)?;

    Ok((
        input,
        TokenKind::Attributes(
            list.iter()
                .map(|s| Attribute { val: s.to_string() })
                .collect(),
        ),
    ))
}

fn token<'a>(input: Span<'a>) -> IResult<Span<'a>, TokenKind> {
    let (input, token) = terminated(
        preceded(
            multispace0,
            alt((
                comment,
                keyword,
                symbol,
                operator,
                float_literal,
                integer_literal,
                string_literal,
                char_literal,
                boolean_literal,
                identifier,
                attribute,
            )),
        ),
        multispace0,
    )(input)?;
    Ok((input, token))
}

pub fn tokenize<'a>(input: Span<'a>) -> Option<Vec<Token>> /* IResult<Span<'a>, Vec<Token>> */ {
    let mut tokens: Vec<Token> = Vec::new();
    let mut remaining = input;
    while let Ok((input, token)) = token(remaining) {
        remaining = input;
        let pos: IResult<Span<'a>, _> = position(remaining);
        let pos = match pos {
            Ok((pos, _)) => pos,
            Err(_) => {
                /* return Err(Err::Error(nom::error::Error::new(
                    remaining,
                    nom::error::ErrorKind::Verify,
                ))) */
                return None;
            }
        };

        tokens.push(Token {
            kind: token,
            position: pos, /* TokenPosition {
                               line: pos.location_line() as usize,
                               column: pos.get_column(),
                               offset: pos.location_offset(),
                           } */
        });
    }
    let tokens = tokens
        .drain(..)
        .filter(|tok| !matches!(tok.kind, TokenKind::Comment))
        .collect();
    //Ok((Span::new(""), tokens))
    Some(tokens)
}
