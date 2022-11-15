// Author: Will Hopkins

use std::fmt::{Display, Formatter};

use nom::{
    branch::alt,
    bytes::{complete::tag, streaming::is_not},
    character::{
        self,
        complete::{alpha1, alphanumeric1, multispace0, space0},
    },
    combinator::{map, recognize},
    multi::many0_count,
    sequence::{pair, preceded, terminated, tuple},
    Err, IResult,
};
use nom_locate::{position, LocatedSpan};

pub type Span<'a> = LocatedSpan<&'a str>;

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub position: TokenPosition,
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} at {}", self.kind, self.position)
    }
}

impl PartialEq<TokenKind> for Token {
    fn eq(&self, other: &TokenKind) -> bool {
        self.kind == *other
    }
}

#[derive(Debug, PartialEq, Clone)]
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
    Literal(Literal),
    Operator(Operator),
    Keyword(Keyword),
    Symbol(Symbol),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    String(String),
    Int(i64),
    Float(f64),
    Char(char),
    Bool(bool),
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
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Operator {
    Assign(AssignmentOperator),
    Plus,
    Minus,
    Times,
    Divide,
    Modulo,
    Power,
    Equals,
    NotEquals,
    LessThan,
    LessThanEquals,
    GreaterThan,
    GreaterThanEquals,
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
            Operator::LessThanEquals => OperatorType::Relational,
            Operator::GreaterThan => OperatorType::Relational,
            Operator::GreaterThanEquals => OperatorType::Relational,
            Operator::And => OperatorType::LogicalAnd,
            Operator::Or => OperatorType::LogicalOr,
            Operator::Not => OperatorType::LogicalNot,
            Operator::BitwiseAnd => OperatorType::Bitwise,
            Operator::BitwiseOr => OperatorType::Bitwise,
            Operator::BitwiseXor => OperatorType::Bitwise,
            Operator::BitwiseNot => OperatorType::Bitwise,
            Operator::BitwiseLeftShift => OperatorType::Bitwise,
            Operator::BitwiseRightShift => OperatorType::Bitwise,
        }
    }

    pub fn is_unary(&self) -> bool {
        let t = self.op_type();
        t == OperatorType::LogicalNot || t == OperatorType::Additive
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
            Operator::LessThanEquals => write!(f, "<="),
            Operator::GreaterThan => write!(f, ">"),
            Operator::GreaterThanEquals => write!(f, ">="),
            Operator::And => write!(f, "&&"),
            Operator::Or => write!(f, "||"),
            Operator::Not => write!(f, "!"),
            Operator::BitwiseAnd => write!(f, "&"),
            Operator::BitwiseOr => write!(f, "|"),
            Operator::BitwiseXor => write!(f, "^"),
            Operator::BitwiseNot => write!(f, "~"),
            Operator::BitwiseLeftShift => write!(f, "<<"),
            Operator::BitwiseRightShift => write!(f, ">>"),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
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
    Colon,
    Comma,
    Dot,
    Arrow,
}

fn keyword(input: Span) -> IResult<Span, TokenKind> {
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

fn ops1(input: Span) -> IResult<Span, Span> {
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

fn ops2(input: Span) -> IResult<Span, Span> {
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
    ))(input)
}

fn operator(input: Span) -> IResult<Span, TokenKind> {
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
                "<=" => TokenKind::Operator(Operator::LessThanEquals),
                ">" => TokenKind::Operator(Operator::GreaterThan),
                ">=" => TokenKind::Operator(Operator::GreaterThanEquals),
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

fn symbol(input: Span) -> IResult<Span, TokenKind> {
    match alt((
        tag("("),
        tag(")"),
        tag("{"),
        tag("}"),
        tag("["),
        tag("]"),
        tag(";"),
        tag(":"),
        tag(","),
        tag("."),
        tag("->"),
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
                ":" => TokenKind::Symbol(Symbol::Colon),
                "," => TokenKind::Symbol(Symbol::Comma),
                "." => TokenKind::Symbol(Symbol::Dot),
                "->" => TokenKind::Symbol(Symbol::Arrow),
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

fn float_literal(input: Span) -> IResult<Span, TokenKind> {
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
    Ok((input, TokenKind::Literal(Literal::Float(val))))
}

fn integer_literal(input: Span) -> IResult<Span, TokenKind> {
    let (input, digits) = nom::character::complete::digit1(input)?;
    let Ok(val) = digits.parse() else {
        return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Digit)));
    };
    Ok((input, TokenKind::Literal(Literal::Int(val))))
}

fn string_literal(input: Span) -> IResult<Span, TokenKind> {
    let (input, (_, text, _)) = tuple((tag("\""), is_not("\""), tag("\"")))(input)?;
    Ok((input, TokenKind::Literal(Literal::String(text.to_string()))))
}

fn char_literal(input: Span) -> IResult<Span, TokenKind> {
    let (input, (_, character, _)) = tuple((
        tag("\'"),
        character::complete::satisfy(|c| c != '\''),
        tag("\'"),
    ))(input)?;
    Ok((input, TokenKind::Literal(Literal::Char(character))))
}

fn boolean_literal(input: Span) -> IResult<Span, TokenKind> {
    alt((
        map(tag("true"), |_| TokenKind::Literal(Literal::Bool(true))),
        map(tag("false"), |_| TokenKind::Literal(Literal::Bool(false))),
    ))(input)
}

fn identifier(input: Span) -> IResult<Span, TokenKind> {
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

fn token(input: Span) -> IResult<Span, TokenKind> {
    let (input, token) = terminated(
        preceded(
            multispace0,
            alt((
                keyword,
                symbol,
                operator,
                float_literal,
                integer_literal,
                string_literal,
                char_literal,
                boolean_literal,
                identifier,
            )),
        ),
        multispace0,
    )(input)?;
    Ok((input, token))
}

pub fn tokenize(input: Span) -> IResult<Span, Vec<Token>> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut remaining = input;
    loop {
        match token(remaining) {
            Ok((new_remaining, token)) => {
                remaining = new_remaining;
                let pos = position(remaining)?;
                let a = pos.0;
                let b = pos.1;
                tokens.push(Token {
                    kind: token,
                    position: TokenPosition {
                        line: b.location_line() as usize,
                        column: b.get_column(),
                        offset: b.location_offset(),
                    },
                });
            }
            Err(Err::Error(_)) => {
                return Ok((remaining, tokens));
            }
            Err(Err::Failure(_)) => {
                return Ok((remaining, tokens));
            }
            Err(Err::Incomplete(_)) => {
                return Ok((remaining, tokens));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn keyword_Test() {
        let input = "let x = 5;";
        let input = Span::from(input);
        let expected = vec![
            TokenKind::Keyword(Keyword::Let),
            TokenKind::Identifier("x".to_string()),
            TokenKind::Operator(Operator::Assign(AssignmentOperator::Assign)),
            TokenKind::Literal(Literal::Int(5)),
            TokenKind::Symbol(Symbol::Semicolon),
        ];
        let (remaining, tokens) = tokenize(input).unwrap();
        assert_eq!(remaining.to_string(), String::from(""));
        assert_eq!(tokens, expected);
    }

    #[test]
    fn tokenize_test1() {
        let input = "let x = 5;";
        let input = Span::from(input);
        let expected = vec![
            TokenKind::Keyword(Keyword::Let),
            TokenKind::Identifier("x".to_string()),
            TokenKind::Operator(Operator::Assign(AssignmentOperator::Assign)),
            TokenKind::Literal(Literal::Int(5)),
            TokenKind::Symbol(Symbol::Semicolon),
        ];
        let (remaining, tokens) = tokenize(input).unwrap();
        assert_eq!(remaining.to_string(), String::from(""));
        assert_eq!(tokens, expected);
    }

    #[test]
    fn tokenize_test2() {
        let input = "let x = 5.5;";
        let input = Span::from(input);
        let expected = vec![
            TokenKind::Keyword(Keyword::Let),
            TokenKind::Identifier("x".to_string()),
            TokenKind::Operator(Operator::Assign(AssignmentOperator::Assign)),
            TokenKind::Literal(Literal::Float(5.5)),
            TokenKind::Symbol(Symbol::Semicolon),
        ];
        let (remaining, tokens) = tokenize(input).unwrap();
        assert_eq!(remaining.to_string(), String::from(""));
        assert_eq!(tokens, expected);
    }

    #[test]
    fn tokenize_test3() {
        let input = "
fn main() {
    let x = 5;
    let y = 6;
    let z = x + y;
}        
";
        let input = Span::from(input);
        let expected = vec![
            TokenKind::Keyword(Keyword::Function),
            TokenKind::Identifier("main".to_string()),
            TokenKind::Symbol(Symbol::OpenParen),
            TokenKind::Symbol(Symbol::CloseParen),
            TokenKind::Symbol(Symbol::OpenBrace),
            TokenKind::Keyword(Keyword::Let),
            TokenKind::Identifier("x".to_string()),
            TokenKind::Operator(Operator::Assign(AssignmentOperator::Assign)),
            TokenKind::Literal(Literal::Int(5)),
            TokenKind::Symbol(Symbol::Semicolon),
            TokenKind::Keyword(Keyword::Let),
            TokenKind::Identifier("y".to_string()),
            TokenKind::Operator(Operator::Assign(AssignmentOperator::Assign)),
            TokenKind::Literal(Literal::Int(6)),
            TokenKind::Symbol(Symbol::Semicolon),
            TokenKind::Keyword(Keyword::Let),
            TokenKind::Identifier("z".to_string()),
            TokenKind::Operator(Operator::Assign(AssignmentOperator::Assign)),
            TokenKind::Identifier("x".to_string()),
            TokenKind::Operator(Operator::Plus),
            TokenKind::Identifier("y".to_string()),
            TokenKind::Symbol(Symbol::Semicolon),
            TokenKind::Symbol(Symbol::CloseBrace),
        ];
        let (remaining, tokens) = tokenize(input).unwrap();
        assert_eq!(remaining.to_string(), String::from(""));
        assert_eq!(tokens, expected);
    }

    #[test]
    fn if_statement() {
        let input = "
fn main() {
    let x = 5;
    let y = 6;
    if x > y {
        let z = x;
    } else {
        let z = y;
    }
}";
        let input = Span::from(input);
        let expected = [
            TokenKind::Keyword(Keyword::Function),
            TokenKind::Identifier("main".to_string()),
            TokenKind::Symbol(Symbol::OpenParen),
            TokenKind::Symbol(Symbol::CloseParen),
            TokenKind::Symbol(Symbol::OpenBrace),
            TokenKind::Keyword(Keyword::Let),
            TokenKind::Identifier("x".to_string()),
            TokenKind::Operator(Operator::Assign(AssignmentOperator::Assign)),
            TokenKind::Literal(Literal::Int(5)),
            TokenKind::Symbol(Symbol::Semicolon),
            TokenKind::Keyword(Keyword::Let),
            TokenKind::Identifier("y".to_string()),
            TokenKind::Operator(Operator::Assign(AssignmentOperator::Assign)),
            TokenKind::Literal(Literal::Int(6)),
            TokenKind::Symbol(Symbol::Semicolon),
            TokenKind::Keyword(Keyword::If),
            TokenKind::Identifier("x".to_string()),
            TokenKind::Operator(Operator::GreaterThan),
            TokenKind::Identifier("y".to_string()),
            TokenKind::Symbol(Symbol::OpenBrace),
            TokenKind::Keyword(Keyword::Let),
            TokenKind::Identifier("z".to_string()),
            TokenKind::Operator(Operator::Assign(AssignmentOperator::Assign)),
            TokenKind::Identifier("x".to_string()),
            TokenKind::Symbol(Symbol::Semicolon),
            TokenKind::Symbol(Symbol::CloseBrace),
            TokenKind::Keyword(Keyword::Else),
            TokenKind::Symbol(Symbol::OpenBrace),
            TokenKind::Keyword(Keyword::Let),
            TokenKind::Identifier("z".to_string()),
            TokenKind::Operator(Operator::Assign(AssignmentOperator::Assign)),
            TokenKind::Identifier("y".to_string()),
            TokenKind::Symbol(Symbol::Semicolon),
            TokenKind::Symbol(Symbol::CloseBrace),
            TokenKind::Symbol(Symbol::CloseBrace),
        ];
        let (remaining, tokens) = tokenize(input).unwrap();
        assert_eq!(remaining.to_string(), String::from(""));
        assert_eq!(tokens, expected);
    }
}
