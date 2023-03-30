use crate::ast::BinaryOp;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Symbol(String),
    String(String),
    Uinteger(usize),
    Float(f32),
    Equal,
    Lparen, 
    Rparen,
    Plus, 
    Minus, 
    Star, 
    Slash,
    Colon,
    Dcolon,
    Arrow,
    Comma,
    Semicolon,
    Lcurly,
    Rcurly,
    Point,
    Langle,
    Rangle,

    Kfun,
    Klet,
    Kstruct,
    Kenum,
    Kreturn,
    Kif,
    Kelse,
    Ktrue,
    Kfalse,
    Ktypeof,

    Space,
    Creturn,
    Tab,
    Newline,

    Comment,

    End,
}

impl Token {
    pub fn to_bop(&self) -> BinaryOp {
        match self {
            Token::Plus  => BinaryOp::Add,
            Token::Minus => BinaryOp::Sub,
            Token::Slash => BinaryOp::Div,
            Token::Star  => BinaryOp::Mul,
            Token::Equal => BinaryOp::Assign,
            _ => unreachable!()
        }
    }
}