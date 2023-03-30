type Symbol = String;

#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Assign
}

impl std::fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOp::Add    => write!(f, "+"),
            BinaryOp::Sub    => write!(f, "-"),
            BinaryOp::Mul    => write!(f, "*"),
            BinaryOp::Div    => write!(f, "/"),
            BinaryOp::Assign => write!(f, "="),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOp {
    Plu,
    Neg,
}

impl std::fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOp::Plu => write!(f, "+"),
            UnaryOp::Neg => write!(f, "-"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Program(Vec<Node>),
    Block(Vec<Node>),
    Binary(Box<Node>, BinaryOp, Box<Node>),
    Unary(UnaryOp, Box<Node>),
    Uinteger(usize),
    Float(f32),
    String(Symbol),
    Symbol(Symbol),
    True,
    False,
    Fun(Symbol, Option<Vec<Symbol>>, Vec<Node>, Box<Node>, Box<Node>),
    TypedSymbol(Symbol, Box<Node>),
    Call(Box<Node>, Vec<Node>),
    TypeCall(Box<Node>, Vec<Node>),
    Projection(Box<Node>, Symbol),
    FuncType(Vec<Node>, Box<Node>),
    Let(Symbol, Box<Node>, Box<Node>),
    Struct(Symbol, Option<Vec<Symbol>>, Vec<Node>),
    Enum(Symbol, Vec<Symbol>),
    Return(Box<Node>),
    If(Box<Node>, Box<Node>, Box<Node>),
    GenericType(Symbol, Vec<Node>),
    Typeof(Box<Node>),
    Empty,
    NoType,
    NoExpr
}

// Freaking Love RUST \o/ <- me
pub fn vec_string<T: std::fmt::Display>(vec: &Vec<T>) -> String {
    let mut text = String::new();
    let mut first = true; 
    for element in vec {
        if first {
            text += &format!("{element}");
            first = false
        } else {
            text += &format!(", {element}");
        }
    }
    text
}

impl std::fmt::Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Node::Program(states) => write!(f, "{}", vec_string(states)),
            Node::Block(states) => write!(f, "{}", vec_string(states)),
            Node::Binary(left, op, right) => write!(f, "({} {} {})", op, left, right),
            Node::Unary(op, operand) => write!(f, "({} {})", op, operand),
            Node::Uinteger(uint) => write!(f, "{uint}"),
            Node::Float(float) => write!(f, "{float}f"),
            Node::String(text) => write!(f, "\"{text}\""),
            Node::Symbol(sym) => write!(f, "{sym}"),
            Node::True => write!(f, "true"),
            Node::False => write!(f, "false"),
            // TODO: fix display typ_params
            Node::Fun(name, _type_vars, args, ret, body) => write!(f, "(fun {name} ({}) ({ret}) {body})", vec_string(args)),
            Node::TypedSymbol(sym, typ) => write!(f, "{sym}::{typ}"),
            // TODO: fix display typ_params
            Node::Call(fexpr, args) => write!(f, "({fexpr} {})", vec_string(args)),
            Node::TypeCall(expr, args) => write!(f, "({expr} {})", vec_string(args)),
            Node::Projection(expr, member) => write!(f, "(prj {expr} {member})"),
            Node::Let(name, typ, expr) => write!(f, "(let {} ({}) {})", name, typ, expr),
            Node::FuncType(domain, codomain) => write!(f, "fun({}) -> {codomain}", vec_string(domain)),
            // TODO: fix display typ_params
            Node::Struct(name, _typ_params, members) => write!(f, "(struct {name} ({}))", vec_string(members)),
            Node::Enum(name, syms) => write!(f, "(enum {name} ({}))", vec_string(syms)),
            Node::Return(expr) => write!(f, "(return {expr})"),
            // TODO: fix
            Node::If(expr, body, elseb) => write!(f, "(if {expr} {} (else {}))", body, elseb),
            Node::GenericType(typ, typ_args) => write!(f, "{typ}<{}>", vec_string(typ_args)),
            Node::Typeof(expr) => write!(f, "(typeof {expr})"),
            Node::Empty => write!(f, "<>"),
            Node::NoType => write!(f, "{{NoType}}"),
            Node::NoExpr => write!(f, "{{NoExpr}}"),
        }
    }
}