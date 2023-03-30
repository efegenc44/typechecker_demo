use crate::token::Token;
use crate::ast::{ Node, BinaryOp, UnaryOp };

pub struct Parser {
    tokens: Vec<Token>,
    cursor: usize,
}

/// place expression, value expression ?

/// TODO: fix assignment expression
///   we don't check for invalid lhs expressions
///   but we should
///   Maybe -> set a = 4;

/// TODO: seperate declarations and statements, maybe?

// program      = statement* <end>
// statement    = fun
//              | let
//              | struct
//              | enum
//              | return
//              | if
//              | block
//              | typeof
//              | expression ';'
// typeof       = 'typeof' expression ';'
// block        = '{' statement* '}'
// if           = 'if' '(' expr ')' statement ('else' statement)?
// return       = 'return' expr ';'
// enum         = 'enum' sym '{' (sym (',' sym)*)? '}'
// type-vars    = ('<' sym (',' sym)* '>')?
// struct       = 'struct' sym type-vars '{' (typed-symbol (',' typed-symbol)*)? '}'
// fun          = 'fun' sym type-vars '(' (typed-symbol (',' typed-symbol)*)? ')' ('->' type)? block
// let          = 'let' sym ('::' typ) '=' expr ';'
// typed-symbol = sym '::' typ
// type         = sym type-vars?
//              | 'fun' '(' (type (',' type)*)? ')' ('->' type)?
// expr         = arithmetic ('=' expr)?
// arithmetic   = term (('+' | '-') term)*
// term         = product (('*' | '/') product)*
// product      = sym
//              | string
//              | '(' expr ')'
//              | '(' expr (',' expr)+  ')'
//              | ('+' | '-') product
//              | (integer | float) sym?
//              | product ('::' '<' type (',' type)* '>')?
//              | product '(' (expr (',' expr)*)? ')'
//              | product '.' sym

type ParseError = String;
type ParseResult = Result<Node, ParseError>;

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser { tokens, cursor: 0 }
    }
    fn current(&self) -> Token {
        self.tokens[self.cursor].clone()
    }

    fn advance(&mut self) -> Token {
        self.cursor += 1;
        self.tokens[self.cursor - 1].clone()
    }
 
    fn consume(&mut self, token: Token) -> Result<(), ParseError> {
        if self.current() == token {
            self.advance();
            return Ok(())
        }
        Err(format!("Expected '{:?}' but got '{:?}'", token, self.current()))
    }

    // Now I started to think that GADTs are cool.
    fn consume_symbol(&mut self) -> Result<String, ParseError> {
        match self.current() {
            Token::Symbol(sym) => {
                self.advance();
                Ok(sym)
            },
            _ => Err(format!("Expected Symbol but got '{:?}'", self.current()))
        }
    }

    fn matches(&mut self, token: &Token) -> bool {
        if self.current() != *token {
            return false
        }
        self.advance(); true
    }

    fn comma_seperated<T>(
        &mut self, 
        f: fn(&mut Parser) -> Result<T, ParseError>, 
        until: Token
    ) -> Result<Vec<T>, ParseError> {
        let mut vec = vec![];
        if !self.matches(&until) {
            vec.push(f(self)?);
            while !self.matches(&until) {
                self.consume(Token::Comma)?;
                vec.push(f(self)?)
            }
        }
        Ok(vec)
    }

    fn product(&mut self) -> ParseResult {
        use Token::*;

        let token = self.advance();

        let mut left = match token {
            Symbol(sym) => Node::Symbol(sym.clone()),
            String(text) => Node::String(text.clone()),
            Ktrue => Node::True,
            Kfalse => Node::False,
            Float(float) => 
                match self.current() {
                    Symbol(sym) => {
                        self.advance();
                        Node::Binary(
                            Box::new(Node::Float(float)), 
                            BinaryOp::Mul, 
                            Box::new(Node::Symbol(sym))
                        )
                    }
                    _ => Node::Float(float)
                },
            Uinteger(uint) => 
                match self.current() {
                    Symbol(sym) => {
                        self.advance();
                        Node::Binary(
                            Box::new(Node::Uinteger(uint)), 
                            BinaryOp::Mul, 
                            Box::new(Node::Symbol(sym))
                        )
                    }
                    _ => Node::Uinteger(uint)
                },
            Lparen => {
                let expr = self.expression()?;
                self.consume(Rparen)?;
                expr
            },
            Plus => Node::Unary(UnaryOp::Plu, Box::new(self.product()?)),
            Minus => Node::Unary(UnaryOp::Neg, Box::new(self.product()?)),
            _ => return Err(format!("Unexpected token '{:?}'", token))
        };
        
        loop {            
            if self.matches(&Lparen) {
                let args = self.comma_seperated(Parser::expression, Token::Rparen)?;
                left = Node::Call(Box::new(left), args);
            } else if self.matches(&Point) {
                let field = self.consume_symbol()?;
                left = Node::Projection(Box::new(left), field); 
            } else if self.matches(&Dcolon) {
                self.consume(Langle)?;
                let typs = self.comma_seperated(Parser::typ, Token::Rangle)?;
                left = Node::TypeCall(Box::new(left), typs);
            } else if self.matches(&Lparen) {
                let args = self.comma_seperated(Parser::expression, Token::Rparen)?;
                left = Node::Call(Box::new(left), args);
            } else {
                break;
            }
        }
        Ok(left)
    }

    fn term(&mut self) -> ParseResult {
        use Token::*;
        
        let mut left = self.product()?;
        while let Slash | Star = self.current() {
            let op = self.current().to_bop(); 
            self.advance();
            left = Node::Binary(Box::new(left), op, Box::new(self.product()?))   
        }
        Ok(left)
    }

    fn arithmetic(&mut self) -> ParseResult {
        use Token::*;
        
        let mut left = self.term()?;
        while let Minus | Plus = self.current() {
            let op = self.current().to_bop();
            self.advance();
            left = Node::Binary(Box::new(left), op, Box::new(self.term()?))   
        }
        Ok(left)
    }

    fn expression(&mut self) -> ParseResult {
        let left = self.arithmetic()?;
        if self.matches(&Token::Equal) {
            let right = self.expression()?;
            return Ok(Node::Binary(Box::new(left), BinaryOp::Assign, Box::new(right)))
        }
        Ok(left)
    }
    
    fn typ(&mut self) -> ParseResult {
        match self.advance() {
            Token::Symbol(sym) => {
                if self.matches(&Token::Langle) {
                    let typs = self.comma_seperated(Self::typ, Token::Rangle)?;
                    Ok(Node::GenericType(sym, typs))
                } else {
                    Ok(Node::Symbol(sym))
                }
            },
            Token::Kfun => {
                self.consume(Token::Lparen)?;
                let arg_types = self.comma_seperated(Parser::typ, Token::Rparen)?;

                let ret;
                if self.matches(&Token::Arrow) {
                    ret = Box::new(self.typ()?);
                } else {
                    ret = Box::new(Node::NoType);     
                }
                Ok(Node::FuncType(arg_types, ret))
            },
            _ => return Err(format!("Unexpected token '{:?}'", self.current()))
        }
    }

    fn typed_symbol(&mut self) -> ParseResult {
        let sym = self.consume_symbol()?;
        self.consume(Token::Dcolon)?;
        let typ = Box::new(self.typ()?);

        Ok(Node::TypedSymbol(sym, typ))
    }

    fn type_vars(&mut self) -> Result<Option<Vec<String>>, ParseError> {
        let typ_params;
        if self.matches(&Token::Langle) {
            typ_params = Some(self.comma_seperated(Self::consume_symbol, Token::Rangle)?); 
        } else {
            typ_params = None;
        }
        Ok(typ_params)
    }


    fn fun(&mut self) -> ParseResult {
        self.advance();

        let name = self.consume_symbol()?;
        let type_vars = self.type_vars()?;
        self.consume(Token::Lparen)?;    
        let args = self.comma_seperated(Parser::typed_symbol, Token::Rparen)?;

        let ret;
        if self.matches(&Token::Arrow) {
            ret = Box::new(self.typ()?);
        } else {
            ret = Box::new(Node::NoType);     
        }
        
        let body = Box::new(self.block()?);
        
        Ok(Node::Fun(name, type_vars, args, ret, body))
    }
    
    fn lett(&mut self) -> ParseResult {
        self.advance();

        let name = self.consume_symbol()?;
        let typ;
        if self.matches(&Token::Dcolon) {
            typ = Box::new(self.typ()?);
        } else {
            typ = Box::new(Node::NoType);  
        }
        self.consume(Token::Equal)?;
        let expr = Box::new(self.expression()?);
        self.consume(Token::Semicolon)?;
        
        Ok(Node::Let(name, typ, expr))
    }

    fn structt(&mut self) -> ParseResult {
        self.advance();

        let name = self.consume_symbol()?;
        let typ_params = self.type_vars()?;
        
        self.consume(Token::Lcurly)?;
        let members = self.comma_seperated(Self::typed_symbol, Token::Rcurly)?; 
        
        Ok(Node::Struct(name, typ_params, members))
    }

    fn enumm(&mut self) -> ParseResult {
        self.advance();

        let name = self.consume_symbol()?;
        self.consume(Token::Lcurly)?;
        let members = self.comma_seperated(Self::consume_symbol, Token::Rcurly)?; 
        
        Ok(Node::Enum(name, members))
    }

    fn returnn(&mut self) -> ParseResult {
        self.advance();

        let expr;
        if self.matches(&Token::Semicolon) {
            expr = Box::new(Node::NoExpr);
        } else {
            expr = Box::new(self.expression()?);
            self.consume(Token::Semicolon)?;
        }

        Ok(Node::Return(expr))
    }

    fn iff(&mut self) -> ParseResult {
        self.advance();

        self.consume(Token::Lparen)?;
        let expr = Box::new(self.expression()?);
        self.consume(Token::Rparen)?;
        let body = Box::new(self.statement()?);
        let elseb = if self.matches(&Token::Kelse) {
            Box::new(self.statement()?)
        } else {
            Box::new(Node::Empty)
        };
        
        Ok(Node::If(expr, body, elseb))
    }

    fn block(&mut self) -> ParseResult {
        self.consume(Token::Lcurly)?;

        let mut states = vec![];
        while !self.matches(&Token::Rcurly) {
            states.push(self.statement()?);
        }
        Ok(Node::Block(states))
    }

    fn typeoff(&mut self) -> ParseResult {
        self.advance();

        let expr = Box::new(self.expression()?);
        self.consume(Token::Semicolon)?;

        Ok(Node::Typeof(expr))
    }
    
    fn statement(&mut self) -> ParseResult {
        match self.current() {
            Token::Kfun => self.fun(),
            Token::Klet => self.lett(),
            Token::Kstruct => self.structt(),
            Token::Kenum => self.enumm(),
            Token::Kreturn => self.returnn(),
            Token::Kif => self.iff(),
            Token::Ktypeof => self.typeoff(),
            Token::Lcurly => self.block(),
            _ => {
                let node = self.expression();
                self.consume(Token::Semicolon)?;
                node
            }
        }
    }

    pub fn parse(&mut self) -> ParseResult {
        let mut states = vec![];
        while !self.matches(&Token::End) {
            states.push(self.statement()?);
        }
        Ok(Node::Program(states))
    }

}