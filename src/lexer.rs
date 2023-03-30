use crate::token::Token;

pub struct Lexer {
    chars: Vec<char>,
    cursor: usize,
}

type LexError = String;
type LexResult = Result<Token, LexError>;

impl Lexer {
    pub fn new(mut source: String) -> Lexer {
        source += "\0";
        Lexer { chars: source.chars().collect(), cursor: 0 }
    }
    
    fn current(&self) -> char {
        self.chars[self.cursor]
    }
    
    fn matchch(&mut self, ch: char) -> bool {
        if self.current() == ch {
            self.cursor += 1;
            return true;
        }
        false
    }

    fn symbol(&mut self) -> LexResult {
        use Token::*;
        
        let mut text = "".to_string();
        while 
            self.current().is_alphanumeric() || 
            self.current() == '_'
        {
            text.push(self.current());
            self.cursor += 1;
        }

        while self.current() == '\'' {
            text.push(self.current());
            self.cursor += 1;
        }
        Ok(match text.as_str() {
            "fun"    => Kfun, 
            "let"    => Klet, 
            "struct" => Kstruct, 
            "enum"   => Kenum, 
            "return" => Kreturn,
            "if"     => Kif,
            "else"   => Kelse,
            "true"   => Ktrue,
            "false"  => Kfalse,
            "typeof" => Ktypeof,
            _        => Symbol(text)
        })
    }
    
    fn number(&mut self) -> LexResult {
        let mut text = String::new();
        while 
            self.current().is_digit(10) || 
            self.current() == '_' 
        {
            if self.current() != '_' {
                text.push(self.current());
            }
            self.cursor += 1;
        }

        if self.current() == '.' {
            text.push(self.current());
            self.cursor += 1;

            if !self.current().is_digit(10) {
                return Err(format!("Expected number sequence after dot but got '{}'", self.current()))
            }

            while 
                self.current().is_digit(10) || 
                self.current() == '_' 
            {
                if self.current() != '_' {
                    text.push(self.current());
                }
                self.cursor += 1;
            }   
            
            // TODO: handle parse failures 
            return Ok(Token::Float(text.parse().unwrap()))
        }

        // TODO: handle parse failures
        Ok(Token::Uinteger(text.parse().unwrap()))
    }

    fn string(&mut self) -> LexResult {
        // TODO: handle escape sequences
        let mut text = String::new();
        
        self.cursor += 1;
        while 
            // TODO: strings are multiline, 
            // maybe we want to have different literals for 
            // multi and single line strings  
            self.current() != '\0' && 
            self.current() != '\"' 
        {
            text.push(self.current());
            self.cursor += 1;
        }

        if self.current() != '\"' {
            return Err("Unterminated string".into())
        } else {
            self.cursor += 1;
        }
        Ok(Token::String(text))
    }

    fn token(&mut self) -> LexResult {
        use Token::*;
        
        let ch = self.current();
        
        if ch.is_alphabetic() {
            return self.symbol();
        }
        
        if ch.is_digit(10) {
            return self.number();
        } 
        
        if ch == '"' {
            return self.string();
        }
        
        self.cursor += 1;
        match ch {
            '='  => Ok(Equal),
            '('  => Ok(Lparen),
            ')'  => Ok(Rparen),
            '+'  => Ok(Plus),
            '*'  => Ok(Star),
            ','  => Ok(Comma),
            ';'  => Ok(Semicolon),
            '{'  => Ok(Lcurly),
            '}'  => Ok(Rcurly),
            '.'  => Ok(Point),
            '<'  => Ok(Langle),
            '>'  => Ok(Rangle),
            ' '  => Ok(Space),
            '/'  => if self.matchch('/') { 
                while self.current() != '\n' && self.current() != '\0' { self.cursor += 1 }
                Ok(Comment) 
            } else { 
                Ok(Slash) 
            },
            '-'  => if self.matchch('>') { Ok(Arrow) } else { Ok(Minus) },
            ':'  => if self.matchch(':') { Ok(Dcolon) } else { Ok(Colon) }
            '\r' => Ok(Creturn),
            '\t' => Ok(Tab),
            '\n' => Ok(Newline),
            '\0' => Ok(End),
            _    => Err(format!("Unknown character '{}'", ch)) 
        }
    }
    
    // TODO: fix name inconsistency
    pub fn lex(&mut self) -> Result<Vec<Token>, LexError> {
        use Token::*;

        let mut tokens = vec![];
        
        while self.cursor < self.chars.len() {
            let token = self.token()?;
            if let Space | Creturn | Tab | Newline | Comment = token {
                continue;
            }
            tokens.push(token);
        }
        Ok(tokens)
    }
}