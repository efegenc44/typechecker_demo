use std::{io::{stdout, stdin, Write}, env, fs};

mod token;
mod lexer;
mod ast;
mod parser;
// TODO: change the name of the file
mod r#type;

// TODO: Reconsider overloading
// TODO: Think about closures and first-class functions
// TODO: syntax

// TODO: Turn scope from linked list to Vec
//       for performance and sanity reasons  

/// ROADMAP
///     [x] Self refering structs
///     [x] Polymorphism
///         [x] Polymorphic Structs
///         [x] Polymorphic Functions
///         [ ] Type Checking for Polymorphic Functions
///             Collect possible types
///     [ ] using (or some other similar) keyword
///     [ ] sum types 
///     [ ] switch statement (exhaustive checking)
///     [ ] dynamic type | essentialy gradual typing
///     [ ] inference for polymorphic functions
///     [ ] Pipe op | uniform function calling notation | methods
///         one of them (maybe always keep pipe op) 
///     [ ] Finish remaining parts
///     [ ] étiquettes (compiler directives)

fn repl() {
   
    let mut tc = r#type::Typechecker::new();
    loop {
        let mut buf = String::new();
        print!("> "); 
        let _ = stdout().flush();
        let _ = stdin().read_line(&mut buf).unwrap();
        buf = buf.trim_end().to_string();
        if buf == ".quit" {
            break
        } 
        
        let mut lexer = lexer::Lexer::new(buf);
        let tokens = lexer.lex().unwrap();
        // println!("{:?}", tokens);
        
        let mut parser = parser::Parser::new(tokens);
        let ast = parser.parse().unwrap();
        // println!("{}", ast);
        
        match tc.typecheck(ast) {
            Ok(_) => {},
            Err(e) => println!("{e}"),
        }
    }
}

fn from_file(path: String) {
    let source = fs::read_to_string(path.clone())
        .expect("\n  Error while reading the file\n");     
    
    let mut lexer = lexer::Lexer::new(source);
    let tokens = lexer.lex().unwrap();

    let mut parser = parser::Parser::new(tokens);
    let ast = parser.parse().unwrap(); 
    // println!("{}", ast);

    let mut tc = r#type::Typechecker::new();
    match tc.typecheck(ast) {
        Ok(_) => {},
        Err(e) => println!("{e}"),
    }
}
fn main() {
    let args: Vec<String> = env::args().collect(); 
    let len = args.len();
    if len == 1 {
        repl();
    } 
    else if len == 2 {
        from_file(args[1].clone());
    } 
    else {
        println!("\n  Usage: ./belki <file-path>");
    }
}
