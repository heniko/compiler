mod interpreter;
mod io;
mod parser;
mod scanner;

use crate::io::{read_file, UserIO, IO};
use interpreter::Interpreter;
use parser::{Parser};
use scanner::{Scanner, Token};

fn main() {
    let mut io = UserIO::from();
    println!("Source code location:");
    let path = io.read();
    let source = read_file(path.as_str());

    /*
    Use scanner to find tokens. Check if scanner
    found unrecognized tokens and if so report
    errors and exit program early.
     */
    let mut scanner = Scanner::from(&source);
    scanner.scan();
    //dbg!(scanner.tokens.clone());
    let tokens = scanner.tokens;
    let positions = scanner.positions;
    let mut scanner_errors = 0;

    for token in tokens.iter() {
        match token {
            Token::Unknown { value } => {
                scanner_errors += 1;
                println!("{}", value);
            }
            _ => { /* Do nothing */ }
        }
    }

    if scanner_errors > 0 {
        println!(
            "{} unrecognized token(s) found during scanning phase!",
            scanner_errors
        );
        return;
    }

    /*
    Use the parser to create parse tree. Check if
    parser returned errors and if so print error
    messages and exit program early.
     */
    let parser = Parser::from(&tokens, &positions);

    //dbg!(parser.ast.clone());

    if !parser.errors.is_empty() {
        for e in parser.errors.iter() {
            match &e.position {
                Some(p) => {
                    println!("Error in line {} char {}: {}", p.line, p.char, e.message);
                }
                None => {
                    println!("Error in file: {}", e.message);
                }
            }
        }
        return;
    }

    let mut interpreter = Interpreter::from(parser.ast, Box::from(UserIO::from()));

    interpreter.run();
}
