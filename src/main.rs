mod io;
mod parser;
mod scanner;
mod semantic_analysis;

use crate::io::{read_file, UserIO};
use crate::semantic_analysis::SemanticAnalyzer;
use parser::Parser;
use scanner::{Scanner, Token};

fn main() {
    let mut io = UserIO::from();
    println!("Source code location:");
    //let path = io.read();
    let path = String::from("programs/swap_and_sum_them.minipl");
    let source = read_file(path.as_str());

    dbg!(source.clone());
    /*
    Use scanner to find tokens. Check if scanner
    found unrecognized tokens and if so report
    errors and exit program early.
     */
    let mut scanner = Scanner::from(&source);
    scanner.scan();
    dbg!(scanner.tokens.clone());
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

    dbg!(parser.ast.clone());

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

    /*
    Validate the parse tree using semantic analysis.
     */
    let semantic_analyzer = SemanticAnalyzer::from(parser.ast.clone());

    if !semantic_analyzer.errors.is_empty() {
        println!(
            "Found {} semantic error(s):",
            semantic_analyzer.errors.len()
        );

        for error in semantic_analyzer.errors.iter() {
            println!("{}", error);
        }
        return;
    }
}
