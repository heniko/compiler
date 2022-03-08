mod scanner;
mod parser;

use scanner::{Scanner, Token};
use parser::{Tree, Parser};

fn main() {
    //let source = String::from("var hi : int := 42;");
    //let source=String::from("for i in 1..12 do\nvar j : int := 42;\nend for;");
    let source = String::from("var hi wrong : int;");
    //let source = String::from("for i in 1..12 do\nfor in 1..12 do\nvar i : int := 42;\nend for;\nend for;");
    //let source = String::from("var i:bool:=1+(1+1)*2;");

    /*
    Use scanner to find tokens. Check if scanner
    found unrecognized tokens and if so report
    errors and exit program early.
     */
    let mut scanner = Scanner::from(&source);
    scanner.scan();
    let mut tokens = scanner.tokens;
    let mut positions = scanner.positions;
    let mut scanner_errors = 0;

    //dbg!(scanner.positions);

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
        println!("{} unrecognized token(s) found during scanning phase!", scanner_errors);
        return;
    }

    /*
    Use the parser to create parse tree. Check if
    parser returned errors and if so print error
    messages and exit program early.
     */
    //dbg!(tokens.clone());

    let parser = Parser::from(&tokens, &positions);

    dbg!(parser.errors);
    dbg!(parser.ast);
}
