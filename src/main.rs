mod scanner;
mod parser;

use scanner::{scan_clean, Token};
use parser::{Tree};

fn main() {
    //let source = String::from("var hi : int := 42;");
    //let source=String::from("for i in 1..12 do\nvar j : int := 42;\nend for;");
    //let source = String::from("var hi : int;");
    //let source = String::from("for i in 1..12 do\nfor j in 1..12 do\nvar i : int := 42;\nend for;\nend for;");
    let source = String::from("i := 42;");

    /*
    Use scanner to find tokens. Check if scanner
    found unrecognized tokens and if so report
    errors and exit program early.
     */
    let mut tokens = scan_clean(&source);
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
        println!("{} unrecognized token(s) found during scanning phase!", scanner_errors);
        return;
    }

    /*
    Use the parser to create parse tree. Check if
    parser returned errors and if so print error
    messages and exit program early.
     */
    dbg!(tokens.clone());

    tokens.reverse();
    let tree = parser::parse(&mut tokens);

    dbg!(tree);
}
