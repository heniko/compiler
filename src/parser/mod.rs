use std::ops::Deref;
use crate::scanner::{Token};

#[derive(Debug, Eq, PartialEq)]
pub enum Tree {
    Error { value: Vec<Token>, message: String },
    Stmts { value: Vec<Tree> },
    Stmt { value: Vec<Tree> },
    Var,
    For,
    Print,
    Read,
    Id { value: String },
    Int,
    String,
    Bool,
    Expr { value: Box<Tree> },
    Number { value: i32 },
}

/// Creates parse tree for the input token vector.
pub fn parse(tokens: &mut Vec<Token>) -> Tree {
    /*
    Statements is basically a linked list
    so it can easily be implemented as a
    vector.

    <stmts> ::= <stmt><stmts>
     */
    Tree::Stmts {
        value: {
            let mut stmts: Vec<Tree> = Vec::new();

            while let Some(token) = tokens.last() {
                stmts.push(match token {
                    Token::Keyword { value } => { parse_keyword(tokens) }
                    _ => { parse_error(tokens, String::from("Unknown starting token for a statement."), Vec::new()) }
                });

                tokens.pop();
            }

            stmts
        }
    }
}

/// Find the next line ending and return all tokens (excluding the semicolon since we let parse() pop it) as error.
pub fn parse_error(tokens: &mut Vec<Token>, message: String, mut error_tokens: Vec<Token>) -> Tree {
    Tree::Error {
        message,
        value: {
            while let Some(token) = tokens.last() {
                if token == &Token::Semicolon {
                    break;
                }
                error_tokens.push(tokens.pop().unwrap());
            }

            error_tokens
        },
    }
}

/// Parser for statement that starts with a keyword.
/// Will return either Token::Statement or Token::Error.
pub fn parse_keyword(tokens: &mut Vec<Token>) -> Tree {
    if let Some(token) = tokens.last() {
        if let Token::Keyword { value } = token {
            match value.as_str() {
                "var" => {
                    parse_var(tokens)
                }
                "for" => {
                    parse_for(tokens)
                }
                "read" => {
                    parse_read(tokens)
                }
                "print" => {
                    parse_print(tokens)
                }
                "assert" => {
                    parse_assert(tokens)
                }
                _ => {
                    parse_error(
                        tokens,
                        String::from("Starting statement with keyword that cannot start statement."),
                        Vec::new())
                }
            }
        } else {
            /*
            This branch will only be reached if parse_keyword()
            is called with a list containing some other token
            than Token::Keyword so in normal scenarios this
            should never be reached.
             */
            panic!("parse_keyword() was called with other token than Token::Keyword!");
        }
    } else {
        /*
        This else branch will only be executed when
        parse_keyword() is called with an empty input
        vector which should normally not happen since
        parse() will only call this method when current
        token is a keyword.
         */
        panic!("parse_keyword() was called with an empty input vector!");
    }
}

/// Parser for statement starting with keyword 'var'
pub fn parse_var(tokens: &mut Vec<Token>) -> Tree {
    let mut err: Vec<Token> = Vec::new();
    // We know this is Tree::Var since this fn was called
    let mut stmt: Vec<Tree> = vec![Tree::Var];
    err.push(tokens.pop().unwrap());

    // Check that next token is Variable
    if let Some(Token::Variable { value }) = tokens.last() {
        stmt.push(Tree::Id { value: String::from(value) });
        err.push(tokens.pop().unwrap());
    } else {
        return parse_error(tokens, String::from("Error while parsing variable: Expected variable identifier."), err);
    }

    // Check that next token is Colon
    if let Some(Token::Colon) = tokens.last() {
        err.push(tokens.pop().unwrap());
    } else {
        return parse_error(tokens, String::from("Error while parsing variable: Expected colon."), err);
    }

    // Check that next token is Keyword of one of the types int, string or bool
    if let Some(Token::Keyword { value }) = tokens.last() {
        match value.as_str() {
            "int" => { stmt.push(Tree::Int) }
            "string" => { stmt.push(Tree::String) }
            "bool" => { stmt.push(Tree::Bool) }
            _ => { return parse_error(tokens, String::from("Error while parsing variable: Expected type keyword."), err); }
        };
        err.push(tokens.pop().unwrap());
    } else {
        return parse_error(tokens, String::from("Error while parsing variable: Expected keyword."), err);
    }

    // Check that next token is Assign
    if let Some(Token::Assign) = tokens.last() {
        err.push(tokens.pop().unwrap());
    } else {
        return parse_error(tokens, String::from("Error while parsing variable: Expected assign."), err);
    }

    // Parse following expression
    stmt.push(parse_expr(tokens));

    Tree::Stmt {
        value: stmt
    }
}

/// Parser for statement starting with keyword 'for'
pub fn parse_for(tokens: &mut Vec<Token>) -> Tree {
    Tree::Stmt {
        value: Vec::new()
    }
}

/// Parser for statement starting with keyword 'read'
pub fn parse_read(tokens: &mut Vec<Token>) -> Tree {

    Tree::Stmt {
        value: vec![Tree::Read, parse_expr(tokens)]
    }
}

/// Parser for statement starting with keyword 'print'
pub fn parse_print(tokens: &mut Vec<Token>) -> Tree {
    /*
    This one is pretty simple since if this fn
    was called we already know the keyword is print
    so we just need to pass tokens for expression
    parser.
     */
    Tree::Stmt {
        value: vec![Tree::Print, parse_expr(tokens)]
    }
}

/// Parser for statement starting with keyword 'assert'
pub fn parse_assert(tokens: &mut Vec<Token>) -> Tree {
    Tree::Stmt {
        value: Vec::new()
    }
}

pub fn parse_expr(tokens: &mut Vec<Token>) -> Tree {
    /*
    For now only scan for one number and expect it to be the end.
     */
    let mut res = if let Some(Token::Number { value }) = tokens.last() {
        Tree::Expr {
            value: Box::new(Tree::Number {
                value: value.clone()
            })
        }
    } else {
        parse_error(tokens, String::from("Error while parsing expression."), Vec::new())
    };

    tokens.pop();
    res
}