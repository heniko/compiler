mod expr_parser;

use crate::scanner::{Token};
use std::collections::VecDeque;
use expr_parser::Expression;

#[cfg(test)]
mod tests;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum VarType {
    Int,
    String,
    Bool,
    Unknown,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Expr {
    Mul,
    Div,
    Plus,
    Minus,
    Not,
    And,
    OpenParen,
    CloseParen,
    Less,
    Eq,
    Group { value: Box<Expr> },
    Binary { op: Box<Expr>, left: Box<Expr>, right: Box<Expr> },
    Unary { op: Box<Expr>, value: Box<Expr> },
    Number { value: i32 },
    String { value: String },
    Bool { value: bool },
    Variable { value: String },
    Error,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Op {
    Plus,
    Minus,
    Div,
    Mul,
    Not,
    Less,
    Eq,
    And,
    Unknown,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Tree {
    Error { tokens: Vec<Token>, message: String },
    Statements { value: Vec<Tree> },
    For { var: String, start: Box<Tree>, end: Box<Tree>, statements: Box<Tree> },
    Print { value: Box<Tree> },
    Read { var: String },
    Assert { value: Box<Tree> },
    Var { name: String, var_type: VarType, value: Box<Tree> },
    Number { value: i32 },
    Bool { value: bool },
    String { value: String },
    Assign { var: String, value: Box<Tree> },
    Expr { value: Expr },
    End,
}

/// Creates the AST for given vector of tokens.
pub fn parse(tokens: &mut Vec<Token>) -> Tree {
    /*
    <statements> ::= <statement><statements> // Implemented as vector
    <statements> ::= <statement>

    <statement> ::= "var" <var_id> ":" <var_type> ";"
    <statement> ::= "var" <var_id> ":" <var_type> ":=" <expression> ";"
    <statement> ::= "for" <var_id> "in" <expression> ".." <expression> "do" <statements> <end_for>
    <statement> ::= "read" <var_id> ";"
    <statement> ::= "print" <expression> ";"
    <statement> ::= "assert" <expression> ";"
    <statement> ::= <var_id> ":=" <expression> ";"

    <var_type> ::= "int" | "bool" | "string"

    <end_for> ::= "end" "for" ";"

    <var_id> ::= <id>

    <expression> ::= <and>

    <and> ::= <and> "&" <equality>
    <and> ::= <equality> "&" <equality>
    <and> ::= <equality>

    <equality> ::= <equality> "=" <comparison>
    <equality> ::= <comparison> "=" <comparison>
    <equality> ::= <comparison>

    <comparison> ::= <comparison> ">" <term>
    <comparison> ::= <term> ">" <term>
    <comparison> ::= <term>

    <term> ::= <term> "+" | "-"
    <term> ::= <factor> "+" | "-" <factor>
    <term> ::= <factor>

    <factor> ::= <factor> "*" | "/" <unary>
    <factor> ::= <unary> "*" | "/" <unary>
    <factor> ::= <unary>

    <unary> ::= "" | "-" | "!" <primary>
    <unary> ::= <primary>

    <primary> ::= <string> | <number> | <variable> | "(" <expression> ")"
     */
    Tree::Statements {
        value: {
            let mut stmts: Vec<Tree> = Vec::new();

            while let Some(token) = tokens.last() {
                stmts.push(match token {
                    Token::Variable { value } => { parse_assign(tokens) }
                    Token::Var => { parse_var(tokens) }
                    Token::For => { parse_for(tokens) }
                    Token::Read => { parse_read(tokens) }
                    Token::Print => { parse_print(tokens) }
                    Token::Assert => { parse_assert(tokens) }
                    _ => { parse_error(tokens, String::from("Unknown starting token for a statement."), Vec::new()) }
                });

                tokens.pop();
            }

            stmts
        }
    }
}

fn parse_loop_statements(tokens: &mut Vec<Token>) -> Box<Tree> {
    /*
    Similar to parse() but has added functionality for ending loops.
     */
    let mut stmts: Vec<Tree> = Vec::new();

    while let Some(token) = tokens.last() {
        stmts.push(match token {
            Token::Variable { value } => { parse_assign(tokens) }
            Token::Var => { parse_var(tokens) }
            Token::For => { parse_for(tokens) }
            Token::Read => { parse_read(tokens) }
            Token::Print => { parse_print(tokens) }
            Token::Assert => { parse_assert(tokens) }
            Token::End => { parse_end(tokens) }
            _ => { parse_error(tokens, String::from("Unknown starting token for a statement."), Vec::new()) }
        });
        if let Some(Tree::End) = stmts.last() {
            break;
        }
        tokens.pop();
    }

    if let Some(Tree::End) = stmts.last() {
        stmts.pop(); // Pop Tree::End since it has no actual use
    } else {
        stmts.push(Tree::Error { tokens: Vec::new(), message: String::from("EOF before closing for loop.") })
    }

    Box::from(Tree::Statements {
        value: stmts
    })
}

fn parse_error(tokens: &mut Vec<Token>, message: String, mut error_tokens: Vec<Token>) -> Tree {
    Tree::Error {
        message,
        tokens: {
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

fn parse_assign(tokens: &mut Vec<Token>) -> Tree {
    /*
    <statement> ::= <var_id> ":=" <expression> ";"
     */
    let mut var_name = String::from("");
    if let Some(Token::Variable { value }) = tokens.last() {
        var_name = value.clone();
    }
    let mut err = vec![tokens.pop().unwrap()];

    if let Some(Token::Assign) = tokens.last() {
        tokens.pop();
    } else {
        return parse_error(tokens, String::from("Expected ':='."), err);
    }

    Tree::Assign {
        var: var_name,
        value: Box::from(parse_expr(tokens)),
    }
}

fn parse_var(tokens: &mut Vec<Token>) -> Tree {
    /*
    <statement> ::= "var" <var_id> ":" <var_type> ";"
    <statement> ::= "var" <var_id> ":" <var_type> ":=" <expression> ";"
     */
    let mut err: Vec<Token> = vec![tokens.pop().unwrap()];

    // Get the variable name
    let mut var_name = String::new();

    if let Some(Token::Variable { value }) = tokens.last() {
        var_name = value.clone();
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
    let mut var_type = VarType::Unknown;

    if let Some(token) = tokens.last() {
        match token {
            Token::Int => { var_type = VarType::Int; }
            Token::String => { var_type = VarType::String; }
            Token::Bool => { var_type = VarType::Bool }
            _ => { return parse_error(tokens, String::from("Error while parsing variable: Expected type keyword."), err); }
        };
        err.push(tokens.pop().unwrap());
    } else {
        return parse_error(tokens, String::from("Error while parsing variable: Expected keyword."), err);
    }

    /*
    Check that next token exists since we are either
    expecting Semicolon to end the line or Assign
    followed by expression to assign some value to
    the variable the programmer is initializing.
    If no value is provided, default initial value
    will be used instead:
    int    -> 0
    string -> ""
    bool   -> false
     */
    let mut initial = Tree::End;

    if let Some(token) = tokens.last() {
        match token {
            Token::Assign => {
                err.push(tokens.pop().unwrap());
                initial = parse_expr(tokens);
            }
            Token::Semicolon => {
                match var_type {
                    VarType::Int => { initial = Tree::Expr { value: Expr::Number { value: 0 } }; }
                    VarType::String => { initial = Tree::Expr { value: Expr::String { value: String::from("") } }; }
                    VarType::Bool => { initial = Tree::Expr { value: Expr::Bool { value: false } }; }
                    VarType::Unknown => {
                        // Should not be possible!
                        panic!("Unknown VarType")
                    }
                }
            }
            _ => {
                return parse_error(tokens, String::from("Error while parsing variable: Expected semicolon or assign."), err);
            }
        }
    } else {
        return Tree::Error {
            tokens: err,
            message: String::from("Unexpected EOF while parsing variable."),
        };
    }

    Tree::Var {
        name: var_name,
        var_type,
        value: Box::from(initial),
    }
}

fn parse_for(tokens: &mut Vec<Token>) -> Tree {
    /*
    <statement> ::= "for" <var_id> "in" <expression> ".." <expression> "do" <statements> <end_for>
     */
    let mut err = vec![tokens.pop().unwrap()];

    // Expect identifier
    let mut var_name = String::new();

    if let Some(Token::Variable { value }) = tokens.last() {
        var_name = value.clone();
        err.push(tokens.pop().unwrap());
    } else {
        return parse_error(tokens, String::from("Error while parsing for loop start: Expected identifier."), err);
    }

    // Expect keyword 'in'
    if let Some(Token::In) = tokens.last() {
        err.push(tokens.pop().unwrap());
    } else {
        return parse_error(tokens, String::from("Error while parsing for loop start: Expected keyword 'in'."), err);
    }

    // Expect expression
    let mut start = Box::from(parse_expr(tokens));

    // Expect dots
    if let Some(Token::Dots) = tokens.last() {
        err.push(tokens.pop().unwrap());
    } else {
        return parse_error(tokens, String::from("Error while parsing for loop start: Expected dots."), err);
    }

    // Expect Expression
    let mut end = Box::from(parse_expr(tokens));

    // Expect keyword 'do'
    if let Some(Token::Do) = tokens.last() {
        tokens.pop();
    } else {
        return parse_error(tokens, String::from("Error while parsing for loop start: Expected keyword 'do'."), err);
    }

    Tree::For {
        var: var_name,
        start,
        end,
        statements: parse_loop_statements(tokens),
    }
}

fn parse_read(tokens: &mut Vec<Token>) -> Tree {
    /*
    <statement> ::= "read" <var_id> ";"
     */
    let mut err = vec![tokens.pop().unwrap()];

    // Check that the next token is variable
    let mut var_name = String::new();

    if let Some(Token::Variable { value }) = tokens.last() {
        var_name = value.clone();
        err.push(tokens.pop().unwrap());
    } else {
        return parse_error(tokens, String::from("Error while parsing read statement: Expected identifier."), err);
    }

    if let Some(Token::Semicolon) = tokens.last() {
        /* Do nothing as the statement is complete */
    } else {
        return parse_error(tokens, String::from("Error while parsing read statement: Expected semicolon."), err);
    }

    Tree::Read {
        var: var_name
    }
}

fn parse_print(tokens: &mut Vec<Token>) -> Tree {
    /*
    <statement> ::= "print" <expression> ";"
     */
    tokens.pop(); // pop() print token
    Tree::Print {
        value: Box::from(parse_expr(tokens))
    }
}

fn parse_assert(tokens: &mut Vec<Token>) -> Tree {
    /*
    <statement> ::= "assert" <expression> ";"
     */
    tokens.pop();
    Tree::Assert {
        value: Box::from(parse_expr(tokens))
    }
}

fn parse_end(tokens: &mut Vec<Token>) -> Tree {
    /*
    <end_for> ::= "end" "for" ";"
     */
    let mut err = vec![tokens.pop().unwrap()];

    // Check that next token is Keyword 'for'
    if let Some(Token::For) = tokens.last() {
        err.push(tokens.pop().unwrap());
    } else {
        return parse_error(tokens, String::from("Error while parsing end statement: Expected keyword 'for'."), err);
    }

    // Check that next token is Semicolon
    return if let Some(Token::Semicolon) = tokens.last() {
        Tree::End
    } else {
        parse_error(tokens, String::from("Error while parsing end statement: Expected semicolon."), err)
    };
}

fn parse_expr(tokens: &mut Vec<Token>) -> Tree {
    /*
    Starting point for parsing expressions.
     */
    let mut expr = Vec::new();
    while let Some(token) = tokens.last() {
        match token {
            Token::Variable { value } => { expr.push(Expr::Variable { value: value.clone() }); }
            Token::Number { value } => { expr.push(Expr::Number { value: value.clone() }); }
            Token::StringLiteral { value } => { expr.push(Expr::String { value: value.clone() }); }
            Token::Minus => { expr.push(Expr::Minus); }
            Token::Plus => { expr.push(Expr::Plus); }
            Token::Divide => { expr.push(Expr::Div); }
            Token::Multiply => { expr.push(Expr::Mul); }
            Token::Not => { expr.push(Expr::Not); }
            Token::LessThan => { expr.push(Expr::Less); }
            Token::Equals => { expr.push(Expr::Eq); }
            Token::And => { expr.push(Expr::And); }
            Token::OpenParen => { expr.push(Expr::OpenParen); }
            Token::CloseParen => { expr.push(Expr::CloseParen); }
            _ => { break; }
        }
        tokens.pop();
    };

    Tree::Expr { value: Expression::from(&expr).expression() }
}