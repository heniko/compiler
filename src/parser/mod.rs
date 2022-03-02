use crate::scanner::{Token};
use crate::scanner::Token::StringLiteral;

#[cfg(test)]
mod tests;

#[derive(Debug, Eq, PartialEq)]
pub enum VarType {
    Int,
    String,
    Bool,
    Unknown,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Tree {
    Error { tokens: Vec<Token>, message: String },
    Statements { value: Vec<Tree> },
    For { var: String, start: Box<Tree>, end: Box<Tree>, statements: Box<Tree> },
    Print { value: Box<Tree> },
    Read { var: String },
    Assert { value: Box<Tree> },
    Var { name: String, var_type: VarType, value: Box<Tree> },
    Expr { value: Box<Tree> },
    Number { value: i32 },
    Bool { value: bool },
    String { value: String },
    Assign { var: String, value: Box<Tree> },
    End,
}

/// Creates parse tree for the input token vector.
pub fn parse(tokens: &mut Vec<Token>) -> Tree {
    /*
    Statements is basically a linked list
    so it can easily be implemented as a
    vector.

    <stmts> ::= <stmt><stmts>
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

/// Similar to parse but returns the statements on 'end for;'.
/// This function aims to make nested loops easy to implement.
pub fn parse_loop_statements(tokens: &mut Vec<Token>) -> Box<Tree> {
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
        /* Do nothing as loop was ended correctly */
    } else {
        stmts.push(Tree::Error { tokens: Vec::new(), message: String::from("EOF before closing for loop.") })
    }

    Box::from(Tree::Statements {
        value: stmts
    })
}

/// Find the next line ending and return all tokens (excluding the semicolon since we let parse() pop it) as error.
pub fn parse_error(tokens: &mut Vec<Token>, message: String, mut error_tokens: Vec<Token>) -> Tree {
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

/// Parser for '<identifier> := <expression>;'
pub fn parse_assign(tokens: &mut Vec<Token>) -> Tree {
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

/// Parser for statement starting with keyword 'var'
pub fn parse_var(tokens: &mut Vec<Token>) -> Tree {
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
                match var_type {
                    VarType::Int => { initial = parse_expr(tokens) }
                    VarType::String => {}
                    VarType::Bool => {}
                    VarType::Unknown => {
                        // Should not be possible!
                        panic!("Unknown VarType")
                    }
                }
            }
            Token::Semicolon => {
                match var_type {
                    VarType::Int => { initial = Tree::Expr { value: Box::from(Tree::Number { value: 0 }) }; }
                    VarType::String => { initial = Tree::Expr { value: Box::from(Tree::String { value: String::new() }) }; }
                    VarType::Bool => { initial = Tree::Expr { value: Box::from(Tree::Bool { value: false }) }; }
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

/// Parser for statement starting with keyword 'for'
pub fn parse_for(tokens: &mut Vec<Token>) -> Tree {
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

/// Parser for statement starting with keyword 'read'
pub fn parse_read(tokens: &mut Vec<Token>) -> Tree {
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

/// Parser for statement starting with keyword 'print'
pub fn parse_print(tokens: &mut Vec<Token>) -> Tree {
    /*
    This one is pretty simple since if this fn
    was called we already know the keyword is print
    so we just need to pass tokens for expression
    parser.
     */
    tokens.pop(); // pop() print token
    Tree::Print {
        value: Box::from(parse_expr(tokens))
    }
}

/// Parser for statement starting with keyword 'assert'
pub fn parse_assert(tokens: &mut Vec<Token>) -> Tree {
    tokens.pop();
    Tree::Assert {
        value: Box::from(parse_expr(tokens))
    }
}

/// Parser for for end statement
pub fn parse_end(tokens: &mut Vec<Token>) -> Tree {
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

/// Parser for expression
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