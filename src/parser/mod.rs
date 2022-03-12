mod expr_parser;

use crate::scanner::{Token, TokenPosition};
use std::collections::VecDeque;
use expr_parser::Expression;

#[cfg(test)]
mod tests;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum VarType {
    Int,
    String,
    Bool,
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
pub enum Tree {
    Error,
    Statements { value: Vec<Tree> },
    For { var: String, start: Box<Tree>, end: Box<Tree>, statements: Box<Tree> },
    Print { value: Box<Tree> },
    Read { var: String },
    Assert { value: Box<Tree> },
    Var { name: String, var_type: VarType, value: Box<Tree> },
    Assign { var: String, value: Box<Tree> },
    Expr { value: Expr },
    End,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ParserError {
    pub position: Option<TokenPosition>,
    pub message: String,
}

impl ParserError {
    pub fn from(position: Option<TokenPosition>, message: String) -> ParserError {
        ParserError {
            position,
            message,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Parser {
    tokens: VecDeque<Token>,
    positions: VecDeque<TokenPosition>,
    pub errors: Vec<ParserError>,
    pub ast: Tree,
}

impl Parser {
    pub fn from(tokens: &Vec<Token>, positions: &Vec<TokenPosition>) -> Parser {
        let mut parser = Parser {
            tokens: VecDeque::from(tokens.clone()),
            positions: VecDeque::from(positions.clone()),
            errors: Vec::new(),
            ast: Tree::End, // Placeholder
        };
        parser.ast = parser.parse();

        parser
    }

    fn pop(&mut self) -> (Option<Token>, Option<TokenPosition>) {
        (self.tokens.pop_front(), self.positions.pop_front())
    }

    fn peek(&mut self) -> (Option<&Token>, Option<&TokenPosition>) {
        (self.tokens.front(), self.positions.front())
    }

    fn parse(&mut self) -> Tree {
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

                while let (Some(token), Some(position)) = self.peek() {
                    stmts.push(match token {
                        Token::Variable { value: _ } => { self.parse_assign() }
                        Token::Var => { self.parse_var() }
                        Token::For => { self.parse_for() }
                        Token::Read => { self.parse_read() }
                        Token::Print => { self.parse_print() }
                        Token::Assert => { self.parse_assert() }
                        _ => {
                            let err = self.parse_error(
                                String::from("Unrecognized starting token for a statement.")
                            );
                            self.errors.push(err);
                            Tree::Error
                        }
                    });

                    self.pop();
                }

                stmts
            }
        }
    }

    fn parse_loop_statements(&mut self) -> Box<Tree> {
        /*
        Similar to parse() but has added functionality for ending loops.
         */
        let mut stmts: Vec<Tree> = Vec::new();

        while let (Some(token), Some(position)) = self.peek() {
            stmts.push(match token {
                Token::Variable { value: _ } => { self.parse_assign() }
                Token::Var => { self.parse_var() }
                Token::For => { self.parse_for() }
                Token::Read => { self.parse_read() }
                Token::Print => { self.parse_print() }
                Token::Assert => { self.parse_assert() }
                Token::End => { self.parse_end() }
                _ => {
                    let err = self.parse_error(
                        String::from("Unrecognized starting token for a statement."
                        )
                    );
                    self.errors.push(err);
                    Tree::Error
                }
            });
            if let Some(Tree::End) = stmts.last() {
                break;
            }
            self.pop();
        }

        if let Some(Tree::End) = stmts.last() {
            stmts.pop(); // Pop Tree::End since it has no actual use
        } else {
            self.errors.push(
                ParserError::from(
                    None,
                    String::from("Loop was not closed before EOF."),
                )
            );
        }

        Box::from(Tree::Statements {
            value: stmts
        })
    }

    fn parse_error(&mut self, message: String) -> ParserError {
        let error;

        if let (Some(token), Some(position)) = self.peek() {
            error = ParserError {
                position: Some(position.clone()),
                message,
            }
        } else {
            error = ParserError {
                position: None,
                message,
            }
        }

        while let (Some(token), Some(position)) = self.peek() {
            if token == &Token::Semicolon {
                break;
            }
            self.pop();
        }

        error
    }

    fn parse_assign(&mut self) -> Tree {
        /*
        <statement> ::= <var_id> ":=" <expression> ";"
         */
        let var_name;
        if let (Some(Token::Variable { value }), Some(position)) = self.peek() {
            var_name = value.clone();
            self.pop();
        } else {
            /*
            This should never happen since this function
            should never be called when there is no
            variable to be parsed.
             */
            return Tree::Error;
        }

        if let (Some(Token::Assign), Some(position)) = self.peek() {
            self.pop();
        } else {
            let err = self.parse_error(
                String::from("Expected assign.")
            );
            self.errors.push(err);
            return Tree::Error;
        }

        Tree::Assign {
            var: var_name,
            value: Box::from(self.parse_expr()),
        }
    }

    fn parse_var(&mut self) -> Tree {
        /*
        <statement> ::= "var" <var_id> ":" <var_type> ";"
        <statement> ::= "var" <var_id> ":" <var_type> ":=" <expression> ";"
         */
        self.pop(); // pop 'var'

        // Get the variable name
        let var_name: String;

        if let (Some(Token::Variable { value }), Some(position)) = self.peek() {
            var_name = value.clone();
            self.pop();
        } else {
            let err = self.parse_error(
                String::from("Expected variable identifier.")
            );
            self.errors.push(err);
            return Tree::Error;
        }

        // Check that next token is Colon
        if let (Some(Token::Colon), Some(position)) = self.peek() {
            self.pop();
        } else {
            let err = self.parse_error(
                String::from("Expected colon.")
            );
            self.errors.push(err);
            return Tree::Error;
        }

        // Check that next token is Keyword of one of the types int, string or bool
        let var_type: VarType;

        if let (Some(token), Some(position)) = self.peek() {
            match token {
                Token::Int => { var_type = VarType::Int; }
                Token::String => { var_type = VarType::String; }
                Token::Bool => { var_type = VarType::Bool }
                _ => {
                    let err = self.parse_error(
                        String::from("Expected keyword 'int', 'string' or 'bool'.")
                    );
                    self.errors.push(err);
                    return Tree::Error;
                }
            };
            self.pop();
        } else {
            let err = self.parse_error(
                String::from("Expected keyword 'int', 'string' or 'bool'.")
            );
            self.errors.push(err);
            return Tree::Error;
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
        let initial: Tree;

        if let (Some(token), Some(position)) = self.peek() {
            match token {
                Token::Assign => {
                    self.pop();
                    initial = self.parse_expr();
                }
                Token::Semicolon => {
                    match var_type {
                        VarType::Int => { initial = Tree::Expr { value: Expr::Number { value: 0 } }; }
                        VarType::String => { initial = Tree::Expr { value: Expr::String { value: String::from("") } }; }
                        VarType::Bool => { initial = Tree::Expr { value: Expr::Bool { value: false } }; }
                    }
                }
                _ => {
                    let err = self.parse_error(
                        String::from("Unexpected token.")
                    );
                    self.errors.push(err);
                    return Tree::Error;
                }
            }
        } else {
            let err = self.parse_error(
                String::from("Unexpected EOF.")
            );
            self.errors.push(err);
            return Tree::Error;
        }

        Tree::Var {
            name: var_name,
            var_type,
            value: Box::from(initial),
        }
    }

    fn parse_for(&mut self) -> Tree {
        /*
        <statement> ::= "for" <var_id> "in" <expression> ".." <expression> "do" <statements> <end_for>
         */
        self.pop(); // pop 'for'

        // Expect identifier
        let var_name: String;
        if let (Some(Token::Variable { value }), Some(position)) = self.peek() {
            var_name = value.clone();
            self.pop();
        } else {
            let err = self.parse_error(
                String::from("Expected variable identifier.")
            );
            self.errors.push(err);
            return Tree::Error;
        }

        // Expect keyword 'in'
        if let (Some(Token::In), Some(position)) = self.peek() {
            self.pop();
        } else {
            let err = self.parse_error(
                String::from("Expected keyword 'in'.")
            );
            self.errors.push(err);
            return Tree::Error;
        }

        // Expect expression
        let start = Box::from(self.parse_expr());

        // Expect dots
        if let (Some(Token::Dots), Some(position)) = self.peek() {
            self.pop();
        } else {
            let err = self.parse_error(
                String::from("Expected '..'.")
            );
            self.errors.push(err);
            return Tree::Error;
        }

        // Expect Expression
        let end = Box::from(self.parse_expr());

        // Expect keyword 'do'
        if let (Some(Token::Do), Some(position)) = self.peek() {
            self.pop();
        } else {
            let err = self.parse_error(
                String::from("Expected keyword 'do'.")
            );
            self.errors.push(err);
            return Tree::Error;
        }

        Tree::For {
            var: var_name,
            start,
            end,
            statements: self.parse_loop_statements(),
        }
    }

    fn parse_read(&mut self) -> Tree {
        /*
        <statement> ::= "read" <var_id> ";"
         */
        self.pop(); // pop 'read'

        // Check that the next token is variable
        let var_name: String;

        if let (Some(Token::Variable { value }), Some(position)) = self.peek() {
            var_name = value.clone();
            self.pop();
        } else {
            let err = self.parse_error(
                String::from("Expected variable identifier.")
            );
            self.errors.push(err);
            return Tree::Error;
        }

        if let (Some(Token::Semicolon), Some(position)) = self.peek() {
            /* Do nothing as the statement is complete */
        } else {
            let err = self.parse_error(
                String::from("Expected ';'.")
            );
            self.errors.push(err);
            return Tree::Error;
        }

        Tree::Read {
            var: var_name
        }
    }

    fn parse_print(&mut self) -> Tree {
        /*
        <statement> ::= "print" <expression> ";"
         */
        self.pop(); // pop 'print'
        Tree::Print {
            value: Box::from(self.parse_expr())
        }
    }

    fn parse_assert(&mut self) -> Tree {
        /*
        <statement> ::= "assert" <expression> ";"
         */
        self.pop(); // pop 'assert'
        Tree::Assert {
            value: Box::from(self.parse_expr())
        }
    }

    fn parse_end(&mut self) -> Tree {
        /*
        <end_for> ::= "end" "for" ";"
         */
        self.pop(); // pop 'end'

        // Check that next token is Keyword 'for'
        if let (Some(Token::For), Some(position)) = self.peek() {
            self.pop();
        } else {
            let err = self.parse_error(
                String::from("Expected keyword 'for'.")
            );
            self.errors.push(err);
            return Tree::Error;
        }

        // Check that next token is Semicolon
        return if let (Some(Token::Semicolon), Some(position)) = self.peek() {
            Tree::End
        } else {
            let err = self.parse_error(
                String::from("Expected ';'.")
            );
            self.errors.push(err);
            return Tree::Error;
        };
    }

    fn parse_expr(&mut self) -> Tree {
        /*
        Starting point for parsing expressions.
         */
        let mut expr = Vec::new();
        while let (Some(token), Some(position)) = self.peek() {
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
            self.pop();
        };

        Tree::Expr { value: Expression::from(&expr).expression() }
    }
}