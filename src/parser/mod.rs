mod expr_parser;

use crate::scanner::{Token, TokenPosition};
use std::collections::VecDeque;
use std::string::ParseError;
//use expr_parser::Expression;

#[cfg(test)]
mod tests;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum VarType {
    Int,
    String,
    Bool,
    Real,
    Error,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Error,
    Assignment { id: String, value: Expression },
    Call { id: String, arguments: Vec<Expression> },
    Return { value: Expression },
    Read { ids: Vec<String> },
    Write { arguments: Vec<Expression> },
    Assert { value: Expression },
    Block { statements: Vec<Statement> },
    If { value: Expression, statement: Box<Statement> },
    IfElse { value: Expression, if_statement: Box<Statement>, else_statement: Box<Statement> },
    While { value: Expression, statement: Box<Statement> },
    VarDeclaration { ids: Vec<String>, var_type: VarType },
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Error,
    None,
    Eq,
    Le,
    Ge,
    Leq,
    Geq,
    Inequality,
    Plus,
    Minus,
    Or,
    Not,
    Multiply,
    Divide,
    Modulo,
    And,
    StringLiteral { value: String },
    IntegerLiteral { value: i32 },
    RealLiteral { value: f32 },
    True,
    False,
    Function { id: String, parameters: Vec<Expression> },
    Variable { id: String },
    OpenParen,
    CloseParen,
    Unary { op: Box<Expression>, value: Box<Expression> },
    Binary { op: Box<Expression>, left: Box<Expression>, right: Box<Expression> },
    Group { value: Box<Expression> },
}

#[derive(Debug, PartialEq, Clone)]
pub enum AST {
    Error,
    Program { id: String, functions: Vec<AST>, procedures: Vec<AST>, main: Statement },
    Procedure { parameters: Vec<AST>, block: Statement },
    Function { parameters: Vec<AST>, block: Box<AST>, res_type: VarType },
    Call { parameter: Expression },
    Parameters,
    Parameter,
    SimpleType,
    ArrayType,
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

#[derive(Debug, PartialEq, Clone)]
pub struct Parser {
    tokens: VecDeque<Token>,
    positions: VecDeque<TokenPosition>,
    pub errors: Vec<ParserError>,
    pub ast: AST,
}

impl Parser {
    pub fn from(tokens: &Vec<Token>, positions: &Vec<TokenPosition>) -> Parser {
        let mut parser = Parser {
            tokens: VecDeque::from(tokens.clone()),
            positions: VecDeque::from(positions.clone()),
            errors: Vec::new(),
            ast: AST::Error, // Placeholder
        };
        parser.ast = parser.program();

        parser
    }

    fn pop(&mut self) -> (Option<Token>, Option<TokenPosition>) {
        (self.tokens.pop_front(), self.positions.pop_front())
    }

    fn peek(&mut self) -> (Option<&Token>, Option<&TokenPosition>) {
        (self.tokens.front(), self.positions.front())
    }

    fn parse_error(&mut self, message: String) {
        /*
        Create error that contains the position of the
        error (or not if it's not easily available)
        and message of what went wrong. Find the point
        where it is expected to be easy to continue parsing
        or in other words next ';' since at that point
        we are pretty confident there is something we can
        try parsing.
         */
        let error;

        if let (Some(_token), Some(position)) = self.peek() {
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

        while let (Some(token), Some(_position)) = self.pop() {
            if token == Token::Semicolon {
                break;
            }
        }

        self.errors.push(error);
    }

    fn expect(&mut self, match_token: Token) -> bool {
        /*
        Checks that the next token is the expected token.
        If it is then consume it and if it isn't then
        handle error. Return true if next token is the expected
        one and false if it is not.
         */
        if let (Some(token), Some(position)) = self.peek() {
            if token.clone() == match_token {
                self.pop(); // Consume expected token
                return true;
            } else {
                let err_token = token.clone();
                self.parse_error(format!("Expected token: {:?}, found token: {:?}.", match_token.clone(), err_token));
                return false;
            }
        }

        self.parse_error(String::from("Expected token."));
        false
    }

    fn program(&mut self) -> AST {
        /*
        <Program> ::= "program" <Id> ";" { <Procedure> | <Function> } <MainBlock> "."
        <MainBlock> ::= <Block>
        */
        let id;
        let mut procedures = vec![];
        let mut functions = vec![];
        let main_block;

        if !self.expect(Token::Program) {
            return AST::Error;
        }

        // Get program identifier
        if let (Some(token), Some(position)) = self.peek() {
            match token {
                Token::Variable { value } => {
                    id = value.clone();
                    self.pop();
                }
                _ => {
                    self.parse_error(String::from("Expected identifier."));
                    return AST::Error;
                }
            }
        } else {
            self.parse_error(String::from("Expected identifier."));
            return AST::Error;
        }

        if !self.expect(Token::Semicolon) {
            return AST::Error;
        }

        /*
        Check if another function or procedure is added
        if yes then call correct handler and add the
        function/procedure to AST and if not move on
        and expect main block.
         */
        while let (Some(token), Some(position)) = self.peek() {
            match token {
                Token::Procedure => {
                    todo!();
                }
                Token::Function => {
                    todo!();
                }
                _ => {
                    break;
                }
            }
        }

        main_block = self.block();

        if !self.expect(Token::Dot) {
            // No expected dot at the end of the program
            AST::Error
        } else {
            // All is fine and we can return the program
            AST::Program {
                id,
                procedures,
                functions,
                main: main_block,
            }
        }
    }

    fn statement(&mut self) -> Statement {
        /*
        <Statement> ::= <AssignStatement> | <CallStatement> | <ReturnStatement>
                        <ReadStatement> | <WriteStatement> | <AssertStatement>
                        <Block> | <IfStatement> | <WhileStatement> | <VarDeclaration>
         */

        // Check what kind of statement we are starting
        // and call correct function to handle that case.
        return if let (Some(token), Some(position)) = self.peek() {
            match token {
                Token::Begin => {
                    dbg!("Parse begin");
                    self.block()
                }
                Token::While => {
                    dbg!("Parse while");
                    self.while_statement()
                }
                Token::Return => {
                    dbg!("parse return");
                    self.return_statement()
                }
                Token::If => {
                    dbg!("Parse if");
                    self.if_statement()
                }
                Token::Assert => {
                    dbg!("Parse assert");
                    self.assert_statement()
                }
                Token::Var => {
                    dbg!("Parse variable declaration");
                    self.var_declaration()
                }
                Token::Variable { value: _ } => {
                    dbg!("Parse assign or call");
                    self.call_or_assign()
                }
                _ => {
                    // This is where we end up if expected statement but found
                    // something else
                    self.parse_error(String::from("Expected statement."));
                    Statement::Error
                }
            }
        } else {
            // This is where we end up if we expected statement but found EOF
            self.parse_error(String::from("Expected statement."));
            Statement::Error
        };
    }

    fn block(&mut self) -> Statement {
        /*
        <Block> ::= "begin" { <Statement> } "end"

        Expect block to start with Token::Begin.
        We cannot be sure of this since sometimes
        like with main block this function is called
        directly instead of statement() calling it.
        */
        if !self.expect(Token::Begin) {
            return Statement::Error;
        }

        let mut statements = vec![];

        // Parse statements until we find one starting with Token::End.
        // Use statements() to reduce code duplication.
        while let (Some(token), Some(position)) = self.peek() {
            match token {
                Token::End => {
                    // Found end of the block
                    self.pop(); // pop Token::End
                    return Statement::Block { statements };
                }
                _ => {
                    // Try parsing another statement
                    statements.push(self.statement());
                }
            }
        }

        // EOF before Token::End
        self.expect(Token::End);
        return Statement::Error;
    }

    fn while_statement(&mut self) -> Statement {
        // <WhileStatement> ::= "while" <Boolean expression> "do" <Statement>
        if !self.expect(Token::While) {
            return Statement::Error;
        }

        let expression = self.expression();

        if !self.expect(Token::Do) {
            return Statement::Error;
        }

        let statement = self.statement();

        Statement::While {
            value: expression,
            statement: Box::from(statement),
        }
    }

    fn if_statement(&mut self) -> Statement {
        /*
        <IfStatement> ::= "if" <Boolean expression> "then" <Statement>
                        | "if" <Boolean expression> "then" <Statement> "else" <Statement>
         */
        if !self.expect(Token::If) {
            return Statement::Error;
        }

        let expression = self.expression();

        if !self.expect(Token::Then) {
            return Statement::Error;
        }

        let statement = self.statement();

        if let (Some(token), Some(_position)) = self.peek() {
            match token {
                Token::Else => {
                    self.pop(); // pop Token::Else
                    let else_statement = self.statement();
                    return Statement::IfElse {
                        value: expression,
                        if_statement: Box::from(statement),
                        else_statement: Box::from(else_statement),
                    };
                }
                _ => { /* Do nothing */ }
            }
        }

        Statement::If {
            value: expression,
            statement: Box::from(statement),
        }
    }

    fn assert_statement(&mut self) -> Statement {
        /*
        <AssertStatement> ::= "assert" "(" <Argument> ")" ";"

        This differs a bit from read and write statements
        since assert is not treated as a predefined identifier
        but is actually language keyword instead.
         */
        if !self.expect(Token::Assert) {
            return Statement::Error;
        }

        let mut arguments = self.arguments();

        if !self.expect(Token::Semicolon) {
            return Statement::Error;
        }

        /*
        Check that arguments only has one element since
        assert will always only take one expression as
        argument.
         */
        return if arguments.len() == 1 {
            Statement::Assert {
                value: arguments.pop().unwrap()
            }
        } else {
            self.errors.push(
                ParserError {
                    message: String::from("Assert can only take one argument."),
                    position: None,
                }
            );
            Statement::Error
        };
    }

    fn read_statement(&mut self) -> Statement {
        /*
        <ReadStatement> ::= "read" "(" <Id> { "," <Id> } ")" ";"

        TODO: This might be just a function that automatically
              gets added if the user doesn't declare function
              with the same name.
         */
        todo!();
    }

    fn write_statement(&mut self) -> Statement {
        /*
        <WriteStatement> ::= "writeln" "(" <Arguments> ")" ";"

        TODO: Might be just a normal function: See read_statement()
         */
        todo!();
    }

    fn var_declaration(&mut self) -> Statement {
        /*
        <VarDeclaration> ::= "var" <Id> { "," <Id> } ":" <Type> ";"
         */
        todo!();
    }

    fn call_or_assign(&mut self) -> Statement {
        /*
        Checks if the statement is a call or assign and
        calls the correct handler to parse the statement.
         */
        todo!();
    }

    fn call(&mut self) -> Statement {
        /*
        <CallStatement> ::= <Id> "(" <Arguments> ")" ";"
        TODO: Might break this into call() and call_statement()
              since that might be handy for parsing functions that
              are part of expressions and don't need to end in ';'.
         */
        todo!();
    }

    fn type_declaration(&mut self) {
        /*
        AssignStatement ::= <Id> ":=" <Expression> ";"
         */
        todo!();
    }

    fn return_statement(&mut self) -> Statement {
        /*
        <ReturnStatement> ::= "return" <Expression> ';'
         */
        if !self.expect(Token::Return) {
            return Statement::Error;
        }

        if let (Some(token), Some(position)) = self.peek() {
            if token.clone() == Token::Semicolon {
                // We have return statement without return value
                self.pop(); // Consume ';'
                Statement::Return { value: Expression::None }
            } else {
                // We have return statement with return value
                let expression = self.expression();
                // Make sure there is ';' at the end of the statement
                return if !self.expect(Token::Semicolon) {
                    Statement::Error
                } else {
                    Statement::Return {
                        value: expression
                    }
                };
            }
        } else {
            self.parse_error(String::from("Expected semicolon or expression."));
            return Statement::Error;
        }
    }

    fn parse_function(&mut self) -> AST {
        /*
        <Function> ::= "function" <Id> "(" <Parameters> ")" ";" <Block> ";"
         */
        todo!();
    }

    fn parse_procedure(&mut self) -> AST {
        /*
        <Procedure> ::= "procedure" <Id> "(" <Parameters> ")" ";" <Block> ";"
         */
        todo!();
    }

    fn parameters(&mut self) {
        todo!();
    }

    fn id(&mut self) -> String {
        /*
        <Id> ::= <Letter> { <Letter> | <Digit> | "_" }
        <Letter> ::= <Alphabetical character>
        <Digit> ::= <Numerical character>

        Checks that the next next token is Token::Variable
        and returns the string stored as identifier.
         */
        if let (Some(token), Some(position)) = self.peek() {
            match token {
                Token::Variable { value } => {
                    let res = value.clone();
                    self.pop(); // pop Token::Variable
                    return res;
                }
                _ => {}
            }
        }

        self.parse_error(String::from("Expected identifier."));
        String::new()
    }

    fn arguments(&mut self) -> Vec<Expression> {
        /*
        <Arguments> ::= "(" { <Expression> [","]} ")"

        Split arguments into individual expressions and use
        expression() to parse the expressions. Arguments will
        be inside parenthesis divided with commas e.g.

        '(<Expression>, <Expression>, <Expression>)'

        Since the expressions may contain parenthesis and
        even other function calls themselves we need to
        also keep track of the parenthesis count so we
        know the 'depth' we are at in the arguments e.g.

        'function_1(function_2(variable_1, variable_2), variable_1)'

        For function_1 the arguments would be:

        [
            function_2(variable_1, variable_2),
            variable_1
        ]

        TODO: Parameters are supposed to be always passed
              as reference where functions/procedures can
              read/write to the parameter memory address
              so how are these handled when the parameters
              are expressions? If the parameter is complex
              expression do we create new memory address for
              it and if it is simple variable pass the variable?
              If parameter itself involves functions/procedures
              that change some variable it is probably important
              to also evaluate them in some specific order?
         */
        if self.expect(Token::OpenParen) {
            return vec![];
        }

        let mut paren_count = 1;

        // TODO: Actually handle expressions
        while let (Some(token), Some(position)) = self.peek() {
            match token {
                Token::OpenParen => {
                    self.pop();
                    paren_count += 1;
                }
                Token::CloseParen => {
                    self.pop();
                    paren_count -= 1;
                }
                _ => {
                    self.pop();
                }
            }

            if paren_count == 0 {
                return vec![]; // Placeholder for success
            }
        }

        // If we get here we have unmatched paren count
        self.parse_error(String::from("Number of opening and closing parenthesis did not match."));
        vec![] // Placeholder for fail
    }

    fn expression(&mut self) -> Expression {
        /*
        TODO: CFG
         */
        todo!();
    }
}