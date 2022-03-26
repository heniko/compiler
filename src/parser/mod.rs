mod expr_parser;

use crate::scanner::{Token, TokenPosition};
use std::collections::VecDeque;
use std::string::ParseError;
use expr_parser::ExpressionParser;

#[cfg(test)]
mod tests;

#[derive(Debug, PartialEq, Clone)]
pub struct VariableDeclaration {
    id: String,
    var_type: String,
    arr: bool,
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
    VariableDeclaration { variables: Vec<VariableDeclaration> },
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
    BooleanLiteral { value: bool },
    Function { id: String, arguments: Vec<Expression> },
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
    Procedure { id: String, parameters: Vec<VariableDeclaration>, block: Statement },
    Function { id: String, parameters: Vec<VariableDeclaration>, block: Statement, res_type: String },
    //Call { id: String, arguments: Vec<Expression> },
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

    fn next(&mut self) -> (Option<&Token>, Option<&TokenPosition>) {
        (self.tokens.get(1), self.positions.get(1))
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
            return if token.clone() == match_token {
                self.pop(); // Consume expected token
                true
            } else {
                let err_token = token.clone();
                self.parse_error(format!("Expected token: {:?}, found token: {:?}.", match_token.clone(), err_token));
                false
            };
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
        if let (Some(Token::Variable { value }), Some(_position)) = self.peek() {
            id = value.clone();
            self.pop();
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
                    procedures.push(self.parse_procedure());
                }
                Token::Function => {
                    functions.push(self.parse_function());
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
                Token::Begin => { self.block() }
                Token::While => { self.while_statement() }
                Token::Return => { self.return_statement() }
                Token::If => { self.if_statement() }
                Token::Assert => { self.assert_statement() }
                Token::Var => { self.var_declaration() }
                Token::Variable { value: _ } => { self.call_or_assign() }
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

        return if let (Some(Token::Else), Some(_position)) = self.peek() {
            self.pop();
            let else_statement = self.statement();
            Statement::IfElse {
                value: expression,
                if_statement: Box::from(statement),
                else_statement: Box::from(else_statement),
            }
        } else {
            Statement::If {
                value: expression,
                statement: Box::from(statement),
            }
        };
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

    fn var_declaration(&mut self) -> Statement {
        /*
        <VarDeclaration> ::= "var" <Id> { "," <Id> } ":" <Type> ";"
         */
        if !self.expect(Token::Var) {
            return Statement::Error;
        }

        let mut ids = vec![];
        let var_type;
        let arr;

        while let (Some(Token::Variable { value }), Some(_position)) = self.peek() {
            ids.push(value.clone());
            self.pop();

            if let (Some(t), Some(_p)) = self.peek() {
                /*
                pop comma if there is one and check that there is
                a variable next so for example

                var s1, s2, s3, : string;

                isn't valid.
                 */
                self.pop();
                if let (Some(Token::Variable { value: _ }), Some(_p)) = self.peek() {
                    /* Do nothing */
                } else {
                    self.parse_error(String::from("Expected identifier."));
                    return Statement::Error;
                }
            }
        }

        // All var ids are parsed so expect :
        if !self.expect(Token::Colon) {
            return Statement::Error;
        }

        // Check if next is brackets or just type
        if let (Some(token), Some(position)) = self.peek() {
            match token {
                Token::OpenBracket => {
                    // Parses [<var id>]
                    arr = true;
                    self.pop(); // pop [

                    if let (Some(Token::Variable { value }), Some(p)) = self.peek() {
                        var_type = value.clone();
                    } else {
                        self.parse_error(String::from("Expected variable type."));
                        return Statement::Error;
                    }

                    if !self.expect(Token::CloseBracket) {
                        return Statement::Error;
                    }
                }
                Token::Variable { value } => {
                    arr = false;
                    var_type = value.clone();
                }
                _ => {
                    self.parse_error(String::from("Expected variable type."));
                    return Statement::Error;
                }
            }
        } else {
            self.parse_error(String::from("Expected type identifier."));
            return Statement::Error;
        }

        // Expect statement to end in ;
        return if !self.expect(Token::Semicolon) {
            Statement::Error
        } else {
            // Everything is fine and we can return variable declarations
            Statement::Error
        };
    }

    fn call_or_assign(&mut self) -> Statement {
        /*
        Checks if the statement is a call or assign and
        calls the correct handler to parse the statement.
         */
        return if let (Some(token), Some(position)) = self.next() {
            return match token {
                Token::Assign => {
                    self.assign()
                }
                Token::OpenParen => {
                    self.call()
                }
                _ => {
                    // Not assign or call so we don't know what it is
                    self.parse_error(String::from("Expected ':=' or '('."));
                    Statement::Error
                }
            };
        } else {
            // EOF
            self.parse_error(String::from("Expected ':=' or '('."));
            Statement::Error
        };
    }

    fn call(&mut self) -> Statement {
        /*
        <CallStatement> ::= <Id> <Arguments> ";"
         */
        let id;
        let arguments;

        // Get function/procedure id
        if let (Some(Token::Variable { value }), Some(_position)) = self.peek() {
            id = value.clone();
            self.pop();
        } else {
            self.parse_error(String::from(""));
            return Statement::Error;
        }

        // Get arguments
        arguments = self.arguments();

        // Check that statement end with semicolon
        if !self.expect(Token::Semicolon) {
            Statement::Error
        } else {
            Statement::Call {
                id,
                arguments,
            }
        }
    }

    fn assign(&mut self) -> Statement {
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

        // Get expression (This will also return Expression::None if there isn't one)
        let expression = self.expression();

        // Check that statement ends in ;
        return if !self.expect(Token::Semicolon) {
            Statement::Error
        } else {
            Statement::Return {
                value: expression
            }
        };
    }

    fn parse_function(&mut self) -> AST {
        /*
        <Function> ::= "function" <Id> "(" <Parameters> ")" ":" <Variable> ";" <Block> ";"
         */
        let id;
        let parameters;
        let res_type;
        let block;

        if !self.expect(Token::Function) {
            return AST::Error;
        }

        if let (Some(Token::Variable { value }), Some(position)) = self.peek() {
            id = value.clone();
            self.pop();
        } else {
            self.parse_error(String::from("Expected identifier."));
            return AST::Error;
        }

        parameters = self.parameters();

        if !self.expect(Token::Colon) {
            return AST::Error;
        }

        if let (Some(Token::Variable { value }), Some(position)) = self.peek() {
            res_type = value.clone();
            self.pop();
        } else {
            self.parse_error(String::from("Expected identifier."));
            return AST::Error;
        }

        if !self.expect(Token::Semicolon) {
            return AST::Error;
        }

        block = self.block();

        return if !self.expect(Token::Semicolon) {
            AST::Error
        } else {
            AST::Function {
                id,
                parameters,
                res_type,
                block,
            }
        };
    }

    fn parse_procedure(&mut self) -> AST {
        /*
        <Procedure> ::= "procedure" <Id> "(" <Parameters> ")" ";" <Block> ";"
         */
        let id;
        let parameters;
        let block;

        if !self.expect(Token::Procedure) {
            return AST::Error;
        }

        if let (Some(Token::Variable { value }), Some(position)) = self.peek() {
            id = value.clone();
            self.pop();
        } else {
            self.parse_error(String::from("Expected identifier."));
            return AST::Error;
        }

        parameters = self.parameters();

        if !self.expect(Token::Semicolon) {
            return AST::Error;
        }

        block = self.block();

        return if !self.expect(Token::Semicolon) {
            AST::Error
        } else {
            AST::Procedure {
                id,
                parameters,
                block,
            }
        };
    }

    fn parameters(&mut self) -> Vec<VariableDeclaration> {
        let mut parameters = vec![];

        if !self.expect(Token::OpenParen) {
            return vec![];
        }

        if let (Some(Token::Comma), Some(_position)) = self.peek() {
            self.parse_error(String::from("Expected parameter."));
            return vec![];
        }

        loop {
            if let (Some(token), Some(position)) = self.peek() {
                // If ')' is next we break out of the loop
                if token == &Token::CloseParen {
                    break;
                }

                /*
                Get rid of commas that separate parameters.

                This would technically make

                function x(,x : integer) : string;

                valid if we didn't check it before the loop.
                 */
                if token == &Token::Comma {
                    self.pop();
                }

                // Parse parameter and add it to the list
                let param = self.parameter();
                match param {
                    Some(p) => {
                        parameters.push(p);
                    }
                    None => {
                        self.parse_error(String::from("Error while parsing parameters."));
                    }
                }
            }
        }

        if !self.expect(Token::CloseParen) {
            return vec![];
        }

        parameters
    }

    fn parameter(&mut self) -> Option<VariableDeclaration> {
        /*
        Some(VariableDeclaration) if parameter is correct.
        None if parameter is invalid.
         */
        let id;
        let arr;
        let var_type;

        if let (Some(Token::Variable { value }), Some(position)) = self.peek() {
            id = value.clone();
            self.pop();
        } else {
            return None;
        }

        if !self.expect(Token::Colon) {
            return None;
        }

        // Check if is array type
        if let (Some(Token::OpenBracket), Some(position)) = self.peek() {
            arr = true;
            self.pop();
        } else {
            arr = false;
        }

        // Get type
        if let (Some(Token::Variable { value }), Some(position)) = self.peek() {
            var_type = value.clone();
            self.pop();
        } else {
            return None;
        }

        // If arr check for closing brackets
        if arr {
            if !self.expect(Token::CloseBracket) {
                return None;
            }
        }

        Some(
            VariableDeclaration {
                id,
                arr,
                var_type,
            }
        )
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
        if !self.expect(Token::OpenParen) {
            return vec![];
        }

        let mut arguments = vec![];

        while let (Some(token), Some(position)) = self.peek() {
            match token {
                Token::Comma => {
                    self.pop();
                    arguments.push(self.expression());
                }
                Token::CloseParen => {
                    self.pop();
                    break;
                }
                _ => {
                    arguments.push(self.expression());
                }
            }
        }

        arguments
    }

    fn expression(&mut self) -> Expression {
        /*
        Functionality:
         - Starting point for parsing any kind of expression
         - Turn Vecdeque<Token> to Vec<Expression> and use
           expr_parser to parse the expression
         - Recursively parse functions to Expression::Function
         */
        let mut expr_tokens = vec![];
        /*
        If paren_count goes under 0 then we know we are out
        of the expression. Consider:

        We need to parse
        writeln((1+2)*3);

        arguments() calls this function at the point where the
        first expression starts:

        (1+2)*3);

        We parse the expression and when only

        );

        is left paren_count will be -1 and we know that is the
        end of the expression even though ')' could as a character be
        part of the expression. We can also stop parsing the expression
        if we find some character that clearly isn't part of the
        expression like ';'.
         */
        let mut paren_count = 0;

        while let (Some(token), Some(position)) = self.peek() {
            match token {
                Token::OpenParen => {
                    paren_count += 1;
                    expr_tokens.push(Expression::OpenParen);
                }
                Token::CloseParen => {
                    paren_count -= 1;
                    if paren_count == -1 {
                        // We found parenthesis that is outside of the expression
                        break;
                    } else {
                        // We did not find ending parenthesis
                        expr_tokens.push(Expression::CloseParen);
                    }
                }
                Token::Plus => { expr_tokens.push(Expression::Plus); }
                Token::Minus => { expr_tokens.push(Expression::Minus); }
                Token::Multiply => { expr_tokens.push(Expression::Multiply); }
                Token::Divide => { expr_tokens.push(Expression::Divide); }
                Token::Le => { expr_tokens.push(Expression::Le); }
                Token::Leq => { expr_tokens.push(Expression::Leq); }
                Token::Ge => { expr_tokens.push(Expression::Ge); }
                Token::Geq => { expr_tokens.push(Expression::Geq); }
                Token::Inequality => { expr_tokens.push(Expression::Inequality); }
                Token::Eq => { expr_tokens.push(Expression::Eq); }
                Token::Or => { expr_tokens.push(Expression::Or); }
                Token::And => { expr_tokens.push(Expression::And); }
                Token::Not => { expr_tokens.push(Expression::Not); }
                Token::Variable { value } => {
                    expr_tokens.push(self.variable_or_function_expression());
                    continue; // Skip loop pop() to make things easier
                }
                Token::StringLiteral { value } => {
                    expr_tokens.push(Expression::StringLiteral { value: value.clone() });
                }
                Token::IntegerLiteral { value } => {
                    expr_tokens.push(Expression::IntegerLiteral { value: value.clone() });
                }
                Token::RealLiteral { value } => {
                    expr_tokens.push(Expression::RealLiteral { value: value.clone() });
                }
                _ => {
                    // Found token that cannot be part of expression like ';'
                    break;
                }
            }
            self.pop();
        }

        let mut expression_parser = ExpressionParser::from(&expr_tokens);
        expression_parser.expression()
    }

    fn variable_or_function_expression(&mut self) -> Expression {
        let id;
        if let (Some(Token::Variable { value }), Some(_p)) = self.pop() {
            id = value;
        } else {
            // Should not happen but makes sure id is initialized
            self.parse_error(String::from("Expected Token::Variable."));
            return Expression::Error;
        }

        return if let (Some(Token::OpenParen), Some(_position)) = self.peek() {
            Expression::Function {
                id: id.clone(),
                arguments: self.arguments(),
            }
        } else {
            Expression::Variable { id: id.clone() }
        };
    }
}