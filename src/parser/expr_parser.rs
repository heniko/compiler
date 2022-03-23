use super::*;

#[derive(Debug, PartialEq)]
struct Cursor {
    tokens: Vec<Expression>,
    index: usize,
}

impl Cursor {
    fn from(tokens: &Vec<Expression>, index: usize) -> Cursor {
        Cursor { tokens: tokens.clone(), index }
    }

    fn is_end(&mut self) -> bool {
        match self.tokens.get(self.index) {
            Some(_t) => { false }
            None => { true }
        }
    }

    fn advance(&mut self) -> Expression {
        if !self.is_end() {
            self.index += 1;
        }
        self.previous()
    }

    fn previous(&mut self) -> Expression {
        self.tokens.get(self.index - 1).unwrap().clone()
    }

    fn peek(&mut self) -> Expression {
        self.tokens.get(self.index).unwrap().clone()
    }

    fn check(&mut self, e: Expression) -> bool {
        return if self.is_end() {
            false
        } else {
            self.peek() == e
        };
    }

    fn matches(&mut self, v: Vec<Expression>) -> bool {
        for i in v.iter() {
            if self.check(i.clone()) {
                self.advance();
                return true;
            }
        }
        false
    }
}

#[derive(Debug, PartialEq)]
pub struct ExpressionParser {
    cursor: Cursor,
}

impl ExpressionParser {
    pub fn from(expr: &Vec<Expression>) -> ExpressionParser {
        ExpressionParser {
            cursor: Cursor::from(expr, 0)
        }
    }

    pub fn expression(&mut self) -> Expression {
        /*
        Implementation based on example from
        https://craftinginterpreters.com/parsing-expressions.html

        <expression> ::= <and>
         */
        self.and()
    }

    fn and(&mut self) -> Expression {
        /*
        <and> ::= <and> "&" <equality>
        <and> ::= <equality> "&" <equality>
        <and> ::= <equality>
         */
        let mut expr = self.equality();

        while self.cursor.matches(vec![Expression::And]) {
            expr = Expression::Binary {
                op: Box::from(self.cursor.previous()),
                left: Box::from(expr),
                right: Box::from(self.equality()),
            }
        }

        expr
    }

    fn equality(&mut self) -> Expression {
        /*
        <equality> ::= <equality> "=" <comparison>
        <equality> ::= <comparison> "=" <comparison>
        <equality> ::= <comparison>
         */
        let mut expr = self.comparison();

        while self.cursor.matches(vec![Expression::Eq]) {
            expr = Expression::Binary {
                op: Box::from(self.cursor.previous()),
                left: Box::from(expr),
                right: Box::from(self.comparison()),
            }
        }

        expr
    }

    fn comparison(&mut self) -> Expression {
        /*
        <comparison> ::= <comparison> ">" <term>
        <comparison> ::= <term> ">" <term>
        <comparison> ::= <term>
         */
        let mut expr = self.term();

        while self.cursor.matches(vec![Expression::Le]) {
            expr = Expression::Binary {
                op: Box::from(self.cursor.previous()),
                left: Box::from(expr),
                right: Box::from(self.term()),
            }
        }

        expr
    }

    fn term(&mut self) -> Expression {
        /*
        <term> ::= <term> "+" | "-"
        <term> ::= <factor> "+" | "-" <factor>
        <term> ::= <factor>
         */
        let mut expr = self.factor();

        while self.cursor.matches(vec![Expression::Plus, Expression::Minus]) {
            expr = Expression::Binary {
                op: Box::from(self.cursor.previous()),
                left: Box::from(expr),
                right: Box::from(self.factor()),
            }
        }

        expr
    }

    fn factor(&mut self) -> Expression {
        /*
        <factor> ::= <factor> "*" | "/" <unary>
        <factor> ::= <unary> "*" | "/" <unary>
        <factor> ::= <unary>
         */
        let mut expr = self.unary();

        while self.cursor.matches(vec![Expression::Multiply, Expression::Divide]) {
            expr = Expression::Binary {
                op: Box::from(self.cursor.previous()),
                left: Box::from(expr),
                right: Box::from(self.unary()),
            }
        }

        expr
    }

    fn unary(&mut self) -> Expression {
        /*
        <unary> ::= "" | "-" | "!" <primary>
        <unary> ::= <primary>
         */
        if self.cursor.matches(vec![Expression::Not, Expression::Minus]) {
            let op = self.cursor.previous();
            let r = self.unary();
            return Expression::Unary { op: Box::from(op), value: Box::from(r) };
        }
        self.primary()
    }

    fn primary(&mut self) -> Expression {
        /*
        <primary> ::= <string> | <number> | <variable> | "(" <expression> ")"
         */
        let ex = self.cursor.peek();
        let e = ex.clone();
        return match ex {
            Expression::IntegerLiteral { value: _ } => {
                self.cursor.advance();
                e.clone()
            }
            Expression::Variable { id: _ } => {
                self.cursor.advance();
                e.clone()
            }
            Expression::StringLiteral { value: _ } => {
                self.cursor.advance();
                e.clone()
            }
            Expression::OpenParen => {
                self.cursor.advance();
                let expr = self.expression();
                return if self.cursor.matches(vec![Expression::CloseParen]) {
                    Expression::Group { value: Box::from(expr) }
                } else {
                    Expression::Error
                };
            }
            _ => {
                Expression::Error
            }
        };
    }
}