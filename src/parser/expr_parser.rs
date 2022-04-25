use super::*;

#[derive(Debug, PartialEq)]
struct Cursor {
    tokens: Vec<Expression>,
    index: usize,
}

impl Cursor {
    fn from(tokens: &Vec<Expression>, index: usize) -> Cursor {
        Cursor {
            tokens: tokens.clone(),
            index,
        }
    }

    fn is_end(&mut self) -> bool {
        match self.tokens.get(self.index) {
            Some(_t) => false,
            None => true,
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
            cursor: Cursor::from(expr, 0),
        }
    }

    pub fn expression(&mut self) -> Expression {
        /*
        Implementation based on example from
        https://craftinginterpreters.com/parsing-expressions.html

        <expression> ::= <and>
         */
        if self.cursor.is_end() {
            Expression::None
        } else {
            self.comparison()
        }
    }

    fn comparison(&mut self) -> Expression {
        /*
        <comparison> ::= <comparison> ">" <term>
        <comparison> ::= <term> ">" <term>
        <comparison> ::= <term>
         */
        let mut expr = self.term();

        while self.cursor.matches(vec![
            Expression::Le,         // <
            Expression::Eq,         // =
            Expression::Leq,        // <=
            Expression::Ge,         // >
            Expression::Geq,        // >=
            Expression::Inequality, // <>
        ]) {
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
        <term> ::= <term> "+" | "-" | "or"
        <term> ::= <factor> "+" | "-" | "or" <factor>
        <term> ::= <factor>
         */
        let mut expr = self.factor();

        while self
            .cursor
            .matches(vec![Expression::Plus, Expression::Minus, Expression::Or])
        {
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

        while self.cursor.matches(vec![
            Expression::Multiply,
            Expression::Divide,
            Expression::Modulo,
            Expression::And,
        ]) {
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
        if self
            .cursor
            .matches(vec![Expression::Not, Expression::Minus, Expression::Plus])
        {
            let op = self.cursor.previous();
            let r = self.unary();
            return Expression::Unary {
                op: Box::from(op),
                value: Box::from(r),
            };
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
                e
            }
            Expression::Variable { var: _ } => {
                self.cursor.advance();
                e
            }
            Expression::StringLiteral { value: _ } => {
                self.cursor.advance();
                e
            }
            Expression::RealLiteral { value: _ } => {
                self.cursor.advance();
                e
            }
            Expression::Function {
                id: _,
                arguments: _,
            } => {
                self.cursor.advance();
                e
            }
            Expression::OpenParen => {
                self.cursor.advance();
                let expr = self.expression();
                return if self.cursor.matches(vec![Expression::CloseParen]) {
                    Expression::Group {
                        value: Box::from(expr),
                    }
                } else {
                    Expression::Error
                };
            }
            _ => Expression::Error,
        };
    }
}
