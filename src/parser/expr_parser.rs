use super::*;

#[derive(Debug, Eq, PartialEq)]
struct Cursor {
    tokens: Vec<Expr>,
    index: usize,
}

impl Cursor {
    fn from(tokens: &Vec<Expr>, index: usize) -> Cursor {
        Cursor { tokens: tokens.clone(), index }
    }

    fn is_end(&mut self) -> bool {
        match self.tokens.get(self.index) {
            Some(_t) => { false }
            None => { true }
        }
    }

    fn advance(&mut self) -> Expr {
        if !self.is_end() {
            self.index += 1;
        }
        self.previous()
    }

    fn previous(&mut self) -> Expr {
        self.tokens.get(self.index - 1).unwrap().clone()
    }

    fn peek(&mut self) -> Expr {
        self.tokens.get(self.index).unwrap().clone()
    }

    fn check(&mut self, e: Expr) -> bool {
        return if self.is_end() {
            false
        } else {
            self.peek() == e
        };
    }

    fn matches(&mut self, v: Vec<Expr>) -> bool {
        for i in v.iter() {
            if self.check(i.clone()) {
                self.advance();
                return true;
            }
        }
        false
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Expression {
    cursor: Cursor,
}

impl Expression {
    pub fn from(expr: &Vec<Expr>) -> Expression {
        Expression {
            cursor: Cursor::from(expr, 0)
        }
    }

    pub fn expression(&mut self) -> Expr {
        /*
        Implementation based on example from
        https://craftinginterpreters.com/parsing-expressions.html

        <expression> ::= <and>
         */
        self.and()
    }

    fn and(&mut self) -> Expr {
        /*
        <and> ::= <and> "&" <equality>
        <and> ::= <equality> "&" <equality>
        <and> ::= <equality>
         */
        let mut expr = self.equality();

        while self.cursor.matches(vec![Expr::And]) {
            expr = Expr::Binary {
                op: Box::from(self.cursor.previous()),
                left: Box::from(expr),
                right: Box::from(self.equality()),
            }
        }

        expr
    }

    fn equality(&mut self) -> Expr {
        /*
        <equality> ::= <equality> "=" <comparison>
        <equality> ::= <comparison> "=" <comparison>
        <equality> ::= <comparison>
         */
        let mut expr = self.comparison();

        while self.cursor.matches(vec![Expr::Eq]) {
            expr = Expr::Binary {
                op: Box::from(self.cursor.previous()),
                left: Box::from(expr),
                right: Box::from(self.comparison()),
            }
        }

        expr
    }

    fn comparison(&mut self) -> Expr {
        /*
        <comparison> ::= <comparison> ">" <term>
        <comparison> ::= <term> ">" <term>
        <comparison> ::= <term>
         */
        let mut expr = self.term();

        while self.cursor.matches(vec![Expr::Less]) {
            expr = Expr::Binary {
                op: Box::from(self.cursor.previous()),
                left: Box::from(expr),
                right: Box::from(self.term()),
            }
        }

        expr
    }

    fn term(&mut self) -> Expr {
        /*
        <term> ::= <term> "+" | "-"
        <term> ::= <factor> "+" | "-" <factor>
        <term> ::= <factor>
         */
        let mut expr = self.factor();

        while self.cursor.matches(vec![Expr::Plus, Expr::Minus]) {
            expr = Expr::Binary {
                op: Box::from(self.cursor.previous()),
                left: Box::from(expr),
                right: Box::from(self.factor()),
            }
        }

        expr
    }

    fn factor(&mut self) -> Expr {
        /*
        <factor> ::= <factor> "*" | "/" <unary>
        <factor> ::= <unary> "*" | "/" <unary>
        <factor> ::= <unary>
         */
        let mut expr = self.unary();

        while self.cursor.matches(vec![Expr::Mul, Expr::Div]) {
            expr = Expr::Binary {
                op: Box::from(self.cursor.previous()),
                left: Box::from(expr),
                right: Box::from(self.unary()),
            }
        }

        expr
    }

    fn unary(&mut self) -> Expr {
        /*
        <unary> ::= "" | "-" | "!" <primary>
        <unary> ::= <primary>
         */
        if self.cursor.matches(vec![Expr::Not, Expr::Minus]) {
            let op = self.cursor.previous();
            let r = self.unary();
            return Expr::Unary { op: Box::from(op), value: Box::from(r) };
        }
        self.primary()
    }

    fn primary(&mut self) -> Expr {
        /*
        <primary> ::= <string> | <number> | <variable> | "(" <expression> ")"
         */
        let ex = self.cursor.peek();
        let e = ex.clone();
        return match ex {
            Expr::Number { value: _ } => {
                self.cursor.advance();
                e.clone()
            }
            Expr::Variable { value: _ } => {
                self.cursor.advance();
                e.clone()
            }
            Expr::String { value: _ } => {
                self.cursor.advance();
                e.clone()
            }
            Expr::OpenParen => {
                self.cursor.advance();
                let expr = self.expression();
                return if self.cursor.matches(vec![Expr::CloseParen]) {
                    Expr::Group { value: Box::from(expr) }
                } else {
                    Expr::Error
                };
            }
            _ => {
                Expr::Error
            }
        };
    }
}