use std::borrow::Borrow;
use crate::parser::{Expr, Tree, VarType};
use std::collections::HashMap;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Variable {
    Int,
    String,
    Bool,
    Error,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct SemanticAnalyzer {
    scopes: Vec<HashMap<String, Variable>>,
    pub errors: Vec<String>,
}

impl SemanticAnalyzer {
    pub fn from(ast: Tree) -> SemanticAnalyzer {
        let mut res = SemanticAnalyzer {
            scopes: vec![],
            errors: vec![],
        };
        res.check(&ast.clone());
        res
    }

    fn check(&mut self, ast: &Tree) {
        /*
        Checks:
         - Variable not in scope when initialized
         - Variable in scope when accessed
         - Resolve expression types and check they match the variables
         - Variable in for-loop is int
         - Expressions in for-loop are int
         */
        self.add_scope();

        match ast {
            Tree::Statements { value } => {
                for statement in value.iter() {
                    match statement {
                        Tree::Var { name, value, var_type } => {
                            // Initialize variable
                            match var_type {
                                VarType::Int => {
                                    self.init_var(name.clone(), Variable::Int);
                                }
                                VarType::Bool => {
                                    self.init_var(name.clone(), Variable::Bool);
                                }
                                VarType::String => {
                                    self.init_var(name.clone(), Variable::String);
                                }
                            }
                            // Assign evaluated expression as value
                            match value.borrow() {
                                Tree::Expr { value } => {
                                    let new_val = self.evaluate(value);
                                    self.mutate_var(name.clone(), new_val);
                                }
                                _ => {
                                    // Should not happen
                                    self.errors.push(String::from("Expected Tree::Expr."));
                                }
                            }
                        }
                        Tree::Assign { var, value } => {
                            match value.borrow() {
                                Tree::Expr { value } => {
                                    let new_val = self.evaluate(value);
                                    self.mutate_var(var.clone(), new_val);
                                }
                                _ => {
                                    // Should not happen
                                    self.errors.push(String::from("Expected Tree::Expr."));
                                }
                            }
                        }
                        Tree::For { var, start, end, statements } => {
                            // Evaluate left expression
                            let cs: &Tree = start.borrow();
                            let s: Variable = if let Tree::Expr { value } = cs.clone() {
                                self.evaluate(&value)
                            } else {
                                Variable::Error
                            };

                            // Evaluate right expression type
                            let ce: &Tree = end.borrow();
                            let e: Variable = if let Tree::Expr { value } = ce.clone() {
                                self.evaluate(&value)
                            } else {
                                Variable::Error
                            };

                            // Get var type
                            let v = self.get_var(var.clone());

                            let t = (s, e, v);

                            match t {
                                (Variable::Int, Variable::Int, Variable::Int) => {
                                    /* Everything is fine */
                                }
                                _ => {
                                    self.errors.push(String::from("For loop variable and expressions need to be type int."));
                                }
                            }
                            // Recursively check for loop statements
                            self.check(statements);
                        }
                        Tree::Print { value } => {
                            let v = value.borrow();
                            match v {
                                Tree::Expr { value } => {
                                    let eval = self.evaluate(value);

                                    if let Variable::Error = eval {
                                        self.errors.push(String::from("Expression evaluation error."));
                                    }
                                }
                                _ => { /* Should not happen */ }
                            }
                        }
                        _ => { /* Statements that don't go through semantic analysis */ }
                    }
                }
            }
            _ => {
                // Should not happen
                self.errors.push(String::from("Expected Tree::Statements."));
            }
        }

        self.drop_scope();
    }

    fn add_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn drop_scope(&mut self) {
        self.scopes.pop();
    }

    fn get_var(&mut self, key: String) -> Variable {
        for scope in self.scopes.iter() {
            if scope.contains_key(&key) {
                let res = scope.get(&key).unwrap();
                return res.clone();
            }
        }
        self.errors.push(format!("Variable '{}' is not in scope.", &key));
        Variable::Error
    }

    fn init_var(&mut self, key: String, var: Variable) {
        for scope in self.scopes.iter() {
            if scope.contains_key(&key) {
                self.errors.push(String::from(format!("Variable '{}' is already in scope during initialization.", &key)));
                return;
            }
        }
        self.scopes.last_mut().unwrap().insert(key, var);
    }

    fn mutate_var(&mut self, key: String, var: Variable) {
        for scope in self.scopes.iter_mut() {
            if scope.contains_key(&key) {
                scope.insert(key.clone(), var.clone());
                return;
            }
        }
        self.errors.push(format!("Variable '{}' not in scope.", &key));
    }

    fn evaluate(&mut self, expression: &Expr) -> Variable {
        /*
        Evaluates the type of expression.
         */
        return match expression {
            Expr::Bool { value: _ } => { Variable::Bool }
            Expr::String { value: _ } => { Variable::String }
            Expr::Number { value: _ } => { Variable::Int }
            Expr::Unary { op, value } => {
                // Evaluate value
                let evaluated = self.evaluate(value.borrow());
                // Check both unary operators - and !
                match op.borrow() {
                    Expr::Minus => {
                        /*
                        Check that expression in the value
                        is int since we can't get negative
                        value of string or bool.
                         */
                        match evaluated {
                            Variable::Int => {
                                Variable::Int
                            }
                            _ => { Variable::Error }
                        }
                    }
                    Expr::Not => {
                        /*
                        Check that expression in the value is
                        bool since we can not use not operator
                        on int or string.
                         */
                        match evaluated {
                            Variable::Bool => {
                                Variable::Bool
                            }
                            _ => { Variable::Error }
                        }
                    }
                    _ => { Variable::Error }
                }
            }
            Expr::Binary { op, left, right: _ } => {
                let l = self.evaluate(left.borrow());
                let r = self.evaluate(left.borrow());
                let t = (l, r);

                match op.borrow() {
                    Expr::Plus => {
                        match t {
                            (Variable::Int, Variable::Int) => {
                                Variable::Int
                            }
                            (Variable::String, Variable::String) => {
                                Variable::String
                            }
                            (Variable::String, Variable::Int) => {
                                Variable::String
                            }
                            (Variable::String, Variable::Bool) => {
                                Variable::String
                            }
                            _ => { Variable::Error }
                        }
                    }
                    Expr::Minus => {
                        match t {
                            (Variable::Int, Variable::Int) => {
                                Variable::Int
                            }
                            _ => { Variable::Error }
                        }
                    }
                    Expr::Mul => {
                        match t {
                            (Variable::Int, Variable::Int) => {
                                Variable::Int
                            }
                            _ => { Variable::Error }
                        }
                    }
                    Expr::Div => {
                        match t {
                            (Variable::Int, Variable::Int) => {
                                Variable::Int
                            }
                            _ => { Variable::Error }
                        }
                    }
                    Expr::And => {
                        match t {
                            (Variable::Bool, Variable::Bool) => {
                                Variable::Bool
                            }
                            _ => { Variable::Error }
                        }
                    }
                    Expr::Less => {
                        match t {
                            (Variable::Bool, Variable::Bool) => {
                                Variable::Bool
                            }
                            _ => { Variable::Error }
                        }
                    }
                    Expr::Eq => {
                        match t {
                            (Variable::Bool, Variable::Bool) => {
                                Variable::Bool
                            }
                            _ => { Variable::Error }
                        }
                    }
                    _ => { Variable::Error }
                }
            }
            Expr::Variable { value } => {
                self.get_var(value.clone())
            }
            Expr::Group { value } => { self.evaluate(value.borrow()) }
            _ => { Variable::Error }
        };
    }
}