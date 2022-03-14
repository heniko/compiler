use std::borrow::Borrow;
use crate::parser::{Expr, Tree, VarType};
use std::collections::{HashMap, VecDeque};
use crate::io::{IO};

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Variable {
    Number { value: i32 },
    String { value: String },
    Bool { value: bool },
    Error,
}

pub struct Interpreter {
    /*
    program: Hold the AST that is being interpreted.

    scopes: Holds the variables. When new statements
    block is started inside for loop we can create
    new scope that can be dropped when that block
    of statements ends.

    io: Is responsible for reading user (or mock user)
    inputs and printing text to user (or storing it)
    for unit tests.
     */
    program: Tree,
    scopes: VecDeque<HashMap<String, Variable>>,
    io: Box<dyn IO>,
}

impl Interpreter {
    pub fn from(program: Tree, io: Box<dyn IO>) -> Interpreter {
        Interpreter {
            program,
            scopes: VecDeque::new(),
            io,
        }
    }

    pub fn run(&mut self) {
        self.statements(&self.program.clone());
    }

    fn init_var(&mut self, id: String, var: Variable) -> Variable {
        /*
        We want to check that the variable does not
        exist in any of the scopes and if so we want to
        add it to the current (last) scope. If variable is
        found inside the loop then return error.
         */
        for scope in self.scopes.iter() {
            if scope.contains_key(&id) {
                panic!("Variable already exists.");
            }
        }

        self.scopes.back_mut().unwrap().insert(id.clone(), var.clone());
        var.clone()
    }

    fn assign_var(&mut self, id: String, var: Variable) -> Variable {
        /*
        We want to find the scope the variable is in
        and change it. If it does not exist in any scope
        we want to return error.
         */
        for scope in self.scopes.iter_mut() {
            if scope.contains_key(&id) {
                scope.insert(id.clone(), var.clone());
                return var.clone();
            }
        }

        panic!("Variable not in scope.");
    }

    fn get_var(&mut self, id: String) -> Variable {
        /*
        We want to find the scope the variable is in and
        return a copy of the variable. IF it does not
        exist in any scope then return error.
         */
        for scope in self.scopes.iter() {
            if scope.contains_key(&id) {
                return scope.get(&id).unwrap().clone();
            }
        }

        panic!("Variable not in scope!");
    }

    fn evaluate(&mut self, expression: &Tree) -> Variable {
        match expression {
            Tree::Expr { value } => {
                self.eval(&value)
            }
            _ => {
                Variable::Error
            }
        }
    }

    fn eval(&mut self, expr: &Expr) -> Variable {
        /*
        Recursively evaluate the expression.
         */
        match expr {
            Expr::Binary { op, left, right } => {
                /*
                To match binary statement we first need to recursively
                evaluate the left and right branches and then we need
                to check for every operator if there is a way of combining
                these variables. For example we do not want to allow
                multiplication of two string literals.
                 */
                let l = self.eval(left.borrow());
                let r = self.eval(right.borrow());
                let t = (l, r); // Tuple for matching

                return match op.borrow() {
                    Expr::Plus => {
                        /*
                        For simplicity we only allow combining strings
                        with other type of values when the string is
                        the left branch. So to get a string literal from
                        number literal one would have to use expression
                        '"" + <number>'
                         */
                        match t {
                            (Variable::Number { value: v1 }, Variable::Number { value: v2 }) => {
                                Variable::Number { value: v1 + v2 }
                            }
                            (Variable::String { value: s1 }, Variable::String { value: s2 }) => {
                                Variable::String { value: format!("{}{}", s1, s2) }
                            }
                            (Variable::String { value: s }, Variable::Number { value: n }) => {
                                Variable::String { value: format!("{}{}", s, n) }
                            }
                            (Variable::String { value: s }, Variable::Bool { value: b }) => {
                                Variable::String { value: format!("{}{}", s, b) }
                            }
                            _ => { Variable::Error }
                        }
                    }
                    Expr::Minus => {
                        match t {
                            (Variable::Number { value: n1 }, Variable::Number { value: n2 }) => {
                                Variable::Number { value: n1 - n2 }
                            }
                            _ => { Variable::Error }
                        }
                    }
                    Expr::Mul => {
                        match t {
                            (Variable::Number { value: n1 }, Variable::Number { value: n2 }) => {
                                Variable::Number { value: n1 * n2 }
                            }
                            _ => { Variable::Error }
                        }
                    }
                    Expr::Div => {
                        match t {
                            (Variable::Number { value: n1 }, Variable::Number { value: n2 }) => {
                                Variable::Number { value: n1 / n2 }
                            }
                            _ => { Variable::Error }
                        }
                    }
                    Expr::And => {
                        match t {
                            (Variable::Bool { value: b1 }, Variable::Bool { value: b2 }) => {
                                Variable::Bool { value: b1 && b2 }
                            }
                            _ => { Variable::Error }
                        }
                    }
                    Expr::Less => {
                        match t {
                            (Variable::Number { value: n1 }, Variable::Number { value: n2 }) => {
                                Variable::Bool { value: n1 < n2 }
                            }
                            _ => { Variable::Error }
                        }
                    }
                    Expr::Eq => {
                        match t {
                            (Variable::Number { value: n1 }, Variable::Number { value: n2 }) => {
                                Variable::Bool { value: n1 == n2 }
                            }
                            (Variable::Bool { value: b1 }, Variable::Bool { value: b2 }) => {
                                Variable::Bool { value: b1 == b2 }
                            }
                            (Variable::String { value: s1 }, Variable::String { value: s2 }) => {
                                Variable::Bool { value: s1 == s2 }
                            }
                            _ => { Variable::Error }
                        }
                    }
                    _ => {
                        Variable::Error
                    }
                };
            }
            Expr::Number { value } => {
                return Variable::Number { value: value.clone() };
            }
            Expr::Bool { value } => {
                return Variable::Bool { value: value.clone() };
            }
            Expr::String { value } => {
                Variable::String { value: value.clone() }
            }
            Expr::Variable { value } => {
                return self.get_var(value.clone());
            }
            Expr::Group { value } => {
                self.eval(value.borrow())
            }
            Expr::Unary { value, op } => {
                let v = self.eval(value.borrow());

                match op.borrow() {
                    Expr::Minus => {
                        match v {
                            Variable::Number { value } => {
                                Variable::Number { value: -value }
                            }
                            _ => { Variable::Error }
                        }
                    }
                    Expr::Not => {
                        match v {
                            Variable::Bool { value } => {
                                Variable::Bool { value: !value }
                            }
                            _ => { Variable::Error }
                        }
                    }
                    _ => { Variable::Error }
                }
            }
            _ => { return Variable::Error; }
        }
    }

    fn statements(&mut self, statements: &Tree) {
        /*
        Creates new scope for the statement block.
        Executes the statements in the statement block.
         */
        self.scopes.push_back(HashMap::new());

        match statements {
            Tree::Statements { value } => {
                for stmt in value.iter() {
                    match stmt {
                        Tree::For { var, start, end, statements } => {
                            /*
                            In the loop we want to know the starting value,
                            ending value, direction and variable name.
                             */
                            let start_val = self.evaluate(start.borrow());
                            let end_val = self.evaluate(end.borrow());

                            let start_val_int = match start_val {
                                Variable::Number { value } => {
                                    value.clone()
                                }
                                _ => {
                                    panic!("For loop start expression was not integer!");
                                }
                            };

                            let end_val_int = match end_val {
                                Variable::Number { value } => {
                                    value.clone()
                                }
                                _ => {
                                    panic!("For loop start expression was not integer!");
                                }
                            };

                            // Assign the value so it is correct in the beginning of the loop
                            self.assign_var(var.clone(), start_val.clone());

                            if start_val_int < end_val_int {
                                let mut iter = start_val_int;

                                while iter <= end_val_int {
                                    self.statements(statements.borrow());
                                    match self.get_var(var.clone()) {
                                        Variable::Number { value } => {
                                            iter = value + 1;
                                            self.assign_var(var.clone(), Variable::Number { value: iter });
                                        }
                                        _ => {
                                            panic!("Should not happen!");
                                        }
                                    }
                                }
                            } else if start_val_int > end_val_int {
                                let mut iter = start_val_int;

                                while iter >= end_val_int {
                                    self.statements(statements.borrow());
                                    match self.get_var(var.clone()) {
                                        Variable::Number { value } => {
                                            iter = value - 1;
                                            self.assign_var(var.clone(), Variable::Number { value: iter });
                                        }
                                        _ => {
                                            panic!("Should not happen!");
                                        }
                                    }
                                }
                            } else {
                                /*
                                start_val_int == end_val_int so no loops.
                                This needs to be it's separate branch since
                                in positive loop we match <= and in negative >=.
                                 */
                            }
                        }
                        Tree::Var { value, var_type, name } => {
                            let variable = self.evaluate(value.borrow());
                            // Check that var_type matches variable type
                            match variable.clone() {
                                Variable::Number { value: _ } => {
                                    if var_type == &VarType::Int {
                                        self.init_var(name.clone(), variable);
                                    } else {
                                        panic!("Variable type and expression evaluation didn't match!");
                                    }
                                }
                                Variable::Bool { value: _ } => {
                                    if var_type == &VarType::Bool {
                                        self.init_var(name.clone(), variable);
                                    } else {
                                        panic!("Variable type and expression evaluation didn't match!");
                                    }
                                }
                                Variable::String { value: _ } => {
                                    if var_type == &VarType::String {
                                        self.init_var(name.clone(), variable);
                                    } else {
                                        panic!("Variable type and expression evaluation didn't match!");
                                    }
                                }
                                Variable::Error => {
                                    panic!("Invalid expression evaluation!");
                                }
                            }
                        }
                        Tree::Print { value } => {
                            let e = self.evaluate(value.borrow());
                            match e {
                                Variable::Number { value } => {
                                    self.io.print(format!("{}", value.clone()));
                                }
                                Variable::String { value } => {
                                    self.io.print(format!("{}", value.clone()));
                                }
                                Variable::Bool { value } => {
                                    self.io.print(format!("{}", value.clone()));
                                }
                                _ => {
                                    panic!("Error while evaluating expression!");
                                }
                            }
                        }
                        Tree::Assert { value } => {
                            let e = self.evaluate(value.borrow());

                            match e {
                                Variable::Bool { value } => {
                                    println!("Assert: {}.", value);
                                }
                                _ => { panic!("Assert didn't receive expected type of boolean!"); }
                            }
                        }
                        Tree::Assign { var, value } => {
                            let e = self.evaluate(value.borrow());
                            let v = self.get_var(var.clone());
                            let t = (e.clone(), v);

                            match t {
                                (Variable::Bool { value: _v1 }, Variable::Bool { value: _v2 }) => {
                                    self.assign_var(var.clone(), e);
                                }
                                (Variable::String { value: _v1 }, Variable::String { value: _v2 }) => {
                                    self.assign_var(var.clone(), e);
                                }
                                (Variable::Number { value: _v1 }, Variable::Number { value: _v2 }) => {
                                    self.assign_var(var.clone(), e);
                                }
                                _ => {
                                    panic!("Mismatched type while assigning variable value.");
                                }
                            }
                        }
                        Tree::Read { var } => {
                            let v = self.get_var(var.clone());
                            let input = self.io.read();

                            match v {
                                Variable::String { value: _ } => {
                                    self.assign_var(var.clone(), Variable::String { value: input });
                                }
                                Variable::Number { value: _ } => {
                                    self.assign_var(var.clone(), Variable::Number { value: input.parse().unwrap() });
                                }
                                _ => { panic!("Unable to assign read variable."); }
                            }
                        }
                        _ => {
                            dbg!(stmt);
                            dbg!(self.program.clone());
                            panic!("Unexpected AST branch!");
                        }
                    }
                }
            }
            _ => {
                panic!("Expected statements block!");
            }
        }

        self.scopes.pop_back();
    }
}