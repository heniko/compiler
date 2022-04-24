use crate::parser::{Expression, VariableAccess, VariableDeclaration, VariableType, AST};
use std::borrow::Borrow;
use std::collections::HashMap;
use std::hash::Hash;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Atomic {
    String,
    Integer,
    Real,
    Boolean,
    Error,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Parameters {
    Any,
    // Used for default read and writeln
    List { parameters: Vec<IdType> },
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum IdType {
    SimpleType {
        var_type: Atomic,
    },
    ArrayType {
        var_type: Atomic,
    },
    TypeType {
        var_type: Atomic,
    },
    SizeType,
    Function {
        parameters: Parameters,
        return_type: Atomic,
    },
    Procedure {
        parameters: Parameters,
    },
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct SemanticAnalyzer {
    scope: Vec<HashMap<String, IdType>>,
    pub errors: Vec<String>,
}

/*
Contains all the methods and associated functions
that will be used in the semantic analyzer implementation.
 */
impl SemanticAnalyzer {
    pub fn from(ast: AST) -> SemanticAnalyzer {
        let mut globals: HashMap<String, IdType> = HashMap::new();

        // Add all predefined globals identifiers
        globals.insert(
            String::from("boolean"),
            IdType::TypeType {
                var_type: Atomic::String,
            },
        );
        globals.insert(
            String::from("integer"),
            IdType::TypeType {
                var_type: Atomic::Integer,
            },
        );
        globals.insert(
            String::from("real"),
            IdType::TypeType {
                var_type: Atomic::Real,
            },
        );
        globals.insert(
            String::from("string"),
            IdType::TypeType {
                var_type: Atomic::String,
            },
        );
        globals.insert(
            String::from("false"),
            IdType::SimpleType {
                var_type: Atomic::Boolean,
            },
        );
        globals.insert(
            String::from("true"),
            IdType::SimpleType {
                var_type: Atomic::Boolean,
            },
        );
        globals.insert(
            String::from("read"),
            IdType::Procedure {
                parameters: Parameters::Any,
            },
        );
        globals.insert(
            String::from("writeln"),
            IdType::Procedure {
                parameters: Parameters::Any,
            },
        );
        globals.insert(String::from("size"), IdType::SizeType);

        let mut res = SemanticAnalyzer {
            scope: vec![globals],
            errors: Vec::new(),
        };
        res.check(&ast.clone());
        res
    }

    /*
    Entry point for the semantic analyzer.
     */
    fn check(&mut self, ast: &AST) {
        match ast {
            AST::Program {
                id: _,
                functions,
                procedures,
                main: _,
            } => {
                /*
                Add functions and procedures to global scope.
                */
                for function in functions.iter() {
                    self.handle_function_declaration(function);
                }

                for procedure in procedures.iter() {
                    self.handle_procedure_declaration(procedure);
                }

                /*
                Do semantic analysis on the code of the procedures
                and methods.
                */

                /*
                Do the semantic analysis of main-block.
                */
            }
            _ => {
                self.errors
                    .push(String::from("AST does not contain program."));
            }
        }
    }

    /*
    Adds new layer to current local scope. This happens
    for example when a variable is created inside a loop
    and it then needs to be dropped after the loop iteration
    ends.
     */
    fn add_local_scope(&mut self) {
        self.scope.push(HashMap::new());
    }

    /*
    Drops a layer from the current local scope. For example
    loops will add new layer to the local scope that then
    needs to be dropped after the current iteration ends.
     */
    fn drop_local_scope(&mut self) {
        self.scope.pop();
    }

    /*
    'Initialize' (add type to current scope and layer) for some id.
     */
    fn init_var(&mut self, id: String, var_type: IdType) {
        self.scope
            .last_mut() // Last local scope
            .unwrap()
            .insert(id, var_type);
    }

    /*
    'Access value' (resolve type) of the id. Look order:
    Most recent local scope layer -> Least recent local scope layer, Global scope
     */
    fn access_var(&self, id: String) -> Option<IdType> {
        /*
        Search scopes starting from most recently added (For example the scope
        of block after if statement) to least recently added (Global scope)
        and return the type of first match.
        */
        for layer in self.scope.iter().rev() {
            if layer.contains_key(&id) {
                return Some(layer.get(&id).unwrap().clone());
            }
        }

        // Not in scope
        None
    }

    /*
     */
    fn handle_function_declaration(&mut self, ast: &AST) {
        if let AST::Function {
            block: _,
            id,
            parameters,
            res_type,
        } = ast
        {
            let mut params: Vec<IdType> = Vec::new();

            for p in parameters.iter() {
                let p_var = p.var_type.clone();
                match p_var {
                    VariableType::ArrayType { var_type, size: _ } => {
                        params.push(IdType::ArrayType {
                            var_type: self.string_to_atomic(&var_type),
                        })
                    }
                    VariableType::SimpleType { var_type } => params.push(IdType::SimpleType {
                        var_type: self.string_to_atomic(&var_type),
                    }),
                    _ => {}
                }
            }

            self.init_var(
                id.clone(),
                IdType::Function {
                    parameters: Parameters::List { parameters: params },
                    return_type: self.string_to_atomic(res_type),
                },
            )
        }
    }

    fn handle_procedure_declaration(&mut self, ast: &AST) {
        if let AST::Procedure {
            block: _,
            id,
            parameters,
        } = ast
        {
            let mut params: Vec<IdType> = Vec::new();

            for p in parameters.iter() {
                let p_var = p.var_type.clone();
                match p_var {
                    VariableType::ArrayType { var_type, size: _ } => {
                        params.push(IdType::ArrayType {
                            var_type: self.string_to_atomic(&var_type),
                        })
                    }
                    VariableType::SimpleType { var_type } => params.push(IdType::SimpleType {
                        var_type: self.string_to_atomic(&var_type),
                    }),
                    _ => {}
                }
            }

            self.init_var(
                id.clone(),
                IdType::Procedure {
                    parameters: Parameters::List { parameters: params },
                },
            )
        }
    }

    fn string_to_atomic(&self, s: &String) -> Atomic {
        match s.as_str() {
            "string" => Atomic::String,
            "integer" => Atomic::Integer,
            "real" => Atomic::Real,
            "boolean" => Atomic::Boolean,
            _ => Atomic::Error,
        }
    }
}

/*
Methods for semantic analyzing.
 */
impl SemanticAnalyzer {}

/*impl SemanticAnalyzer {

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

 */
