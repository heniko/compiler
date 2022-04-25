use crate::parser::{Expression, Statement, VariableAccess, VariableType, AST};
use std::collections::HashMap;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Variable {
    String,
    Integer,
    Real,
    Boolean,
    StringArray,
    IntegerArray,
    RealArray,
    BooleanArray,
    Error,
    None,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Parameters {
    Any,
    // Used for default read and writeln
    List { parameters: Vec<Variable> },
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum IdType {
    SimpleType {
        var_type: Variable,
    },
    ArrayType {
        var_type: Variable,
    },
    TypeType {
        var_type: Variable,
    },
    SizeType,
    Function {
        parameters: Parameters,
        return_type: Variable,
    },
    Procedure {
        parameters: Parameters,
    },
    Error,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct SemanticAnalyzer {
    scope: Vec<HashMap<String, IdType>>,
    return_type: Variable,
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
                var_type: Variable::String,
            },
        );
        globals.insert(
            String::from("integer"),
            IdType::TypeType {
                var_type: Variable::Integer,
            },
        );
        globals.insert(
            String::from("real"),
            IdType::TypeType {
                var_type: Variable::Real,
            },
        );
        globals.insert(
            String::from("string"),
            IdType::TypeType {
                var_type: Variable::String,
            },
        );
        globals.insert(
            String::from("false"),
            IdType::SimpleType {
                var_type: Variable::Boolean,
            },
        );
        globals.insert(
            String::from("true"),
            IdType::SimpleType {
                var_type: Variable::Boolean,
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
            /*
            While parsing functions we need to check the type of
            the return statement. However, the return statement
            could be inside some other block like for example
            when we have if statement for early return etc.
            */
            return_type: Variable::None,
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
                for function in functions.iter() {
                    self.check_function(function);
                }

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
    Add id, parameters and return type to globals.
     */
    fn handle_function_declaration(&mut self, ast: &AST) {
        if let AST::Function {
            block: _,
            id,
            parameters,
            res_type,
        } = ast
        {
            let mut params = Vec::new();

            for p in parameters.iter() {
                let p_var = p.var_type.clone();
                params.push(match p_var {
                    VariableType::ArrayType { var_type, size: _ } => {
                        self.string_to_atomic_arr(&var_type)
                    }
                    VariableType::SimpleType { var_type } => self.string_to_atomic(&var_type),
                    _ => Variable::Error,
                })
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

    /*
    Add id and parameters to globals.
    */
    fn handle_procedure_declaration(&mut self, ast: &AST) {
        if let AST::Procedure {
            block: _,
            id,
            parameters,
        } = ast
        {
            let mut params = Vec::new();

            for p in parameters.iter() {
                let p_var = p.var_type.clone();
                params.push(match p_var {
                    VariableType::ArrayType { var_type, size: _ } => {
                        self.string_to_atomic_arr(&var_type)
                    }
                    VariableType::SimpleType { var_type } => self.string_to_atomic(&var_type),
                    _ => Variable::Error,
                })
            }

            self.init_var(
                id.clone(),
                IdType::Procedure {
                    parameters: Parameters::List { parameters: params },
                },
            )
        }
    }

    fn string_to_atomic(&self, s: &String) -> Variable {
        match s.as_str() {
            "string" => Variable::String,
            "integer" => Variable::Integer,
            "real" => Variable::Real,
            "boolean" => Variable::Boolean,
            _ => Variable::Error,
        }
    }

    fn string_to_atomic_arr(&self, s: &String) -> Variable {
        match s.as_str() {
            "string" => Variable::StringArray,
            "integer" => Variable::IntegerArray,
            "real" => Variable::RealArray,
            "boolean" => Variable::BooleanArray,
            _ => Variable::Error,
        }
    }
}

/*
Methods for semantic analyzing.
 */
impl SemanticAnalyzer {
    /*
    Entry point for checking function.
    */
    fn check_function(&mut self, ast: &AST) {
        if let AST::Function {
            block,
            id,
            parameters,
            res_type,
        } = ast
        {
            // Create first local layer to scope
            self.add_local_scope();
            // Add arguments to first local variable layer
            for parameter in parameters.iter() {
                let par_type = parameter.var_type.clone();

                let par_to_add = match par_type {
                    VariableType::SimpleType { var_type } => IdType::SimpleType {
                        var_type: self.string_to_atomic(&var_type),
                    },
                    VariableType::ArrayType { var_type, size } => {
                        /*
                        Size of parameter needs to be 'None' since this is a
                        reference to an array and not a declaration of a new one.
                        */
                        if size != Expression::None {
                            self.errors
                                .push(String::from("Array parameter size can't be predefined."));
                        }
                        IdType::ArrayType {
                            var_type: self.string_to_atomic(&var_type),
                        }
                    }
                    _ => {
                        self.errors
                            .push(String::from("Could not resolve parameter type."));
                        IdType::Error
                    }
                };

                self.init_var(id.clone(), par_to_add);
            }

            // Check statements
            self.return_type = self.string_to_atomic(res_type);
            self.check_statement(block);

            // Drop function scope
            self.drop_local_scope();
        }
    }

    fn check_statement(&mut self, statement: &Statement) {
        match statement {
            Statement::Return { value } => {
                let evaluated = self.evaluate(value);

                if evaluated != self.return_type {
                    self.errors
                        .push(String::from("Return type and returned type didn't match."));
                }
            }
            Statement::VariableDeclaration { variables } => {
                for variable in variables.iter() {
                    let t = match variable.var_type.clone() {
                        VariableType::SimpleType { var_type } => IdType::SimpleType {
                            var_type: self.string_to_atomic(&var_type),
                        },
                        VariableType::ArrayType { var_type, size } => {
                            // Check that array size is integer
                            if self.evaluate(&size) != Variable::Integer {
                                self.errors
                                    .push(String::from("Array size needs to be type integer."));
                            }

                            IdType::ArrayType {
                                var_type: self.string_to_atomic(&var_type),
                            }
                        }
                        _ => IdType::Error,
                    };

                    self.init_var(variable.id.clone(), t);
                }
            }
            Statement::Assignment { var, value } => {
                let eval = self.evaluate(value);

                match var {
                    VariableAccess::SimpleAccess { id } => {
                        let access = self.access_var(id.clone());

                        if let Some(t) = access {
                            if let IdType::SimpleType { var_type } = t {
                                if var_type != eval {
                                    self.errors.push(String::from(
                                        "Variable type and assignment evaluation missmatch.",
                                    ));
                                }
                            } else {
                                self.errors.push(String::from(
                                    "Variable type and assignment evaluation missmatch.",
                                ));
                            }
                        }
                    }
                    VariableAccess::ArrayAccess { id, index } => {
                        if self.evaluate(index) != Variable::Integer {
                            self.errors
                                .push(String::from("Array index needs to be integer."));
                        }

                        let access = self.access_var(id.clone());

                        if let Some(t) = access {
                            if let IdType::ArrayType { var_type } = t {
                                if var_type != eval {
                                    self.errors.push(String::from(
                                        "Variable type and assignment evaluation missmatch.",
                                    ));
                                }
                            } else {
                                self.errors.push(String::from(
                                    "Variable type and assignment evaluation missmatch.",
                                ));
                            }
                        }
                    }
                    _ => {
                        self.errors.push(String::from("Variable not in scope."));
                    }
                }
            }
            Statement::Block { statements } => {
                self.add_local_scope();
                for statement in statements.iter() {
                    self.check_statement(statement);
                }
                self.drop_local_scope();
            }
            _ => {}
        }
    }

    fn match_parameters_and_arguments(
        &mut self,
        params: &Parameters,
        args: &Vec<Expression>,
    ) -> bool {
        match params {
            Parameters::Any => {
                return true;
            }
            Parameters::List { parameters } => {
                if parameters.len() != args.len() {
                    return false;
                } else {
                    for (index, elem) in args.iter().enumerate() {
                        if self.evaluate(elem) != parameters[index] {
                            return false;
                        }
                    }
                    return true;
                }
            }
        }
    }
}

/*
Contains methods used for evaluating expressions.
*/
impl SemanticAnalyzer {
    /*
    Entry point for checking expression type.
    */
    fn evaluate(&mut self, expr: &Expression) -> Variable {
        match expr {
            Expression::None => Variable::None,
            Expression::RealLiteral { value: _ } => Variable::Real,
            Expression::StringLiteral { value: _ } => Variable::String,
            Expression::IntegerLiteral { value: _ } => Variable::Integer,
            Expression::Variable { var } => self.evaluate_id(var),
            Expression::Function {
                id: _,
                arguments: _,
            } => self.evaluate_function_call(expr),
            Expression::Unary { op: _, value: _ } => self.evaluate_unary(expr),
            Expression::Binary {
                op: _,
                left: _,
                right: _,
            } => self.evaluate_binary(expr),
            _ => {
                dbg!(expr.clone());
                Variable::Error
            }
        }
    }

    fn evaluate_unary(&mut self, expr: &Expression) -> Variable {
        if let Expression::Unary { op, value } = expr {
            let op_unboxed = op.as_ref().clone();
            let value_evaluated = self.evaluate(value.as_ref());
            match op_unboxed {
                Expression::Not => match value_evaluated {
                    Variable::Boolean => Variable::Boolean,
                    _ => Variable::Error,
                },
                Expression::Plus | Expression::Minus => match value_evaluated {
                    Variable::Real => Variable::Real,
                    Variable::Integer => Variable::Integer,
                    _ => Variable::Error,
                },
                _ => Variable::Error,
            }
        } else {
            Variable::Error
        }
    }

    fn evaluate_binary(&mut self, expr: &Expression) -> Variable {
        /*
        Strategy here is to recursively find the evaluation of
        right and left. Then for each operation we have some
        pairs that the operation can handle. For example you
        can add two strings but not multiply them. Dealing with
        real equality errors is left for the programmer to handle
        since our language allows such operations even if using
        them usually doesn't make much sense.
        */
        if let Expression::Binary { op, left, right } = expr {
            let op_unboxed = op.as_ref().clone();
            let left_evaluated = self.evaluate(left.as_ref());
            let right_evaluated = self.evaluate(right.as_ref());
            let tuple = (right_evaluated, left_evaluated);
            match op_unboxed {
                Expression::Plus => match tuple {
                    (Variable::Real, Variable::Real) => Variable::Real,
                    (Variable::Integer, Variable::Integer) => Variable::Integer,
                    (Variable::String, Variable::String) => Variable::String,
                    (Variable::Real, Variable::Integer) => Variable::Real,
                    _ => Variable::Error,
                },
                Expression::Minus | Expression::Multiply | Expression::Divide => match tuple {
                    (Variable::Real, Variable::Real) => Variable::Real,
                    (Variable::Integer, Variable::Integer) => Variable::Integer,
                    (Variable::Real, Variable::Integer) => Variable::Real,
                    (Variable::Integer, Variable::Real) => Variable::Real,
                    _ => Variable::Error,
                },
                Expression::Modulo => match tuple {
                    (Variable::Integer, Variable::Integer) => Variable::Integer,
                    _ => Variable::Error,
                },
                Expression::Or | Expression::And => match tuple {
                    (Variable::Boolean, Variable::Boolean) => Variable::Boolean,
                    _ => Variable::Error,
                },
                Expression::Le | Expression::Leq | Expression::Ge | Expression::Geq => {
                    match tuple {
                        (Variable::Real, Variable::Real) => Variable::Boolean,
                        (Variable::Integer, Variable::Integer) => Variable::Boolean,
                        (Variable::Real, Variable::Integer) => Variable::Boolean,
                        (Variable::Integer, Variable::Real) => Variable::Boolean,
                        _ => Variable::Error,
                    }
                }
                Expression::Inequality | Expression::Eq => match tuple {
                    (Variable::Real, Variable::Real) => Variable::Boolean,
                    (Variable::Integer, Variable::Integer) => Variable::Boolean,
                    (Variable::Real, Variable::Integer) => Variable::Boolean,
                    (Variable::Integer, Variable::Real) => Variable::Boolean,
                    (Variable::Boolean, Variable::Boolean) => Variable::Boolean,
                    _ => Variable::Error,
                },
                _ => Variable::Error,
            }
        } else {
            Variable::Error
        }
    }

    fn evaluate_function_call(&mut self, expr: &Expression) -> Variable {
        if let Expression::Function { arguments, id } = expr {
            let stored = self.access_var(id.clone());

            return if let Some(IdType::Function {
                parameters,
                return_type,
            }) = stored
            {
                self.match_parameters_and_arguments(&parameters, &arguments);

                return_type.clone()
            } else {
                Variable::Error
            };
        } else {
            Variable::Error
        }
    }

    fn evaluate_id(&mut self, var: &VariableAccess) -> Variable {
        return match var {
            VariableAccess::SimpleAccess { id } => {
                let stored = self.access_var(id.clone());

                if let Some(v) = stored {
                    match v {
                        IdType::SimpleType { var_type } => var_type.clone(),
                        _ => Variable::Error,
                    }
                } else {
                    Variable::Error
                }
            }
            VariableAccess::ArrayAccess { id, index } => {
                let stored = self.access_var(id.clone());

                if self.evaluate(index) != Variable::Integer {
                    self.errors
                        .push(String::from("Array index needs to be integer."));
                }

                if let Some(v) = stored {
                    match v {
                        IdType::ArrayType { var_type } => var_type.clone(),
                        _ => Variable::Error,
                    }
                } else {
                    Variable::Error
                }
            }
            _ => Variable::Error,
        };
    }
}
