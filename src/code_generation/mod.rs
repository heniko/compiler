use crate::parser::{
    Expression, Statement, VariableAccess, VariableDeclaration, VariableType, AST,
};
use crate::semantic_analysis::{IdType, Parameters, Scope, Variable};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct CodeGenerator {
    pub source: String,
    pub var_count: i32,
    scope: Scope,
}

impl CodeGenerator {
    pub fn from(ast: AST, scope: Scope) -> CodeGenerator {
        let mut code_gen = CodeGenerator {
            source: String::new(),
            var_count: 0,
            scope,
        };
        code_gen.generate(&ast);
        code_gen
    }

    fn add_line(&mut self, line: String) {
        self.source.push_str(line.as_str());
        self.source.push_str("\n");
    }

    fn generate(&mut self, ast: &AST) {
        if let AST::Program {
            id: _,
            functions,
            procedures,
            main,
        } = ast
        {
            // Add stdio.h for printf and scanf
            self.add_line("#include <stdio.h>".to_string());
            // Add stdbool.h for dealing with booleans
            self.add_line("#include <stdbool.h>".to_string());
            self.add_line("".to_string());
            /*
            TODO: user_true and user_false can be overwritten
            */
            self.add_line("bool user_true = true;".to_string());
            self.add_line("bool user_false = false;".to_string());
            self.add_line("".to_string());
            self.create_forward_declarations(ast);
            self.generate_procedures(procedures);
            self.generate_functions(functions);
            // Generate main block
            self.add_line("".to_string());
            self.generate_main(main);
        }
    }

    fn generate_var(&mut self) -> String {
        self.var_count += 1;
        format!("var_{}", self.var_count)
    }

    fn get_latest_var(&mut self) -> String {
        format!("var_{}", self.var_count)
    }
}

impl CodeGenerator {
    /*
    Since our language supports calling functions declared later
    in the source file and C doesn't we need to do forward
    declaration of the functions so they can be used properly.
    */
    fn create_forward_declarations(&mut self, ast: &AST) {
        if let AST::Program {
            id: _,
            functions,
            procedures,
            main: _,
        } = ast
        {
            // Function forward declarations
            for function in functions.iter() {
                if let AST::Function {
                    id,
                    parameters,
                    block: _,
                    res_type,
                } = function
                {
                    self.add_line(format!(
                        "{} {}({});",
                        to_c_type(res_type),
                        id,
                        to_c_parameters(parameters)
                    ));
                }
            }

            // Procedure forward declarations
            for procedure in procedures.iter() {
                if let AST::Procedure {
                    id,
                    block: _,
                    parameters,
                } = procedure
                {
                    self.add_line(format!("void {}({});", id, to_c_parameters(parameters)));
                }
            }
        }
    }

    fn generate_procedures(&mut self, procedures: &Vec<AST>) {
        for procedure in procedures.iter() {
            self.add_line("".to_string());
            self.generate_procedure(procedure);
        }
    }

    fn generate_functions(&mut self, functions: &Vec<AST>) {
        for function in functions.iter() {
            self.add_line("".to_string());
            self.generate_function(function);
        }
    }

    fn generate_main(&mut self, main: &Statement) {
        // Add local scope for main function
        self.scope.add_local_scope();
        // Create main function
        self.add_line("int main(){".to_string());
        // Generate code for statements
        self.statement(main);
        // Jump point for early return statements
        self.add_line("end:;".to_string());
        // Return 0 as is expected by C (but not by MiniPL)
        self.add_line("return 0;".to_string());
        self.add_line("}".to_string());
        // Drop local scope
        self.scope.drop_local_scope();
    }

    fn generate_procedure(&mut self, procedure: &AST) {
        if let AST::Procedure {
            id,
            parameters,
            block,
        } = procedure
        {
            self.scope.add_local_scope();
            self.add_line(format!("void {}({}) {{", id, to_c_parameters(parameters)));
            self.create_parameter_derefs(parameters);
            self.statement(block);
            self.add_line("end:;".to_string());
            self.move_values_to_parameter_pointer(parameters);
            self.add_line("}".to_string());
            self.scope.drop_local_scope();
        }
    }

    fn generate_function(&mut self, function: &AST) {
        if let AST::Function {
            id,
            parameters,
            block,
            res_type,
        } = function
        {
            self.scope.add_local_scope();
            self.add_line(format!(
                "{} {}({}) {{",
                to_c_type(res_type),
                id,
                to_c_parameters(parameters)
            ));
            self.add_line(format!("{} return_value;", to_c_type(res_type)));
            self.create_parameter_derefs(parameters);
            self.statement(block);
            self.add_line("end:;".to_string());
            self.move_values_to_parameter_pointer(parameters);
            self.add_line("return return_value;".to_string());
            self.add_line("}".to_string());
            self.scope.drop_local_scope();
        }
    }

    fn create_parameter_derefs(&mut self, params: &Vec<VariableDeclaration>) {
        /*
        This function creates derefenced copy variables
        of function parameters. This is so we don't have
        to track if the variable is a pointer or normal value
        type in expressions when accessing them. So in short,
        this makes accessing variables easier to handle.
        */
        for param in params.iter() {
            let t = param.var_type.clone();
            match t {
                VariableType::SimpleType { var_type } => {
                    self.add_line(format!(
                        "{} {} = *ptr_{};",
                        to_c_type(&var_type),
                        param.id,
                        param.id
                    ));
                    self.scope.init_var(
                        param.id.clone(),
                        IdType::SimpleType {
                            var_type: self.scope.string_to_atomic(&var_type),
                        },
                    );
                }
                VariableType::ArrayType { var_type, size: _ } => {
                    /*self.add_line(format!(
                        "{} {}[] = *ptr_{};",
                        to_c_type(&var_type),
                        param.id,
                        param.id
                    ));*/
                    self.scope.init_var(
                        param.id.clone(),
                        IdType::ArrayType {
                            var_type: self.scope.string_to_atomic(&var_type),
                        },
                    );
                }
                _ => {}
            }
        }
    }

    fn move_values_to_parameter_pointer(&mut self, params: &Vec<VariableDeclaration>) {
        /*
        This is the 'opposite' function of create_parameter_derefs().
        Since user may change the value of parameters
        the changed values need to be stored back to
        pointer locations at the end.
        */
        for param in params.iter() {
            let t = param.var_type.clone();
            match t {
                VariableType::SimpleType { var_type: _ } => {
                    self.add_line(format!("*ptr_{} = {};", param.id, param.id));
                }
                /*VariableType::ArrayType {
                    var_type: _,
                    size: _,
                } => {
                    self.add_line(format!("*ptr_{} = {};", param.id, param.id));
                }*/
                _ => {}
            }
        }
    }

    fn statement(&mut self, stmt: &Statement) {
        // Match statement type and call correct handler
        match stmt {
            Statement::Block { statements: _ } => self.block(stmt),
            Statement::VariableDeclaration { variables: _ } => self.variable_declaration(stmt),
            Statement::Assignment { var: _, value: _ } => self.assignment(stmt),
            Statement::If {
                value: _,
                statement: _,
            } => self.if_statement(stmt),
            Statement::IfElse {
                value: _,
                if_statement: _,
                else_statement: _,
            } => self.if_else_statement(stmt),
            Statement::While {
                value: _,
                statement: _,
            } => self.while_statement(stmt),
            Statement::Return { value: _ } => self.return_statement(stmt),
            Statement::Call {
                id: _,
                arguments: _,
            } => self.call_statement(stmt),
            _ => {}
        }
    }

    fn assignment(&mut self, stmt: &Statement) {
        if let Statement::Assignment { var, value } = stmt {
            // Check if we are accessing normal variable or an array
            match var {
                VariableAccess::SimpleAccess { id } => {
                    self.expression(id, value);
                }
                VariableAccess::ArrayAccess { id, index } => {
                    // Evaluate index
                    let i = self.generate_var();
                    self.add_line(format!("int {};", i));
                    self.expression(&i, index.as_ref());
                    // Evaluate value expression and assign it to evaluated array index
                    self.expression(&format!("{}[{}]", id, i), value);
                }
                _ => {}
            }
        }
    }

    fn block(&mut self, stmt: &Statement) {
        // Iteratively generate code for all statements in block
        if let Statement::Block { statements } = stmt {
            for statement in statements.iter() {
                self.statement(statement);
            }
        }
    }

    fn variable_declaration(&mut self, stmt: &Statement) {
        // Check that statement is variable declaration
        if let Statement::VariableDeclaration { variables } = stmt {
            // Iterate over all declarations since there may be many
            for dec in variables.iter() {
                let var_type = dec.var_type.clone();
                // Check if we are handling simple or array type
                match var_type {
                    VariableType::SimpleType { var_type } => {
                        self.add_line(format!("{} {};", to_c_type(&var_type), dec.id.clone()));
                        self.scope.init_var(
                            dec.id.clone(),
                            IdType::SimpleType {
                                var_type: self.scope.string_to_atomic(&var_type),
                            },
                        );
                    }
                    VariableType::ArrayType { var_type, size } => {
                        let size_var = format!("size_{}", dec.id.clone());
                        self.add_line(format!("int {};", size_var));
                        self.expression(&size_var, &size);

                        self.add_line(format!(
                            "{} {}[{}];",
                            to_c_type(&var_type),
                            dec.id.clone(),
                            size_var
                        ));

                        self.scope.init_var(
                            dec.id.clone(),
                            IdType::ArrayType {
                                var_type: self.scope.string_to_atomic(&var_type),
                            },
                        );
                    }
                    _ => {}
                }
            }
        }
    }

    fn if_statement(&mut self, stmt: &Statement) {
        if let Statement::If { value, statement } = stmt {
            /*
            Generate boolean variable for the condition, evaluate
            condition expression and then skip generated statements
            if the condition is false.
            */
            let condition = self.generate_var();
            self.add_line(format!("bool {};", condition));
            self.expression(&condition, value);
            let end = self.generate_var();
            self.add_line(format!("if (!{}) goto {};", condition, end));
            self.statement(statement.as_ref());
            self.add_line(format!("{}:;", end));
        }
    }

    fn if_else_statement(&mut self, stmt: &Statement) {
        if let Statement::IfElse {
            value,
            if_statement,
            else_statement,
        } = stmt
        {
            // Evaluate condition
            let condition = self.generate_var();
            self.add_line(format!("bool {};", condition));
            self.expression(&condition, value);
            // Create jump point variables
            let els = self.generate_var();
            let end = self.generate_var();
            // If true -> continue here, if false -> jump to else block
            self.add_line(format!("if (!{}) goto {};", condition, els));
            // Generate code for if block and skip else code
            self.statement(if_statement);
            self.add_line(format!("goto {};", end));
            // Generate code for else
            self.add_line(format!("{}:;", els));
            self.statement(else_statement);
            // Add end jump point for exiting if block
            self.add_line(format!("{}:;", end));
        }
    }

    fn while_statement(&mut self, stmt: &Statement) {
        if let Statement::While { value, statement } = stmt {
            // Create variables for start and end jump points
            let end = self.generate_var();
            let start = self.generate_var();
            // Init condition variable
            let condition = self.generate_var();
            self.add_line(format!("bool {};", condition));
            // Jump point for loop start
            self.add_line(format!("{}:;", start));
            // Evaluate condition
            self.expression(&condition, value);
            // If condition is false then jump out
            self.add_line(format!("if (!{}) goto {};", condition, end));
            // Statement that will be executed in loop
            self.statement(statement);
            // Jump back to beginning of the loop
            self.add_line(format!("goto {};", start));
            // Jump point for exiting loop
            self.add_line(format!("{}:;", end));
        }
    }

    fn return_statement(&mut self, stmt: &Statement) {
        if let Statement::Return { value } = stmt {
            if let Expression::None = value {
                // Either procedure or main which has no return value in MiniPL
                self.add_line("goto end;".to_string());
            } else {
                // Function return statement
                self.expression(&"return_value".to_string(), value);
                self.add_line("goto end;".to_string());
            }
        }
    }

    fn call_statement(&mut self, stmt: &Statement) {
        /*
        For normal functions resolve arguments and create function call.
        For writeln and read we need to check what they mean in the scope
        and if they are not overwritten we need to create prints using
        multiple C printf() or scanf() calls.
        */
        if let Statement::Call { id, arguments } = stmt {
            if id.as_str() == "user_writeln" {
                let print = self.scope.access_var(id.clone()).unwrap();
                if let IdType::Procedure {
                    parameters: Parameters::Any,
                } = print
                {
                    // Call identifier is 'writeln' and 'writeln' is not overwritten
                    for arg in arguments.iter() {
                        self.c_printf(arg);
                    }
                    self.add_line("printf(\"\\n\");".to_string()); // Line change in the end
                    return; // Early return to escape default case
                }
            } else if id.as_str() == "user_read" {
                let read = self.scope.access_var(id.clone()).unwrap();
                if let IdType::Procedure {
                    parameters: Parameters::Any,
                } = read
                {
                    // Call identifier is 'read' and 'read' is not overwritten
                    for arg in arguments.iter() {
                        self.c_scanf(arg);
                    }
                    return; // Early return to escape default case
                }
            }
            // If predefined functions were overwritten or if the
            // function name is not a predefined identifier we end
            // up here (default case).
            let args = self.resolve_arguments(arguments);
            self.add_line(format!("{}({});", id, args));
        }
    }

    fn c_printf(&mut self, expr: &Expression) {
        let t = self.scope.evaluate(expr);
        let output = self.generate_var();
        self.add_line(format!("{} {};", variable_to_c_type(t.clone()), output));
        self.expression(&output, expr);

        match t {
            Variable::Boolean => {
                self.add_line(format!("printf(\"%s\", {}?\"true\":\"false\");", output));
            }
            Variable::Integer => {
                self.add_line(format!("printf(\"%d\", {});", output));
            }
            Variable::Real => {
                self.add_line(format!("printf(\"%f\", {});", output));
            }
            Variable::String => {
                self.add_line(format!("printf(\"%s\", {});", output));
            }
            _ => {}
        }
    }

    fn c_scanf(&mut self, expr: &Expression) {
        todo!();
    }
}

// Functions for handling expressions
impl CodeGenerator {
    fn expression(&mut self, id: &String, expr: &Expression) {
        // Solve expression with expression_recursion()
        self.expression_recursion(expr);
        // Get temporary variable name where value is in
        let latest_var = self.get_latest_var();
        // Store the value from temporary variable
        self.add_line(format!("{} = {};", id, latest_var));
    }

    fn expression_recursion(&mut self, expr: &Expression) {
        match expr {
            Expression::IntegerLiteral { value } => {
                let v = self.generate_var();
                self.add_line(format!("int {} = {};", v, value));
                self.scope.init_var(
                    v,
                    IdType::SimpleType {
                        var_type: Variable::Integer,
                    },
                );
            }
            Expression::RealLiteral { value } => {
                let v = self.generate_var();
                self.add_line(format!("float {} = {};", v, value));
                self.scope.init_var(
                    v,
                    IdType::SimpleType {
                        var_type: Variable::Real,
                    },
                );
            }
            Expression::StringLiteral { value } => {
                let v = self.generate_var();
                self.add_line(format!("char* {} = \"{}\";", v, value));
                self.scope.init_var(
                    v,
                    IdType::SimpleType {
                        var_type: Variable::String,
                    },
                );
            }
            Expression::Variable { var } => {
                self.variable_access(var);
            }
            Expression::Unary { op, value } => {
                self.expression_recursion(value);
                let l = self.get_latest_var();
                let v = self.generate_var();
                self.add_line(format!("{} = {} {};", v, l, to_c_operator(op.as_ref())));
            }
            Expression::Binary { op, left, right } => {
                let t = variable_to_c_type(self.scope.evaluate(expr));
                // Evaluate left
                self.expression_recursion(left);
                let l = self.get_latest_var();
                // Evaluate right
                self.expression_recursion(right);
                let r = self.get_latest_var();
                // Evaluate expression
                let v = self.generate_var();
                self.add_line(format!(
                    "{} {} = {} {} {};",
                    t,
                    v,
                    l,
                    to_c_operator(op.as_ref()),
                    r
                ));
            }
            Expression::Function { id, arguments } => {
                let func = self.scope.access_var(id.clone()).unwrap();

                let t = match func {
                    IdType::Function {
                        parameters: _,
                        return_type,
                    } => variable_to_c_type(return_type),
                    _ => "UNKNOWN_FUNC_TYPE".to_string(),
                };

                let v = self.generate_var();
                let args = self.resolve_arguments(arguments);
                self.add_line(format!("{} {} = {}({});", t, v, id, args));
            }
            _ => {}
        }
    }

    fn resolve_arguments(&mut self, args: &Vec<Expression>) -> String {
        let mut arguments = String::new();

        for (index, element) in args.iter().enumerate() {
            if index > 0 {
                arguments.push(',');
            }

            if let Expression::Variable { var } = element {
                // For simple and array variable access we want
                // to pass reference to the variable as an
                // argument since procedure or function may
                // want to change the value.
                match var {
                    VariableAccess::SimpleAccess { id } => {
                        arguments.push_str(format!("&{}", id).as_str());
                    }
                    VariableAccess::ArrayAccess { id, index } => {
                        let i = self.generate_var();
                        self.add_line(format!("int {};", i));
                        self.expression(&i, index);
                        arguments.push_str(format!("&{}[{}]", id, i).as_str());
                    }
                    VariableAccess::SizeAccess { id: _ } => {
                        let s = self.generate_var();
                        self.add_line(format!("int {};", s));
                        self.expression(&s, element);
                        arguments.push_str(format!("&{}", s).as_str());
                    }
                    _ => {}
                }
            } else {
                // Evaluate expression to tmp variable and pass
                // reference of it as an argument
                let v = self.generate_var();
                let t = variable_to_c_type(self.scope.evaluate(element));
                self.add_line(format!("{} {};", t, v));
                self.expression(&v, element);
                arguments.push_str(format!("&{}", v).as_str());
            }
        }

        arguments
    }

    fn variable_access(&mut self, var: &VariableAccess) {
        match var {
            VariableAccess::SizeAccess { id } => {
                let v = self.generate_var();
                self.add_line(format!("int {} = size_{};", v, id));
            }
            VariableAccess::SimpleAccess { id } => {
                let t = self.scope.access_var(id.clone()).unwrap();
                let v = self.generate_var();
                if let IdType::SimpleType { var_type } = t {
                    match var_type {
                        Variable::Integer => {
                            self.add_line(format!("int {} = {};", v, id));
                        }
                        Variable::Boolean => {
                            self.add_line(format!("bool {} = {};", v, id));
                        }
                        Variable::Real => {
                            self.add_line(format!("float {} = {};", v, id));
                        }
                        Variable::String => {
                            self.add_line(format!("char* {} = {};", v, id));
                        }
                        _ => {}
                    }
                }
            }
            VariableAccess::ArrayAccess { id, index } => {
                let i = self.generate_var();
                self.add_line(format!("int {};", i));
                self.expression(&i, index.as_ref());
                let t = self.scope.access_var(id.clone()).unwrap();
                let v = self.generate_var();
                if let IdType::ArrayType { var_type } = t {
                    match var_type {
                        Variable::Integer => {
                            self.add_line(format!("int {} = {}[{}];", v, id, i));
                        }
                        Variable::Boolean => {
                            self.add_line(format!("bool {} = {}[{}];", v, id, i));
                        }
                        Variable::Real => {
                            self.add_line(format!("float {} = {}[{}];", v, id, i));
                        }
                        Variable::String => {
                            self.add_line(format!("char* {} = {}[{}];", v, id, i));
                        }
                        _ => {}
                    }
                }
            }
            _ => {}
        }
    }
}

fn variable_to_c_type(v: Variable) -> String {
    match v {
        Variable::Boolean => "bool".to_string(),
        Variable::Integer => "int".to_string(),
        Variable::Real => "float".to_string(),
        Variable::String => "char*".to_string(),
        _ => format!("{:?}", v),
    }
}

fn to_c_type(s: &String) -> String {
    match s.as_str() {
        "user_integer" => "int".to_string(),
        "user_real" => "float".to_string(),
        "user_boolean" => "bool".to_string(),
        "user_string" => "char*".to_string(),
        _ => "INVALID_TYPE".to_string(),
    }
}

fn to_c_parameters(params: &Vec<VariableDeclaration>) -> String {
    let mut res = String::new();

    for (index, param) in params.iter().enumerate() {
        // Separate variables with ','
        if index > 0 {
            res.push(',');
        }

        match param.var_type.clone() {
            VariableType::SimpleType { var_type } => {
                res.push_str(
                    format!("{}* ptr_{}", to_c_type(&var_type), param.id.clone()).as_str(),
                );
            }
            VariableType::ArrayType { var_type, size: _ } => {
                res.push_str(
                    format!(
                        "{} {}[], int size_{}",
                        to_c_type(&var_type),
                        param.id.clone().as_str(),
                        param.id.clone()
                    )
                    .as_str(),
                );
            }
            _ => {}
        }
    }

    res
}

fn to_c_operator(op: &Expression) -> String {
    return match op {
        Expression::And => "&&".to_string(),
        Expression::Or => "||".to_string(),
        Expression::Not => "!".to_string(),
        Expression::Le => "<".to_string(),
        Expression::Ge => ">".to_string(),
        Expression::Leq => "<=".to_string(),
        Expression::Geq => ">=".to_string(),
        Expression::Eq => "==".to_string(),
        Expression::Inequality => "!=".to_string(),
        Expression::Minus => "-".to_string(),
        Expression::Plus => "+".to_string(),
        Expression::Divide => "/".to_string(),
        Expression::Multiply => "*".to_string(),
        Expression::Modulo => "%".to_string(),
        _ => "NOT_AN_OPERATOR".to_string(),
    };
}
