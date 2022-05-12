use crate::parser::{
    Expression, Statement, VariableAccess, VariableDeclaration, VariableType, AST
};
use crate::semantic_analysis::{Scope, IdType, Variable};

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
            scope
        };
        code_gen.generate(&ast);
        code_gen
    }

    fn add_line(&mut self, line: String) {
        self.source.push_str(line.as_str());
        self.source.push_str("\n");
    }

    fn generate(&mut self, ast: &AST) {
        // Add stdio.h for printf and scanf
        self.add_line("#include <stdio.h>".to_string());
        self.add_line("#include <stdbool.h>".to_string());
        self.add_line("".to_string());
        /*
        TODO: user_true and user_false can be overwritten
        */
        self.add_line("bool user_true = true;".to_string());
        self.add_line("bool user_false = false;".to_string());
        self.add_line("".to_string());
        self.create_forward_declarations(ast);
        self.add_line("".to_string());
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
            id,
            functions,
            procedures,
            main,
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

            // Generate main block
            self.add_line("".to_string());
            self.generate_main(main);
        }
    }

    fn generate_main(&mut self, main: &Statement) {
        self.scope.add_local_scope();
        self.add_line("int main(){".to_string());

        if let Statement::Block { statements } = main {
            for stmt in statements.iter(){
                self.statement(stmt);
            }
        }

        self.add_line("return 0;".to_string());
        self.add_line("}".to_string());
        self.scope.drop_local_scope();
    }

    fn statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Block { statements:_ }=>self.block(stmt),
            Statement::VariableDeclaration { variables:_ } => self.variable_declaration(stmt),
            Statement::Assignment { var:_, value:_ } => self.assignment(stmt),
            Statement::If { value:_, statement:_ } => self.if_statement(stmt),
            Statement::IfElse { value:_ , if_statement: _, else_statement: _ } => self.if_else_statement(stmt),
            Statement::While { value:_, statement: _ } => self.while_statement(stmt),
            _=>{}
        }
    }

    fn assignment(&mut self, stmt:&Statement){
        if let Statement::Assignment { var, value } = stmt {
            match var {
                VariableAccess::SimpleAccess { id } => {
                    self.expression(id, value);
                }
                VariableAccess::ArrayAccess { id, index } => {
                    let i = self.generate_var();
                    self.add_line(format!("int {};", i));
                    self.expression(&i, index.as_ref());
                    self.expression(&format!("{}[{}]", id, i), value);
                }
                _=>{}
            }
            
        }
    }

    fn block(&mut self, stmt: &Statement){
        self.scope.add_local_scope();
        self.add_line("{".to_string());
        
        self.add_line("}".to_string());
        self.scope.drop_local_scope();
    }

    fn variable_declaration(&mut self, stmt:&Statement) {
        // Check that statement is variable declaration
        if let Statement::VariableDeclaration { variables } = stmt {
            // Iterate over all declarations since there may be many
            for dec in variables.iter() {
                let var_type = dec.var_type.clone();
                // Check if we are handling simple or array type
                match var_type {
                    VariableType::SimpleType { var_type } => {
                        self.add_line(format!("{} {};", to_c_type(&var_type), dec.id.clone()));
                        self.scope.init_var(dec.id.clone(), IdType::SimpleType{var_type:self.scope.string_to_atomic(&var_type)});
                    }
                    VariableType::ArrayType { var_type, size}=>{
                        let size_var = self.generate_var();
                        self.add_line(format!("int {};", size_var));
                        self.expression(&size_var, &size);

                        match var_type.as_str() {
                            "string" => {
                                self.add_line(format!("{} {}[{}];", to_c_type(&var_type), dec.id.clone(), size_var));
                            }
                            _=>{
                                self.add_line(format!("{} {}[{}];", to_c_type(&var_type), dec.id.clone(), size_var));
                            }
                        }

                        self.scope.init_var(dec.id.clone(), IdType::ArrayType{var_type:self.scope.string_to_atomic(&var_type)});
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
        if let Statement::IfElse { value, if_statement, else_statement } = stmt {
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

    fn while_statement(&mut self, stmt:&Statement) {
        if let Statement::While {value, statement} = stmt {
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
                self.scope.init_var(v, IdType::SimpleType { var_type: Variable::Integer });
            }
            Expression::RealLiteral { value } => {
                let v = self.generate_var();
                self.add_line(format!("float {} = {};",v,value));
                self.scope.init_var(v, IdType::SimpleType { var_type: Variable::Real });
            }
            Expression::StringLiteral { value }=>{
                let v = self.generate_var();
                self.add_line(format!("char* {} = \"{}\";", v, value));
                self.scope.init_var(v, IdType::SimpleType { var_type: Variable::String });
            }
            Expression::Variable { var } => {
                self.variable_access(var);
            }
            Expression::Unary { op, value } => {
                self.expression_recursion(value);
                let l = self.get_latest_var();
                let v = self.generate_var();
                match op.as_ref() {
                    Expression::Not => {
                        self.add_line(format!("{} = !{};", v, l));
                    }
                    Expression::Minus => {
                        todo!();
                    }
                    _=>{}
                }
            }
            Expression::Binary { op, left, right } => {
                todo!();
            }
            Expression::Function { id, arguments } => {
                todo!();
            }
            _=>{}
        }
    }

    fn variable_access(&mut self, var: &VariableAccess) {
        match var {
            VariableAccess::SizeAccess { id } => {
                let v = self.generate_var();
                self.add_line(format!("int {} = sizeof({}) / sizeof({}[0]);", v, id, id));
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
            _=>{}
        }
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
                res.push_str(format!("{}* {}", to_c_type(&var_type), param.id.clone()).as_str());
            }
            VariableType::ArrayType { var_type, size: _ } => {
                res.push_str(format!("{}* {}[]", to_c_type(&var_type), param.id.clone()).as_str());
            }
            _ => {}
        }
    }

    res
}
