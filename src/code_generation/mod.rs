use crate::parser::{
    Expression, Statement, VariableAccess, VariableDeclaration, VariableType, AST,
};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct CodeGenerator {
    pub source: String,
    pub var_count: i32,
}

impl CodeGenerator {
    pub fn from(ast: AST) -> CodeGenerator {
        let mut code_gen = CodeGenerator {
            source: String::new(),
            var_count: 0,
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
        self.add_line("int main(){".to_string());

        if let Statement::Block { statements } = main {
            for stmt in statements.iter(){
                self.statement(stmt);
            }
        }

        self.add_line("return 0;".to_string());
        self.add_line("}".to_string());
    }

    fn statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Block { statements:_ }=>self.block(stmt),
            Statement::VariableDeclaration { variables:_ } => self.variable_declaration(stmt),
            _=>{}
        }
    }

    fn block(&mut self, stmt: &Statement){
        self.add_line("{".to_string());
        
        self.add_line("}".to_string());
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
                        self.add_line(format!("{} *{} = NULL;", to_c_type(&var_type), dec.id.clone()));
                    }
                    VariableType::ArrayType { var_type, size}=>{
                        /*
                        First generate temporary variable for holding the
                        size of the array. Then generate code to calculate
                        the value of the variable. Then use the variable
                        as array size parameter. Example generated code
                        could look like:

                        ...
                        int *var_12 = NULL;
                        {
                            int var_13 = 21;
                            int var_14 = 31;
                            int var_15 = var_13 + var_14;
                            var_12 = &var_15;
                        }
                        int user_arr[*var_12];
                        ...
                        */
                        let size_var = self.generate_var();
                        self.add_line(format!("int *{} = NULL;", size_var));
                        self.expression(&size_var, &size);

                        match var_type.as_str() {
                            "string" => {
                                self.add_line(format!("{} *{}[*{}];", to_c_type(&var_type), dec.id.clone(), size_var));
                            }
                            _=>{
                                self.add_line(format!("{} {}[*{}];", to_c_type(&var_type), dec.id.clone(), size_var));
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
    }
}

// Functions for handling expressions
impl CodeGenerator {
    fn expression(&mut self, id: &String, expr: &Expression) {
        self.add_line("{".to_string());
        self.expression_recursion(expr);
        let latest_var = self.get_latest_var();
        self.add_line(format!("{} = &{};", id,latest_var));
        self.add_line("}".to_string());
    }

    fn expression_recursion(&mut self, expr: &Expression) {
        match expr {
            Expression::IntegerLiteral { value } => {
                let v = self.generate_var();
                self.add_line(format!("int {} = {};", v, value));
            }
            Expression::RealLiteral { value } => {
                let v = self.generate_var();
                self.add_line(format!("float {} = {};",v,value));
            }
            Expression::StringLiteral { value }=>{
                let v = self.generate_var();
                self.add_line(format!("char *{} = \"{}\";", v, value));
            }
            Expression::Variable { var } => {
                self.variable_access(var);
            }
            Expression::Unary { op, value } => {
                todo!();
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
                todo!();
            }
            VariableAccess::SimpleAccess { id } => {
                todo!();
            }
            VariableAccess::ArrayAccess { id, index } => {
                todo!();
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
        "user_string" => "char".to_string(),
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
                res.push_str(format!("{} *{}", to_c_type(&var_type), param.id.clone()).as_str());
            }
            VariableType::ArrayType { var_type, size: _ } => {
                res.push_str(format!("{} {}[]", to_c_type(&var_type), param.id.clone()).as_str());
            }
            _ => {}
        }
    }

    res
}
