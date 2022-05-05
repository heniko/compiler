use crate::parser::{Expression, Statement, VariableAccess, VariableType, AST};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct CodeGenerator {
    pub source: String,
}

impl CodeGenerator {
    pub fn from(ast: AST) -> CodeGenerator {
        let mut code_gen = CodeGenerator {
            source: String::new(),
        };
        code_gen.generate(&ast);
        code_gen
    }

    fn generate(&mut self, ast: &AST) {
        self.source.push_str("Hello source file!");
    }
}
