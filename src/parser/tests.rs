use super::*;
use crate::scanner::{scan_clean};


#[test]
fn variable_without_initial_assigned__value() {
    let mut input1 = scan_clean(&String::from("var hello : int;"));
    input1.reverse();
    let res1 = parse(&mut input1);
    let exp1 = Tree::Stmts {
        value: vec![
            Tree::Stmt {
                value: vec![
                    Tree::Var,
                    Tree::Id { value: String::from("hello") },
                    Tree::Int,
                ]
            }
        ]
    };

    assert_eq!(res1, exp1);

    let mut input2 = scan_clean(&String::from("var hello : bool;"));
    input2.reverse();
    let res2 = parse(&mut input2);
    let exp2 = Tree::Stmts {
        value: vec![
            Tree::Stmt {
                value: vec![
                    Tree::Var,
                    Tree::Id { value: String::from("hello") },
                    Tree::Bool,
                ]
            }
        ]
    };

    assert_eq!(res2, exp2);

    let mut input3 = scan_clean(&String::from("var hello : string;"));
    input3.reverse();
    let res3 = parse(&mut input3);
    let exp3 = Tree::Stmts {
        value: vec![
            Tree::Stmt {
                value: vec![
                    Tree::Var,
                    Tree::Id { value: String::from("hello") },
                    Tree::String,
                ]
            }
        ]
    };

    assert_eq!(res3, exp3);
}

#[test]
fn int_variable_with_simple_expression_assign(){
    let mut input1 = scan_clean(&String::from("var hello : int := 42;"));
    input1.reverse();
    let res1 = parse(&mut input1);
    let exp1 = Tree::Stmts {
        value: vec![
            Tree::Stmt {
                value: vec![
                    Tree::Var,
                    Tree::Id { value: String::from("hello") },
                    Tree::Int,
                    Tree::Expr {
                        value:Box::from(Tree::Number {
                            value: 42
                        })
                    }
                ]
            }
        ]
    };

    assert_eq!(res1, exp1);
}