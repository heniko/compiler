use super::*;

#[test]
fn basic_scan_string_case() {
    let mut c = Cursor::new(&String::from("\"Hello, string literal!\"0"), 0);
    let res = scan_string(&mut c);
    let exp = Token::StringLiteral { value: String::from("Hello, string literal!") };
    assert_eq!(res, exp);
    assert_eq!(c.next().unwrap(), '0');
}

#[test]
fn scan_string_literal_with_line_change() {
    let mut c = Cursor::new(&String::from("\"Hello\\nstring literal!\"0"), 0);
    let res = scan_string(&mut c);
    let exp = Token::StringLiteral { value: String::from("Hello\nstring literal!") };
    assert_eq!(res, exp);
    assert_eq!(c.next().unwrap(), '0');
}

#[test]
fn scan_complex_string_literal() {
    let mut c = Cursor::new(&String::from(r#""\\\"Hello\nWorld!\"\t123"0"#), 0);
    let res = scan_string(&mut c);
    let exp = Token::StringLiteral { value: String::from("\\\"Hello\nWorld!\"\t123") };
    assert_eq!(res, exp);
    assert_eq!(c.next().unwrap(), '0');
}

#[test]
fn string_literal_missing_end() {
    let mut c = Cursor::new(&String::from("\"string missing quotation\n"), 0);
    let res = scan_string(&mut c);
    let exp = Token::Unknown { value: String::from("String literal missing ending quotation mark: string missing quotation") };
    assert_eq!(res, exp);
}

#[test]
fn string_literal_sudden_end_after_escaped_char() {
    let mut c = Cursor::new(&String::from("\"string missing escaped char\\"), 0);
    let res = scan_string(&mut c);
    let exp = Token::Unknown { value: String::from("String literal missing escaped character and ending quotation mark: string missing escaped char\\") };
    assert_eq!(res, exp);
}

#[test]
fn string_literal_unknown_escaped_char() {
    let mut c = Cursor::new(&String::from("\"unknown escape\\m"), 0);
    let res = scan_string(&mut c);
    let exp = Token::Unknown { value: String::from("Unknown escaped character in string literal: unknown escape\\m") };
    assert_eq!(res, exp);
}

#[test]
fn string_literal_unknown_special_escaped_char() {
    let mut c = Cursor::new(&String::from("\"unknown escape\\\n"), 0);
    let res = scan_string(&mut c);
    let exp = Token::Unknown { value: String::from("Unknown escaped character in string literal: unknown escape\\\n") };
    assert_eq!(res, exp);
}

#[test]
fn basic_scan_colon() {
    let mut c = Cursor::new(&String::from(":hello"), 0);
    let res = scan_colon(&mut c);
    let exp = Token::Colon;
    assert_eq!(res, exp);
    assert_eq!(c.next().unwrap(), 'h');
}

#[test]
fn basic_scan_assign() {
    let mut c = Cursor::new(&String::from(":=hello"), 0);
    let res = scan_colon(&mut c);
    let exp = Token::Assign;
    assert_eq!(res, exp);
    assert_eq!(c.next().unwrap(), 'h');
}

#[test]
fn basic_scan_dots() {
    let mut c = Cursor::new(&String::from("..hello"), 0);
    let res = scan_dot(&mut c);
    let exp = Token::Dots;
    assert_eq!(res, exp);
    assert_eq!(c.next().unwrap(), 'h');
}

#[test]
fn basic_dot_unknown() {
    let mut c = Cursor::new(&String::from(".hello"), 0);
    let res = scan_dot(&mut c);
    let exp = Token::Unknown { value: String::from("Unrecognized token: '.'") };
    assert_eq!(res, exp);
}

#[test]
fn basic_dot_eof() {
    let mut c = Cursor::new(&String::from("."), 0);
    let res = scan_dot(&mut c);
    let exp = Token::Unknown { value: String::from("Unexpected end of file after character: '.'") };
    assert_eq!(res, exp);
    assert_eq!(c.peek().unwrap(), '.');
    assert_eq!(c.next(), None);
}

#[test]
fn basic_scan_divide() {
    let mut c = Cursor::new(&String::from("/hello"), 0);
    let res = scan_forward_slash(&mut c);
    let exp = Token::Divide;
    assert_eq!(res, exp);
    assert_eq!(c.next().unwrap(), 'h');
}

#[test]
fn basic_line_comment() {
    let mut c = Cursor::new(&String::from("//this is a line comment\nand this is not"), 0);
    let res = scan_forward_slash(&mut c);
    let exp = Token::Comment;
    assert_eq!(res, exp);
    assert_eq!(c.next().unwrap(), '\n');
}

#[test]
fn basic_valid_multiline_comment() {
    let mut c = Cursor::new(&String::from("/*This is multiline comment*/0"), 0);
    let res = scan_forward_slash(&mut c);
    let exp = Token::Comment;
    assert_eq!(res, exp);
    assert_eq!(c.next().unwrap(), '0');
}

#[test]
fn complex_multiline_comment() {
    let mut c = Cursor::new(&String::from("/*Hello * / world** // ***/0123"), 0);
    let res = scan_forward_slash(&mut c);
    let exp = Token::Comment;
    assert_eq!(res, exp);
    assert_eq!(c.next().unwrap(), '0');
}

// let mut c = Cursor::new(&String::from(""),0);
#[test]
fn simple_scan_number() {
    let mut c = Cursor::new(&String::from("123 h"), 0);
    let res = scan_number(&mut c);
    let exp = Token::Number { value: 123 };
    assert_eq!(res, exp);
    assert_eq!(c.next().unwrap(), ' ');
}

#[test]
fn scan_number_error() {
    let mut c = Cursor::new(&String::from("123hello bye"), 0);
    let res = scan_number(&mut c);
    let exp = Token::Unknown { value: String::from("Invalid number suffix in: 123hello") };
    assert_eq!(res, exp);
    assert_eq!(c.peek().unwrap(), 'o');
    assert_eq!(c.next().unwrap(), ' ');
}

#[test]
fn end_number_without_whitespace() {
    let mut c = Cursor::new(&String::from("123*123"), 0);
    let res = scan_number(&mut c);
    let exp = Token::Number { value: 123 };
    assert_eq!(res, exp);
    assert_eq!(c.next().unwrap(), '*');
}

#[test]
fn basic_scan_variable() {
    let mut c = Cursor::new(&String::from("hello : int = 123;"), 0);
    let res = scan_variable(&mut c);
    let exp = Token::Variable { value: String::from("hello") };
    assert_eq!(res, exp);
    assert_eq!(c.peek().unwrap(), 'o');
}

#[test]
fn basic_scan_keyword() {
    let mut c = Cursor::new(&String::from("var hello : int = 123;"), 0);
    let res = scan_variable(&mut c);
    let exp = Token::Var;
    assert_eq!(res, exp);
    assert_eq!(c.peek().unwrap(), 'r');
}

#[test]
fn complex_scan_variable_with_numerics() {
    let mut c = Cursor::new(&String::from("hello123:int=123;"), 0);
    let res = scan_variable(&mut c);
    let exp = Token::Variable { value: String::from("hello123") };
    assert_eq!(res, exp);
    assert_eq!(c.peek().unwrap(), '3');
    assert_eq!(c.next().unwrap(), ':');
}

#[test]
fn scan_assert() {
    let res = scan(&String::from("assert (x = nTimes);"));
    let exp = vec![
        Token::Assert,
        Token::Whitespace,
        Token::OpenParen,
        Token::Variable { value: String::from("x") },
        Token::Whitespace,
        Token::Equals,
        Token::Whitespace,
        Token::Variable { value: String::from("nTimes") },
        Token::CloseParen,
        Token::Semicolon,
    ];
    assert_eq!(res, exp);
}

#[test]
fn test_scan_simple_program() {
    let res = scan(&String::from("var X : int := 4 + (6 * 2);\nprint X;"));
    let exp = vec![
        Token::Var,
        Token::Whitespace,
        Token::Variable { value: String::from("X") },
        Token::Whitespace,
        Token::Colon,
        Token::Whitespace,
        Token::Int,
        Token::Whitespace,
        Token::Assign,
        Token::Whitespace,
        Token::Number { value: 4 },
        Token::Whitespace,
        Token::Plus,
        Token::Whitespace,
        Token::OpenParen,
        Token::Number { value: 6 },
        Token::Whitespace,
        Token::Multiply,
        Token::Whitespace,
        Token::Number { value: 2 },
        Token::CloseParen,
        Token::Semicolon,
        Token::Whitespace,
        Token::Print,
        Token::Whitespace,
        Token::Variable { value: String::from("X") },
        Token::Semicolon,
    ];
    assert_eq!(res, exp);
}

#[test]
fn scan_loop() {
    let res = scan(
        &String::from("var v : int := 1;\nvar i : int;\nfor i in 1..n do\n\tv := v + i;\nend for;")
    );
    let exp = vec![
        Token::Var,
        Token::Whitespace,
        Token::Variable { value: String::from("v") },
        Token::Whitespace,
        Token::Colon,
        Token::Whitespace,
        Token::Int,
        Token::Whitespace,
        Token::Assign,
        Token::Whitespace,
        Token::Number { value: 1 },
        Token::Semicolon,
        Token::Whitespace,
        Token::Var,
        Token::Whitespace,
        Token::Variable { value: String::from("i") },
        Token::Whitespace,
        Token::Colon,
        Token::Whitespace,
        Token::Int,
        Token::Semicolon,
        Token::Whitespace,
        Token::For,
        Token::Whitespace,
        Token::Variable { value: String::from("i") },
        Token::Whitespace,
        Token::In,
        Token::Whitespace,
        Token::Number { value: 1 },
        Token::Dots,
        Token::Variable { value: String::from("n") },
        Token::Whitespace,
        Token::Do,
        Token::Whitespace,
        Token::Whitespace,
        Token::Variable { value: String::from("v") },
        Token::Whitespace,
        Token::Assign,
        Token::Whitespace,
        Token::Variable { value: String::from("v") },
        Token::Whitespace,
        Token::Plus,
        Token::Whitespace,
        Token::Variable { value: String::from("i") },
        Token::Semicolon,
        Token::Whitespace,
        Token::End,
        Token::Whitespace,
        Token::For,
        Token::Semicolon,
    ];
    assert_eq!(res, exp);
}

#[test]
fn scan_complex_math() {
    let res = scan(&String::from("(*89/i=((/Hi );"));
    let exp = vec![
        Token::OpenParen,
        Token::Multiply,
        Token::Number { value: 89 },
        Token::Divide,
        Token::Variable { value: String::from("i") },
        Token::Equals,
        Token::OpenParen,
        Token::OpenParen,
        Token::Divide,
        Token::Variable { value: String::from("Hi") },
        Token::Whitespace,
        Token::CloseParen,
        Token::Semicolon,
    ];
    assert_eq!(res, exp);
}

#[test]
fn assign_complex_string_literal() {
    let res = scan(&String::from(r#"var hi:string:= "Hello\\";"#));
    let exp = vec![
        Token::Var,
        Token::Whitespace,
        Token::Variable { value: String::from("hi") },
        Token::Colon,
        Token::String,
        Token::Assign,
        Token::Whitespace,
        Token::StringLiteral { value: String::from("Hello\\") },
        Token::Semicolon,
    ];
    assert_eq!(res, exp)
}

#[test]
fn scan_clean_cleans_whitespace_and_comments() {
    let res = scan_clean(&String::from("var hello/*wor\n*///ld!"));
    let exp = vec![
        Token::Var,
        Token::Variable { value: String::from("hello") },
    ];
    assert_eq!(res, exp)
}