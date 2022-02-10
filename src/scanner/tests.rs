use super::*;

#[test]
fn basic_scan_string_case() {
    let mut c = Cursor::new(&String::from("\"Hello, string literal!\"0"), 0);
    let res = scan_string(&mut c);
    let exp = Token::String { value: String::from("Hello, string literal!") };
    assert_eq!(res, exp);
    assert_eq!(c.next().unwrap(), '0');
}

#[test]
fn scan_string_literal_with_line_change() {
    let mut c = Cursor::new(&String::from("\"Hello\\nstring literal!\"0"), 0);
    let res = scan_string(&mut c);
    let exp = Token::String { value: String::from("Hello\nstring literal!") };
    assert_eq!(res, exp);
    assert_eq!(c.next().unwrap(), '0');
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
fn is_keyword_test() {
    assert_eq!(is_keyword(&String::from("nope")), false);
    // All keywords exist
    assert_eq!(is_keyword(&String::from("var")),true);
    assert_eq!(is_keyword(&String::from("for")),true);
    assert_eq!(is_keyword(&String::from("end")),true);
    assert_eq!(is_keyword(&String::from("in")),true);
    assert_eq!(is_keyword(&String::from("do")),true);
    assert_eq!(is_keyword(&String::from("read")),true);
    assert_eq!(is_keyword(&String::from("print")),true);
    assert_eq!(is_keyword(&String::from("int")),true);
    assert_eq!(is_keyword(&String::from("string")), true);
    assert_eq!(is_keyword(&String::from("bool")), true);
    assert_eq!(is_keyword(&String::from("assert")), true);
}

#[test]
fn basic_scan_variable() {
    let mut c = Cursor::new(&String::from("hello : int = 123;"),0);
    let res = scan_variable(&mut c);
    let exp = Token::Variable {value: String::from("hello")};
    assert_eq!(res, exp);
    assert_eq!(c.peek().unwrap(), 'o');
}

#[test]
fn basic_scan_keyword() {
    let mut c = Cursor::new(&String::from("var hello : int = 123;"),0);
    let res = scan_variable(&mut c);
    let exp = Token::Keyword {value: String::from("var")};
    assert_eq!(res, exp);
    assert_eq!(c.peek().unwrap(), 'r');
}

#[test]
fn complex_scan_variable_with_numerics() {
    let mut c = Cursor::new(&String::from("hello123:int=123;"),0);
    let res = scan_variable(&mut c);
    let exp = Token::Variable {value: String::from("hello123")};
    assert_eq!(res, exp);
    assert_eq!(c.peek().unwrap(), '3');
    assert_eq!(c.next().unwrap(), ':');
}