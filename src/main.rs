mod scanner;

fn main() {
    let source = String::from("\"Hello, world!\"\"");
    let _s = scanner::scan(&source);
}
