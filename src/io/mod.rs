use std::io;
use std::io::Read;
use std::collections::VecDeque;
use std::fs::File;

pub trait IO {
    fn read(&mut self) -> String;
    fn print(&mut self, out: String);
}

pub struct UserIO {}

impl UserIO {
    pub fn from() -> UserIO {
        UserIO {}
    }
}

impl IO for UserIO {
    fn read(&mut self) -> String {
        print!("\n");
        let mut line = String::new();
        io::stdin().read_line(&mut line).unwrap();
        String::from(line.trim())
    }

    fn print(&mut self, out: String) {
        print!("{}", out);
    }
}

pub struct MockIO {
    input: VecDeque<String>,
    pub output: Vec<String>,
}

impl MockIO {
    pub fn from(input: Vec<String>) -> MockIO {
        MockIO { input: VecDeque::from(input), output: Vec::new() }
    }
}

impl IO for MockIO {
    fn read(&mut self) -> String {
        self.input.pop_front().unwrap()
    }

    fn print(&mut self, out: String) {
        self.output.push(out);
    }
}

pub fn read_file(path: &str) -> String {
    match File::open(path) {
        Ok(mut file) => {
            let mut content = String::new();
            file.read_to_string(&mut content).unwrap();
            content
        }
        Err(error) => {
            panic!("Error opening file {}: {}", path, error);
        }
    }
}