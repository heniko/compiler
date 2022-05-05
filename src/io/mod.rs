use std::fs::File;
use std::io::Read;
use std::io::{self, Write};

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

pub fn write_file(path: &str, output: String) {
    let mut file = std::fs::File::create(path).expect("Failed to create output file.");
    file.write_all(output.as_bytes())
        .expect("Failed to write output to file.");
    print!("Output file successfully created.");
}
