#[derive(Debug, Eq, PartialEq, Clone)]
pub struct TokenPosition {
    pub line: usize,
    pub char: usize,
}

/// Enum contains all possible tokens and those that can have unique values also have value field.
#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Variable { value: String },
    StringLiteral { value: String },
    IntegerLiteral { value: i32 },
    RealLiteral { value: f32 },
    Unknown { value: String },
    /*
    Special symbols:

    "+" | "-" | "*" | "%" | "=" | "<>" | "<" | ">" | "<=" | ">=" |
    "(" | ")" | "[" | "]" | ":=" | "." | "," | ";" | ":"
     */
    // +
    Plus,
    // -
    Minus,
    // *
    Multiply,
    // /
    Divide,
    // %
    Modulo,
    // =
    Eq,
    // <>
    Inequality,
    // <
    Le,
    // >
    Ge,
    // <=
    Leq,
    // >=
    Geq,
    // (
    OpenParen,
    // )
    CloseParen,
    // [
    OpenBracket,
    // ]
    CloseBracket,
    // :=
    Assign,
    // .
    Dot,
    // ,
    Comma,
    // ;
    Semicolon,
    // :
    Colon,
    // //, /* */
    Comment,
    // " " \n
    Whitespace,
    /*
    Keywords:

    "or" | "and" | "not" | "if" | "then" | "else" | "of" | "while" |
    "do" | "begin" | "end" | "var" | "array" | "procedure" |
    "function" | "program" | "assert" | "return"
    */
    Or,
    And,
    Not,
    If,
    Then,
    Else,
    Of,
    While,
    Do,
    Begin,
    End,
    Var,
    Array,
    Procedure,
    Function,
    Program,
    Assert,
    Return,
}

#[derive(Debug, Eq, PartialEq)]
struct Cursor {
    source: Vec<char>,
    index: usize,
    line: usize,
    char: usize,
}

impl Cursor {
    fn from(source: &String) -> Cursor {
        Cursor {
            source: source.chars().collect(),
            index: 0,
            line: 1,
            char: 1,
        }
    }

    fn peek(&mut self) -> Option<char> {
        return match self.source.get(self.index) {
            Some(c) => Some(c.clone()),
            None => None,
        };
    }

    fn next(&mut self) -> Option<char> {
        match self.source.get(self.index + 1) {
            Some(c) => Some(c.clone()),
            None => None,
        }
    }

    fn inc(&mut self) {
        self.index += 1;

        if let Some(c) = self.peek() {
            if c == '\n' {
                // Change line and reset character position
                self.line += 1;
                self.char = 1;
            } else {
                // Increase character position
                self.char += 1;
            }
        }
    }

    fn get_position(&self) -> TokenPosition {
        TokenPosition {
            line: self.line.clone(),
            char: self.char.clone(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Scanner {
    cursor: Cursor,
    pub tokens: Vec<Token>,
    pub positions: Vec<TokenPosition>,
}

impl Scanner {
    pub fn from(source: &String) -> Scanner {
        Scanner {
            cursor: Cursor::from(source),
            tokens: Vec::new(),
            positions: Vec::new(),
        }
    }

    fn clean(&mut self) {
        /*
        Whitespace and Comment tokens are mostly used
        in order to make the implementation simpler to write.
        After the scanner has classified those tokens
        they are not useful anymore for the parsing phase
        so this function returns the 'cleaned' list of tokens.
         */
        let mut new_tokens = Vec::new();
        let mut new_positions = Vec::new();

        while let Some(token) = self.tokens.last() {
            match token {
                Token::Whitespace | Token::Comment => {
                    self.tokens.pop();
                    self.positions.pop();
                }
                _ => {
                    new_tokens.push(self.tokens.pop().unwrap());
                    new_positions.push(self.positions.pop().unwrap());
                }
            }
        }

        new_tokens.reverse();
        new_positions.reverse();

        self.tokens = new_tokens;
        self.positions = new_positions;
    }

    pub fn scan(&mut self) {
        /*
        Goes trough the source code character by character. Confirmed single
        character tokens have value assigned instantly and (possible) multi character
        tokens are checked with starting character specific scanners.
        The handlers should leave cursor to state where peek() immediately
        after the specific scanner returns the last character consumed by
        the specific scanner. This is because scan() will call inc() in the
        end of each iteration. So if for example scan_forward_slash()
        returns Divide then inc() should not have been called at all.
         */
        let mut current: Option<char> = self.cursor.peek();

        while let Some(c) = current {
            self.positions.push(self.cursor.get_position());
            let add = match c {
                '[' => Token::OpenBracket,
                ']' => Token::CloseBracket,
                '(' => Token::OpenParen,
                ')' => Token::CloseParen,
                '=' => Token::Eq,
                '+' => Token::Plus,
                '-' => Token::Minus,
                '*' => Token::Multiply,
                '%' => Token::Modulo,
                ',' => Token::Comma,
                '/' => self.scan_forward_slash(),
                '<' => self.scan_le(),
                '>' => self.scan_ge(),
                '.' => Token::Dot,
                ';' => Token::Semicolon,
                '{' => self.scan_multiline_comment(),
                ':' => self.scan_colon(),
                'A'..='Z' | 'a'..='z' => {
                    // A-Z|a-z
                    self.scan_variable()
                }
                '0'..='9' => self.scan_number(), // 0-9
                //'.' => { self.scan_dot() }
                '"' => self.scan_string(),
                ' ' | '\n' | '\r' | '\t' => Token::Whitespace,
                _ => {
                    // All other characters that are not recognized.
                    Token::Unknown {
                        value: String::from(format!("Unrecognized character: {:?}", c)),
                    }
                }
            };

            self.tokens.push(add);

            self.cursor.inc();
            current = self.cursor.peek();
        }

        self.clean();
    }

    fn scan_string(&mut self) -> Token {
        let mut s = String::new();
        self.cursor.inc();
        let mut current = self.cursor.peek();

        while let Some(c) = current {
            /*
            Cases to handle:
            -Line change before ending quotation mark error
            -Escaping characters and changing escaped character representation to actual character
            -End quotation and return valid token
            -EOF before ending quotation mark error
             */
            if c == '\n' {
                return Token::Unknown {
                    value: String::from(format!(
                        "String literal missing ending quotation mark: {}",
                        s
                    )),
                };
            } else if c == '\\' {
                if let Some(ch) = self.cursor.next() {
                    // There is a character after escape character
                    /*
                    This needs to make sure that the escaped character actually exists.
                    Not all special characters are handled but this should be enough
                    to cover all use cases.
                     */
                    let c_to_add = match ch {
                        'n' => '\n',  // New line
                        '"' => '\"',  // Quotation mark
                        '\\' => '\\', // Backslash
                        'r' => '\r',  // Carriage return
                        't' => '\t',  // Tab
                        _ => '?',     // Unknown escaped character
                    };
                    if (c_to_add) == '?' {
                        /*
                        In this case the escaped character is invalid and we have to handle resulting error.
                        Since escaped character could be for example typo of '\"' it's hard to tell
                        where the string literal ends so at least for now end it at the point of error
                        and hope for the best :).
                         */
                        s.push(self.cursor.peek().unwrap());
                        self.cursor.inc();
                        s.push(self.cursor.peek().unwrap());
                        return Token::Unknown {
                            value: String::from(format!(
                                "Unknown escaped character in string literal: {}",
                                s
                            )),
                        };
                    } else {
                        // Escaped character was recognized and converted
                        self.cursor.inc();
                        s.push(c_to_add)
                    }
                } else {
                    // EOF after escape character
                    s.push(c);
                    return Token::Unknown {
                        value: format!("String literal missing escaped character and ending quotation mark: {}", s)
                    };
                }
            } else if c == '"' {
                /*
                Since escaped characters are checked in one go '"' here
                will always result in string literal ending.
                 */
                break;
            } else {
                s.push(c);
            }

            self.cursor.inc();
            current = self.cursor.peek();
        }

        // Case EOF without closing '"'
        if let None = current {
            return Token::Unknown {
                value: String::from(format!(
                    "String literal missing ending quotation mark: {:?}",
                    s
                )),
            };
        }

        Token::StringLiteral { value: s }
    }

    fn scan_colon(&mut self) -> Token {
        /*
        If next character is EOF return Colon since it is alone valid token.
        Otherwise, Check is next character is '=' and return Assign or if not
        return Colon.
         */
        return if let Some(ch) = self.cursor.next() {
            if ch == '=' {
                self.cursor.inc();
                Token::Assign
            } else {
                Token::Colon
            }
        } else {
            Token::Colon
        };
    }

    fn scan_forward_slash(&mut self) -> Token {
        /*
        Make sure the next character is not EOF and if so return
        Divide since it alone is valid token. Check if next character
        is '/' for a single line comment, '*' for multiline comment or
        anything else for Divide. Returning Comment token is not really
        useful but makes scan() simpler.
         */
        return if let Some(c) = self.cursor.next() {
            /*
            Three possible cases for next():
            -'/' to start a line comment
            -Any other character which means it is a Divide token
             */
            return match c {
                '/' => {
                    /*
                    Skip until next character is EOL or EOF.
                     */
                    while let Some(ch) = self.cursor.next() {
                        if ch == '\n' {
                            break;
                        }
                        self.cursor.inc();
                    }
                    Token::Comment
                }
                _ => Token::Divide,
            };
        } else {
            // Since peek() is '/' and next() None we must return Divide token.
            Token::Divide
        };
    }

    fn scan_multiline_comment(&mut self) -> Token {
        // consume {
        let mut s = String::from('{');
        self.cursor.inc();

        if let Some('*') = self.cursor.peek() {
            // Consume *
            s.push('*');
            self.cursor.inc();
            while let Some(c) = self.cursor.peek() {
                match c {
                    '*' => {
                        /*
                        Either ends the comment ("*}") or doesn't ("*<anything else>")
                         */
                        if let Some('}') = self.cursor.next() {
                            self.cursor.inc();
                        } else {
                            s.push(c.clone());
                        }
                    }
                    _ => {
                        s.push(c.clone());
                    }
                }
            }
        } else {
            return Token::Unknown { value: s };
        }

        return Token::Unknown { value: s };
    }

    fn scan_le(&mut self) -> Token {
        return if let Some(c) = self.cursor.next() {
            match c {
                '>' => {
                    self.cursor.inc();
                    Token::Inequality
                }
                '=' => {
                    self.cursor.inc();
                    Token::Leq
                }
                _ => Token::Le,
            }
        } else {
            Token::Le
        };
    }

    fn scan_ge(&mut self) -> Token {
        return if let Some(c) = self.cursor.next() {
            match c {
                '=' => {
                    self.cursor.inc();
                    Token::Geq
                }
                _ => Token::Ge,
            }
        } else {
            Token::Ge
        };
    }

    fn scan_number_string(&mut self) -> String {
        if !self.cursor.peek().unwrap().is_numeric() {
            return String::from("");
        }

        let mut s = String::from(self.cursor.peek().unwrap());

        while let Some(c) = self.cursor.next() {
            if c.is_numeric() {
                s.push(c);
                self.cursor.inc();
            } else {
                break;
            }
        }

        s
    }

    fn scan_number(&mut self) -> Token {
        let mut num = self.scan_number_string();

        if let Some(c) = self.cursor.next() {
            match c {
                '.' => {
                    self.cursor.inc();
                    self.cursor.inc();
                    num = format!("{}.{}", num, self.scan_number_string());
                }
                'a'..='z' | 'A'..='Z' => {
                    // Invalid character after number
                    return Token::Unknown {
                        value: format!("Invalid number suffix: {}{}.", num, c.clone()),
                    };
                }
                _ => {
                    // Parsed integer
                    return Token::IntegerLiteral {
                        value: num.parse::<i32>().unwrap(),
                    };
                }
            }
        } else {
            // EOF
            return Token::IntegerLiteral {
                value: num.parse::<i32>().unwrap(),
            };
        }

        let parsed_float = num.parse::<f32>().unwrap();
        let pow;

        if let Some(c) = self.cursor.next() {
            match c {
                'e' => {
                    self.cursor.inc();
                    self.cursor.inc();
                    pow = self.scan_number_string();
                }
                'a'..='z' | 'A'..='Z' => {
                    // Invalid character after number
                    return Token::Unknown {
                        value: format!("Invalid number suffix: {}{}.", num, c.clone()),
                    };
                }
                _ => {
                    // Parsed float without exponent
                    return Token::RealLiteral {
                        value: parsed_float,
                    };
                }
            }
        } else {
            // EOF
            return Token::RealLiteral {
                value: parsed_float,
            };
        }

        let parsed_pow = f32::powi(10.0, pow.parse::<i32>().unwrap());

        if let Some(c) = self.cursor.next() {
            match c {
                'a'..='z' | 'A'..='Z' => {
                    // Invalid character after number so
                    return Token::Unknown {
                        value: format!(
                            "Invalid number suffix: {}{}{}{}.",
                            num,
                            'e',
                            pow,
                            c.clone()
                        ),
                    };
                }
                _ => {
                    return Token::RealLiteral {
                        value: parsed_float * parsed_pow,
                    };
                }
            }
        } else {
            // EOF
            return Token::RealLiteral {
                value: parsed_float * parsed_pow,
            };
        }
    }

    fn scan_variable(&mut self) -> Token {
        /*
        Scans variable name which starts with alphabetic character
        and may contain characters from A-Z|a-z|0-9 after the first
        character. Returns Keyword if scanned variable is part of the
        predefined set of language keywords and otherwise Variable.
         */
        let mut s = String::from(self.cursor.peek().unwrap());

        while let Some(c) = self.cursor.next() {
            // Handles EOF
            /*
            This function should not be called with leading numeric
            character since scan() only calls this function when
            alphabetical character is found. This means that to allow
            variable names with numeric values (For example 'num1') this
            only needs to check for each character that it is alphanumeric.
             */
            if c.is_alphanumeric() || c == '_' {
                s.push(c);
                self.cursor.inc();
            } else {
                break;
            }
        }

        // Case non-sensitivity
        s = s.to_lowercase();

        return match s.as_str() {
            /*
            Keywords:

            "or" | "and" | "not" | "if" | "then" | "else" | "of" | "while" |
            "do" | "begin" | "end" | "var" | "array" | "procedure" |
            "function" | "program" | "assert" | "return"
             */
            "or" => Token::Or,
            "and" => Token::And,
            "not" => Token::Not,
            "if" => Token::If,
            "then" => Token::Then,
            "else" => Token::Else,
            "of" => Token::Of,
            "while" => Token::While,
            "do" => Token::Do,
            "begin" => Token::Begin,
            "end" => Token::End,
            "var" => Token::Var,
            "array" => Token::Array,
            "procedure" => Token::Procedure,
            "function" => Token::Function,
            "program" => Token::Program,
            "assert" => Token::Assert,
            "return" => Token::Return,
            _ => Token::Variable {
                value: format!("user_{}", s),
            },
        };
    }
}
