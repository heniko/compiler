#[cfg(test)]
mod tests;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct TokenPosition {
    line: usize,
    char: usize,
}

/// Enum contains all possible tokens and those that can have unique values also have value field.
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Token {
    StringLiteral { value: String },
    Variable { value: String },
    Number { value: i32 },
    Unknown { value: String },
    Assign,
    Semicolon,
    Colon,
    OpenParen,
    CloseParen,
    Dots,
    Equals,
    Plus,
    Minus,
    Multiply,
    Divide,
    And,
    Not,
    LessThan,
    Comment,
    Whitespace,
    Var,
    For,
    End,
    In,
    Do,
    Read,
    Print,
    Int,
    String,
    Bool,
    Assert,
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
        Cursor { source: source.chars().collect(), index: 0, line: 1, char: 1 }
    }

    fn peek(&mut self) -> Option<char> {
        return match self.source.get(self.index) {
            Some(c) => { Some(c.clone()) }
            None => { None }
        };
    }

    fn next(&mut self) -> Option<char> {
        match self.source.get(self.index + 1) {
            Some(c) => { Some(c.clone()) }
            None => { None }
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

#[derive(Debug, Eq, PartialEq)]
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
                '(' => { Token::OpenParen }
                ')' => { Token::CloseParen }
                '=' => { Token::Equals }
                '+' => { Token::Plus }
                '-' => { Token::Minus }
                '*' => { Token::Multiply }
                '/' => { self.scan_forward_slash() }
                '&' => { Token::And }
                '!' => { Token::Not }
                '<' => { Token::LessThan }
                ';' => { Token::Semicolon }
                ':' => { self.scan_colon() }
                'A'..='Z' | 'a'..='z' => { self.scan_variable() } // A-Z|a-z
                '0'..='9' => { self.scan_number() } // 0-9
                '.' => { self.scan_dot() }
                '"' => { self.scan_string() }
                ' ' | '\n' | '\r' | '\t' => { Token::Whitespace }
                _ => { // All other characters that are not recognized.
                    Token::Unknown { value: String::from(format!("Unrecognized character: {:?}", c)) }
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
                    value: String::from(format!("String literal missing ending quotation mark: {}", s))
                };
            } else if c == '\\' {
                if let Some(ch) = self.cursor.next() { // There is a character after escape character
                    /*
                    This needs to make sure that the escaped character actually exists.
                    Not all special characters are handled but this should be enough
                    to cover all use cases.
                     */
                    let c_to_add = match ch {
                        'n' => { '\n' }  // New line
                        '"' => { '\"' }  // Quotation mark
                        '\\' => { '\\' } // Backslash
                        'r' => { '\r' }  // Carriage return
                        't' => { '\t' }  // Tab
                        _ => { '?' }     // Unknown escaped character
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
                            value: String::from(format!("Unknown escaped character in string literal: {}", s))
                        };
                    } else { // Escaped character was recognized and converted
                        self.cursor.inc();
                        s.push(c_to_add)
                    }
                } else { // EOF after escape character
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
                value: String::from(format!("String literal missing ending quotation mark: {:?}", s))
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

    fn scan_dot(&mut self) -> Token {
        /*
        Since '..' is the only token that starts with '.' this needs to
        check that next character exists and it is also '.'.
         */
        return if let Some(ch) = self.cursor.next() {
            if ch == '.' {
                self.cursor.inc();
                Token::Dots
            } else {
                Token::Unknown {
                    value: String::from("Unrecognized token: '.'")
                }
            }
        } else {
            Token::Unknown {
                value: String::from("Unexpected end of file after character: '.'")
            }
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
            -'*' to start a multiline comment that needs to be ended
             correctly
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
                '*' => {
                    /*
                    Skip until end of comment is found and if EOF is found
                    before end of comment return Unknown.
                     */
                    let mut err_val = String::from("/*");
                    self.cursor.inc(); // Skip comment starting '*'
                    let mut ast_found = false;
                    while let Some(ch) = self.cursor.next() {
                        if ch == '*' {
                            err_val.push(ch);
                            ast_found = true;
                        } else if ch == '/' && ast_found {
                            self.cursor.inc(); // After this point the cursor should point to comment ending '/'
                            break;
                        } else {
                            err_val.push(ch);
                            ast_found = false;
                        }
                        self.cursor.inc();
                    }

                    if let None = self.cursor.peek() {
                        /*
                        Comment ending character was not found before EOF.
                         */
                        return Token::Unknown { value: String::from(format!("Unexpected end of file before ending of multiline comment: {}", err_val)) };
                    }

                    Token::Comment
                }
                _ => { Token::Divide }
            };
        } else {
            // Since peek() is '/' and next() None we must return Divide token.
            Token::Divide
        };
    }

    fn scan_number(&mut self) -> Token {
        /*
        Make sure that number is not continued with characters
        in order to attempt naming variable with leading numerical
        character (for example '1num'). This should result into an
        Unknown token result. Any other character ending a number
        should result into an Number token and letting scan() deal
        with the token ending character.
         */
        let mut s = String::from(self.cursor.peek().unwrap());

        while let Some(c) = self.cursor.next() {
            let add = match c {
                '0'..='9' => { c }
                'A'..='Z' | 'a'..='z' => { '?' }
                _ => { '!' }
            };

            if add == '?' {
                /*
                Handle error case where starting number turns into a variable.
                Handled by finding next non numerical and non alphabetical
                character for scan() to continue from.
                 */
                while let Some(ec) = self.cursor.next() { // Also handles EOF.
                    if ec.is_alphanumeric() {
                        s.push(ec);
                        self.cursor.inc();
                    } else {
                        break;
                    }
                }

                return Token::Unknown { value: format!("Invalid number suffix in: {}", s) };
            } else if add == '!' {
                break; // Break without cursor.inc() to let scan() deal with next char.
            } else {
                /*
                Only remaining case is that c is number and we can extend s
                in hopes of creating a valid number.
                 */
                s.push(c);
            }
            self.cursor.inc();
        }

        /*
        There is no need to handle case where end of line
        is found in the middle of building a number since
        for example string "4" alone would result in a valid
        token.
        */

        Token::Number { value: s.parse::<i32>().unwrap() }
    }

    fn scan_variable(&mut self) -> Token {
        /*
        Scans variable name which starts with alphabetic character
        and may contain characters from A-Z|a-z|0-9 after the first
        character. Returns Keyword if scanned variable is part of the
        predefined set of language keywords and otherwise Variable.
         */
        let mut s = String::from(self.cursor.peek().unwrap());

        while let Some(c) = self.cursor.next() { // Handles EOF
            /*
            This function should not be called with leading numeric
            character since scan() only calls this function when
            alphabetical character is found. This means that to allow
            variable names with numeric values (For example 'num1') this
            only needs to check for each character that it is alphanumeric.
             */
            if c.is_alphanumeric() {
                s.push(c);
                self.cursor.inc();
            } else {
                break;
            }
        }

        return match s.as_str() {
            "var" => { Token::Var }
            "for" => { Token::For }
            "end" => { Token::End }
            "in" => { Token::In }
            "do" => { Token::Do }
            "read" => { Token::Read }
            "print" => { Token::Print }
            "int" => { Token::Int }
            "string" => { Token::String }
            "bool" => { Token::Bool }
            "assert" => { Token::Assert }
            _ => { Token::Variable { value: s } }
        };
    }
}