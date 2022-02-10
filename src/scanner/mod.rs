#[cfg(test)]
mod tests;

/// Enum contains all possible tokens and those that can have unique values also have value field.
#[derive(Debug, Eq, PartialEq)]
pub enum Token {
    String { value: String },
    Variable { value: String },
    Keyword { value: String },
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
    Or,
    Comment, // To make it easier to return from scan_forward_slash().
}

/// Cursor keeps track of the index we are reading from source file.
#[derive(Debug, Eq, PartialEq)]
pub struct Cursor {
    source: Vec<char>,
    index: usize,
}

impl Cursor {
    /// Creates new cursor.
    pub fn new(source: &String, index: usize) -> Cursor {
        Cursor { source: source.chars().collect(), index }
    }

    /// Returns copy of the current character in source file.
    pub fn peek(&mut self) -> Option<char> {
        return match self.source.get(self.index) {
            Some(c) => { Some(c.clone()) }
            None => { None }
        };
    }

    /// Returns copy of the next character in source file.
    pub fn next(&mut self) -> Option<char> {
        match self.source.get(self.index + 1) {
            Some(c) => { Some(c.clone()) }
            None => { None }
        }
    }

    /// Increases the current index.
    pub fn inc(&mut self) {
        self.index += 1;
    }
}

/// The scanner that takes in source file as one string and returns the tokens.
pub fn scan(source: &String) -> Vec<Token> {
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
    let mut tokens: Vec<Token> = Vec::new();
    let mut cursor = Cursor::new(&source, 0);
    let mut current: Option<char> = cursor.peek();

    while let Some(c) = current {
        let add = match c {
            '(' => { Token::OpenParen }
            ')' => { Token::CloseParen }
            '=' => { Token::Equals }
            '+' => { Token::Plus }
            '-' => { Token::Minus }
            '*' => { Token::Multiply }
            '/' => { scan_forward_slash(&mut cursor) }
            '&' => { Token::And }
            '|' => { Token::Or }
            ';' => { Token::Semicolon }
            ':' => { scan_colon(&mut cursor) }
            'A'..='Z' | 'a'..='z' => { scan_variable(&mut cursor) } // A-Z|a-z
            '0'..='9' => { scan_number(&mut cursor) } // 0-9
            '.' => { scan_dot(&mut cursor) }
            '"' => { scan_string(&mut cursor) }
            // CHECK FOR WHITESPACE
            _ => { // All other characters that are not recognized.
                Token::Unknown { value: String::from(format!("Unrecognized character: {:?}", c)) }
            }
        };

        tokens.push(add);

        cursor.inc();
        current = cursor.peek();
    }

    println!("Index: {:?}", cursor.index);

    tokens
}

/// Scans string literal and returns String or Unknown token.
/// String literal is returned without the quotation marks.
pub fn scan_string(cursor: &mut Cursor) -> Token {
    let mut s = String::new();
    cursor.inc();
    let mut current = cursor.peek();

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
            if let Some(ch) = cursor.next() { // There is a character after escape character
                // This needs to make sure that the escaped character actually exists
                let c_to_add = match ch {
                    'n' => { '\n' }  // New line
                    '"' => { '\"' }  // Quotation mark
                    '\\' => { '\\' } // Backslash
                    'r' => { '\r' }  // Carriage return
                    '0' => { '\0' }  // Null
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
                    s.push(cursor.peek().unwrap());
                    cursor.inc();
                    s.push(cursor.peek().unwrap());
                    return Token::Unknown {
                        value: String::from(format!("Unknown escaped character in string literal: {}", s))
                    };
                } else { // Escaped character was recognized and converted
                    cursor.inc();
                    s.push(c_to_add)
                }
            } else { // EOF after escape character
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

        cursor.inc();
        current = cursor.peek();
    }

    // Case EOF without closing '"'
    if let None = current {
        return Token::Unknown {
            value: String::from(format!("String literal missing ending quotation mark: {:?}", s))
        };
    }

    Token::String { value: s }
}

/// Scans colon and returns either Colon for ':' or Assign for ':='.
pub fn scan_colon(cursor: &mut Cursor) -> Token {
    /*
    If next character is EOF return Colon since it is alone valid token.
    Otherwise, Check is next character is '=' and return Assign or if not
    return Colon.
     */
    return if let Some(ch) = cursor.next() {
        if ch == '=' {
            cursor.inc();
            Token::Assign
        } else {
            Token::Colon
        }
    } else {
        Token::Colon
    };
}

/// Scans dot and returns Dots or Unknown is second dot is not present.
pub fn scan_dot(cursor: &mut Cursor) -> Token {
    /*
    Since '..' is the only token that starts with '.' this needs to
    check that next character exists and it is also '.'.
     */
    return if let Some(ch) = cursor.next() {
        if ch == '.' {
            cursor.inc();
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

/// Scans forward slash and returns Divide, Comment or Unknown.
pub fn scan_forward_slash(cursor: &mut Cursor) -> Token {
    /*
    Make sure the next character is not EOF and if so return
    Divide since it alone is valid token. Check if next character
    is '/' for a single line comment, '*' for multiline comment or
    anything else for Divide. Returning Comment token is not really
    useful but makes scan() simpler.
     */
    return if let Some(c) = cursor.next() {
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
                while let Some(ch) = cursor.next() {
                    if ch == '\n' {
                        break;
                    }
                    cursor.inc();
                }
                Token::Comment
            }
            '*' => {
                /*
                Skip until end of comment is found and if EOF is found
                before end of comment return Unknown.
                 */
                let mut err_val = String::from("/*");
                cursor.inc(); // Skip comment starting '*'
                let mut ast_found = false;
                while let Some(ch) = cursor.next() {
                    if ch == '*' {
                        err_val.push(ch);
                        ast_found = true;
                    } else if ch == '/' && ast_found {
                        cursor.inc(); // After this point the cursor should point to comment ending '/'
                        break;
                    } else {
                        err_val.push(ch);
                        ast_found = false;
                    }
                    cursor.inc();
                }

                if let None = cursor.peek() {
                    /*
                    Comment ending character was not found before EOF.
                     */
                    return Token::Unknown {value: String::from(format!("Unexpected end of file before ending of multiline comment: {}", err_val))};
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

/// Scans number and returns Number or Unknown.
pub fn scan_number(cursor: &mut Cursor) -> Token {
    /*
    Make sure that number is not continued with characters
    in order to attempt naming variable with leading numerical
    character (for example '1num'). This should result into an
    Unknown token result. Any other character ending a number
    should result into an Number token and letting scan() deal
    with the token ending character.
     */
    let mut s = String::from(cursor.peek().unwrap());

    while let Some(c) = cursor.next() {
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
            while let Some(ec) = cursor.next() { // Also handles EOF.
                if ec.is_alphanumeric() {
                    s.push(ec);
                    cursor.inc();
                } else {
                    break;
                }
            }

            return Token::Unknown { value: format!("Invalid number suffix in: {}",s) };
        } else if add == '!' {
            break; // Break without cursor.inc() to let scan() deal with next char.
        } else {
            /*
            Only remaining case is that c is number and we can extend s
            in hopes of creating a valid number.
             */
            s.push(c);
        }
        cursor.inc();
    }

    /*
    There is no need to handle case where end of line
    is found in the middle of building a number since
    for example string "4" alone would result in a valid
    token.
    */

    Token::Number { value: s.parse::<i32>().unwrap() }
}

/// Scans variable and returns Keyword or Variable.
pub fn scan_variable(cursor: &mut Cursor) -> Token {
    /*
    Scans variable name which starts with alphabetic character
    and may contain characters from A-Z|a-z|0-9 after the first
    character. Returns Keyword if scanned variable is part of the
    predefined set of language keywords and otherwise Variable.
     */
    let mut s = String::from(cursor.peek().unwrap());

    while let Some(c) = cursor.next() { // Handles EOF
        /*
        This function should not be called with leading numeric
        character since scan() only calls this function when
        alphabetical character is found. This means that to allow
        variable names with numeric values (For example 'num1') this
        only needs to check for each character that it is alphanumeric.
         */
        if c.is_alphanumeric() {
            s.push(c);
            cursor.inc();
        } else {
            break;
        }
    }

    if is_keyword(&s) {
        Token::Keyword { value: s }
    } else {
        Token::Variable { value: s }
    }
}

/// Checks if variable is a keyword.
pub fn is_keyword(s: &String) -> bool {
    /*
    Not the optimal way of doing this since
    this list will be created for each check
    of keywords but shouldn't be too bad since
    the language doesn't have too many keywords.
     */
    let words = vec![
        String::from("var"),
        String::from("for"),
        String::from("end"),
        String::from("in"),
        String::from("do"),
        String::from("read"),
        String::from("print"),
        String::from("int"),
        String::from("string"),
        String::from("bool"),
        String::from("assert"),
    ];
    return words.contains(s);
}