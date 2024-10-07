use std::string::FromUtf8Error;

/// Represents a token in the Lua language.
#[derive(Debug, Eq, PartialEq)]
pub enum Token {
    Eof,
    Unknown(u8),

    // Primitives
    Name(String),
    Number(String),
    String(String, char), // Contents and the character the string is created with, eg. ' or "
    MultilineString(String, u16), // Contents and how many ='s?
    Label(String),        // ::label::, span should encompass everything including ::'s

    // Singletons
    True,
    False,
    Nil,

    // Keywords
    Break,
    Do,
    Else,
    ElseIf,
    End,
    For,
    Function,
    If,
    In,
    Local,
    Repeat,
    Return,
    Then,
    Until,
    While,
    Goto,

    // Binary operators
    Add,      // +
    Subtract, // -
    Multiply, // *
    Divide,   // /
    Power,    // ^
    Modulo,   // %

    Greater,      // >
    Less,         // <
    GreaterEqual, // >=
    LessEqual,    // <=

    Equal,
    NotEqualGmod, // !=
    NotEqual,     // ~=
    And,          // "and"
    AndGmod,      // &&
    Or,           // "or"
    OrGmod,       // ||
    Concat,       // ..

    // Unary Operators
    Not,     // "not" keyword
    NotGmod, // ! symbol
    Length,  // #

    // Misc
    Assign, // =
    Dot,    // .
    Dots,   // ...

    SemiColon, // ;
    Comma,     // ,
    Colon,     // :

    LeftCurly,   // {
    RightCurly,  // }
    LeftSquare,  // [
    RightSquare, // ]
    LeftParen,   // (
    RightParen,  // )
}

#[derive(Debug, thiserror::Error)]
pub enum LexError {
    #[error("A utf-8 error has occurred: {from}")]
    Utf8 {
        #[from]
        from: FromUtf8Error,
    },

    #[error("Encountered a malformed number while parsing, \"{value}\": {message}")]
    MalformedNumber { value: String, message: String },

    #[error("Encountered a malformed string while parsing, \"{value}\": {message}")]
    MalformedString { value: String, message: String },
}

pub type LexResult<T> = Result<T, LexError>;

/// Represents the lexical analyzer over a slice of bytes.
pub struct Lex<'data> {
    data: &'data [u8],
    index: usize,

    eof: bool,

    char: u8,
    buffer: Vec<u8>,
}

impl<'data> Lex<'data> {
    pub fn new(data: &'data [u8]) -> Self {
        Self {
            data,
            index: 0,

            eof: data.len() == 0,

            char: if data.len() == 0 { 0 } else { data[0] },
            buffer: vec![],
        }
    }

    pub fn next(&mut self) -> LexResult<Token> {
        self.scan()
    }

    fn scan(&mut self) -> LexResult<Token> {
        self.skip_while(|c| c.is_ascii_whitespace());

        if self.eof {
            return Ok(Token::Eof);
        }

        let token = match self.char {
            // Match keywords or identifiers.
            b'a'..b'z' | b'A'..b'Z' | b'_' => {
                // Consume valid identifier chars.
                self.consume_while(|c| c.is_ascii_alphanumeric() || c == b'_');

                // Match for a keyword, otherwise create a name token.
                match &*self.buffer {
                    b"local" => Token::Local,
                    b"function" => Token::Function,
                    b"while" => Token::While,
                    b"repeat" => Token::Repeat,
                    b"for" => Token::For,
                    b"if" => Token::If,
                    b"else" => Token::Else,
                    b"elseif" => Token::ElseIf,
                    b"do" => Token::Do,
                    b"then" => Token::Then,
                    b"end" => Token::End,
                    _ => Token::Name(String::from_utf8(self.buffer.clone())?),
                }
            }
            // Match numbers.
            b'0'..b'9' => self.scan_number()?,
            // Match strings.
            b'\'' | b'"' => self.scan_string()?,
            // Match unknown tokens.
            char => {
                self.skip();
                Token::Unknown(char)
            }
        };

        Ok(token)
    }

    fn scan_number(&mut self) -> LexResult<Token> {
        let mut xp = b'e';

        // Check for '0x' or '0X'.
        if self.char == b'0' {
            self.consume();

            if self.char.to_ascii_lowercase() == b'x' {
                self.consume();
                xp = b'p';
            }
        }

        // Read alphanumeric characters.
        let mut prev_char = 0u8;
        loop {
            if !(self.char.is_ascii_alphanumeric()
                || self.char == b'_'
                || self.char == b'.'
                || ((self.char == b'-' || self.char == b'+')
                    && prev_char.to_ascii_lowercase() == xp))
            {
                break;
            }

            prev_char = self.char;
            self.consume();
        }

        // Scan the number.
        let mut i = 0;
        let mut d = false;
        let mut e = false;

        if xp == b'p' {
            i += 2;
        }

        loop {
            if i >= self.buffer.len() {
                break;
            }

            let char = self.buffer[i].to_ascii_lowercase();

            match (xp, char) {
                (b'e', b'0'..b'9') => {}
                (b'p', b'0'..b'9' | b'a'..b'f') => {
                    if e && char >= b'a' && char <= b'f' {
                        return Err(LexError::MalformedNumber {
                            value: self.buffer_to_string()?,
                            message: "Encountered hexadecimal characters within exponential portion of number..".to_string(),
                        });
                    }
                }
                (_, b'.') => {
                    if d {
                        return Err(LexError::MalformedNumber {
                            value: self.buffer_to_string()?,
                            message: "Encountered a duplicate decimal point within number."
                                .to_string(),
                        });
                    }

                    if e {
                        return Err(LexError::MalformedNumber {
                            value: self.buffer_to_string()?,
                            message: "Encountered a decimal point within the exponential portion of number.".to_string(),
                        });
                    }

                    d = true
                }
                (b'p', b'p') | (b'e', b'e') => {
                    if e {
                        return Err(LexError::MalformedNumber {
                            value: self.buffer_to_string()?,
                            message: "Encountered a duplicate exponential symbol within number."
                                .to_string(),
                        });
                    }

                    e = true
                }
                (_, b'-' | b'+') => {}
                (_, _) => {
                    return Err(LexError::MalformedNumber {
                        value: self.buffer_to_string()?,
                        message: "Encountered an unknown character within number.".to_string(),
                    });
                }
            };

            i += 1;
        }

        Ok(Token::Number(self.buffer_to_string()?))
    }

    fn scan_string(&mut self) -> LexResult<Token> {
        let quote_char = self.char;

        self.skip();

        while self.char != quote_char {
            if self.eof {
                return Err(LexError::MalformedString {
                    value: self.buffer_to_string()?,
                    message: "Unexpected end of file.".to_string(),
                });
            }

            match self.char {
                b'\r' | b'\n' => {
                    return Err(LexError::MalformedString {
                        value: self.buffer_to_string()?,
                        message: "Unexpected new line.".to_string(),
                    });
                }
                b'\\' => {
                    self.skip();

                    if self.eof {
                        return Err(LexError::MalformedString {
                            value: self.buffer_to_string()?,
                            message: "Unexpected end of file.".to_string(),
                        });
                    }

                    let real_char = match self.char {
                        b'a' => 0x07,
                        b'b' => 0x08,
                        b'f' => 0x0C,
                        b'n' => b'\n',
                        b'r' => b'\r',
                        b't' => b'\t',
                        b'v' => 0x0B,
                        b'x' => todo!(),
                        b'u' => todo!(),
                        _ => todo!(),
                    };
                }
                _ => {}
            }
        }

        todo!()
    }

    fn skip(&mut self) {
        if self.eof {
            return;
        }

        self.index += 1;
        if self.index >= self.data.len() {
            self.eof = true;
            self.char = 0;
            return;
        }

        self.char = self.data[self.index];
    }

    fn skip_while<F>(&mut self, f: F)
    where
        F: Fn(u8) -> bool,
    {
        loop {
            if !f(self.char) || self.eof {
                break;
            }

            self.skip();
        }
    }

    fn consume(&mut self) {
        if self.eof == true {
            return;
        }

        self.buffer.push(self.char);
        self.skip();
    }

    fn consume_while<F>(&mut self, f: F)
    where
        F: Fn(u8) -> bool,
    {
        loop {
            if !f(self.char) || self.eof {
                break;
            }

            self.consume();
        }
    }

    fn buffer_to_string(&self) -> LexResult<String> {
        Ok(String::from_utf8(self.buffer.clone())?)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lex_stream() -> LexResult<()> {
        let mut lex = Lex::new(b"");
        assert_eq!(lex.next()?, Token::Eof);

        let mut lex = Lex::new(b"      ");
        assert_eq!(lex.next()?, Token::Eof);

        Ok(())
    }

    #[test]
    fn test_lex_name() -> LexResult<()> {
        let mut lex = Lex::new(b"abc");
        assert_eq!(lex.next()?, Token::Name("abc".to_string()));

        let mut lex = Lex::new(b"_abc_def");
        assert_eq!(lex.next()?, Token::Name("_abc_def".to_string()));

        Ok(())
    }

    #[test]
    fn test_lex_number() -> LexResult<()> {
        let mut lex = Lex::new(b"123");
        assert_eq!(lex.next()?, Token::Number("123".to_string()));

        let mut lex = Lex::new(b"123.456");
        assert_eq!(lex.next()?, Token::Number("123.456".to_string()));

        let mut lex = Lex::new(b"123e4");
        assert_eq!(lex.next()?, Token::Number("123e4".to_string()));

        let mut lex = Lex::new(b"123.456e7");
        assert_eq!(lex.next()?, Token::Number("123.456e7".to_string()));

        let mut lex = Lex::new(b"0x0123.456e7");
        assert_eq!(lex.next()?, Token::Number("0x0123.456e7".to_string()));

        let mut lex = Lex::new(b"0x0123.456e7");
        assert_eq!(lex.next()?, Token::Number("0x0123.456e7".to_string()));

        let mut lex = Lex::new(b"0x0123p456");
        assert_eq!(lex.next()?, Token::Number("0x0123p456".to_string()));

        let mut lex = Lex::new(b"0x0123p+456");
        assert_eq!(lex.next()?, Token::Number("0x0123p+456".to_string()));

        let mut lex = Lex::new(b"123e4.5");
        assert!(lex.next().is_err(), "123e4.5 is valid but shouldnt be");

        let mut lex = Lex::new(b"0x0FFFp4.5");
        assert!(lex.next().is_err(), "0x0FFFp4.5 is valid but shouldnt be");

        let mut lex = Lex::new(b"0x0FFFpAA");
        assert!(lex.next().is_err(), "0x0FFFpAA is valid but shouldnt be");

        Ok(())
    }

    #[test]
    fn test_lex_string() -> LexResult<()> {
        let mut lex = Lex::new(b"'some string'");
        assert_eq!(lex.next()?, Token::String("some string".to_string(), '\''));

        let mut lex = Lex::new(b"\"some \\\\string\"");
        assert_eq!(
            lex.next()?,
            Token::String("some \\\\string".to_string(), '\"')
        );

        let mut lex = Lex::new(b"[====[abcdef]====]");
        assert_eq!(lex.next()?, Token::MultilineString("abcdef".to_string(), 4));

        let mut lex = Lex::new(b"\"\"");
        assert_eq!(lex.next()?, Token::String("".to_string(), '\"'));

        let mut lex = Lex::new(b"[[]]");
        assert_eq!(lex.next()?, Token::MultilineString("".to_string(), 0));

        let mut lex = Lex::new(b"\"unclosed string'");
        assert!(lex.next().is_err(), "Unclosed string");

        let mut lex = Lex::new(b"[[unclosed mlstring]=]");
        assert!(lex.next().is_err(), "Unclosed mlstring");

        Ok(())
    }

    #[test]
    fn test_lex_labels() -> LexResult<()> {
        let mut lex = Lex::new(b"::labelname::");
        assert_eq!(lex.next()?, Token::Label("labelname".to_string()));

        let mut lex = Lex::new(b"::goto::");
        assert!(lex.next().is_err(), "Invalid labelname");

        let mut lex = Lex::new(b"::a");
        assert!(lex.next().is_err(), "Invalid label syntax");

        Ok(())
    }

    #[test]
    fn test_lex_symbols() -> LexResult<()> {
        let mut lex = Lex::new(b"+");
        assert_eq!(lex.next()?, Token::Add);
        let mut lex = Lex::new(b"-");
        assert_eq!(lex.next()?, Token::Subtract);
        let mut lex = Lex::new(b"*");
        assert_eq!(lex.next()?, Token::Multiply);
        let mut lex = Lex::new(b"/");
        assert_eq!(lex.next()?, Token::Divide);
        let mut lex = Lex::new(b"^");
        assert_eq!(lex.next()?, Token::Power);
        let mut lex = Lex::new(b"%");
        assert_eq!(lex.next()?, Token::Modulo);

        let mut lex = Lex::new(b">");
        assert_eq!(lex.next()?, Token::Greater);
        let mut lex = Lex::new(b"<");
        assert_eq!(lex.next()?, Token::Less);
        let mut lex = Lex::new(b">=");
        assert_eq!(lex.next()?, Token::GreaterEqual);
        let mut lex = Lex::new(b"<=");
        assert_eq!(lex.next()?, Token::LessEqual);

        let mut lex = Lex::new(b"==");
        assert_eq!(lex.next()?, Token::Equal);
        let mut lex = Lex::new(b"!=");
        assert_eq!(lex.next()?, Token::NotEqualGmod);
        let mut lex = Lex::new(b"~=");
        assert_eq!(lex.next()?, Token::NotEqual);
        let mut lex = Lex::new(b"&&");
        assert_eq!(lex.next()?, Token::AndGmod);
        let mut lex = Lex::new(b"||");
        assert_eq!(lex.next()?, Token::OrGmod);
        let mut lex = Lex::new(b"..");
        assert_eq!(lex.next()?, Token::Concat);

        let mut lex = Lex::new(b"!");
        assert_eq!(lex.next()?, Token::NotGmod);
        let mut lex = Lex::new(b"#");
        assert_eq!(lex.next()?, Token::Length);

        let mut lex = Lex::new(b"=");
        assert_eq!(lex.next()?, Token::Assign);
        let mut lex = Lex::new(b".");
        assert_eq!(lex.next()?, Token::Dot);
        let mut lex = Lex::new(b"...");
        assert_eq!(lex.next()?, Token::Dots);

        let mut lex = Lex::new(b";");
        assert_eq!(lex.next()?, Token::SemiColon);
        let mut lex = Lex::new(b",");
        assert_eq!(lex.next()?, Token::Comma);
        let mut lex = Lex::new(b":");
        assert_eq!(lex.next()?, Token::Colon);

        let mut lex = Lex::new(b"{");
        assert_eq!(lex.next()?, Token::LeftCurly);
        let mut lex = Lex::new(b"}");
        assert_eq!(lex.next()?, Token::RightCurly);
        let mut lex = Lex::new(b"[");
        assert_eq!(lex.next()?, Token::LeftSquare);
        let mut lex = Lex::new(b"]");
        assert_eq!(lex.next()?, Token::RightSquare);
        let mut lex = Lex::new(b"(");
        assert_eq!(lex.next()?, Token::LeftParen);
        let mut lex = Lex::new(b")");
        assert_eq!(lex.next()?, Token::RightParen);

        Ok(())
    }

    #[test]
    fn test_lex_keywords() -> LexResult<()> {
        let mut lex = Lex::new(b"break");
        assert_eq!(lex.next()?, Token::Break);
        let mut lex = Lex::new(b"do");
        assert_eq!(lex.next()?, Token::Do);
        let mut lex = Lex::new(b"else");
        assert_eq!(lex.next()?, Token::Else);
        let mut lex = Lex::new(b"elseif");
        assert_eq!(lex.next()?, Token::ElseIf);
        let mut lex = Lex::new(b"end");
        assert_eq!(lex.next()?, Token::End);
        let mut lex = Lex::new(b"for");
        assert_eq!(lex.next()?, Token::For);
        let mut lex = Lex::new(b"function");
        assert_eq!(lex.next()?, Token::Function);
        let mut lex = Lex::new(b"if");
        assert_eq!(lex.next()?, Token::If);
        let mut lex = Lex::new(b"in");
        assert_eq!(lex.next()?, Token::In);
        let mut lex = Lex::new(b"local");
        assert_eq!(lex.next()?, Token::Local);
        let mut lex = Lex::new(b"repeat");
        assert_eq!(lex.next()?, Token::Repeat);
        let mut lex = Lex::new(b"return");
        assert_eq!(lex.next()?, Token::Return);
        let mut lex = Lex::new(b"then");
        assert_eq!(lex.next()?, Token::Then);
        let mut lex = Lex::new(b"until");
        assert_eq!(lex.next()?, Token::Until);
        let mut lex = Lex::new(b"while");
        assert_eq!(lex.next()?, Token::While);
        let mut lex = Lex::new(b"goto");
        assert_eq!(lex.next()?, Token::Goto);

        let mut lex = Lex::new(b"and");
        assert_eq!(lex.next()?, Token::And);
        let mut lex = Lex::new(b"or");
        assert_eq!(lex.next()?, Token::Or);
        let mut lex = Lex::new(b"not");
        assert_eq!(lex.next()?, Token::Not);

        let mut lex = Lex::new(b"true");
        assert_eq!(lex.next()?, Token::True);
        let mut lex = Lex::new(b"false");
        assert_eq!(lex.next()?, Token::False);
        let mut lex = Lex::new(b"nil");
        assert_eq!(lex.next()?, Token::Nil);

        Ok(())
    }
}
