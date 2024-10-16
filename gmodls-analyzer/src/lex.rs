use crate::macros::*;
use crate::source::Source;
use crate::span::{Pos, Span};
use std::string::FromUtf8Error;

const EOF: u8 = 0;

/// Represents a token in the Lua language.
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Token {
    Eof,
    Unknown(u8),

    // Primitives
    Name(String),
    Number(String),
    String(String, u8), // Contents and the character the string is created with, eg. ' or "
    MultilineString(String, u16), // Contents and how many ='s?
    Label(String),      // ::label::, span should encompass everything including ::'s
    CommentGmod(String),
    MultiLineCommentGmod(String),
    Comment(String),
    MultiLineComment(String, u16),

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
    Continue,

    // Binary operators
    Add,      // +
    Subtract, // -
    Multiply, // *
    Divide,   // /
    Power,    // ^
    Modulo,   // %

    GreaterThan,      // >
    LessThan,         // <
    GreaterThanEqual, // >=
    LessThanEqual,    // <=

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

/// Represents a group of comments and any leading and trailing whitespace.
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct CommentWhitespace {
    start: Pos,
    end: Pos,

    comments: Vec<(Token, Span)>,
}

/// Error type used by [`Lex`].
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

    #[error("Encountered an unexpected end of file while parsing, \"{value}\"")]
    UnexpectedEOF { value: String },

    #[error("Encountered an invalid label while parsing, \"{value}\": {message}")]
    InvalidLabel { value: String, message: String },
}

/// Result type used by [`Lex`].
pub type LexResult<T> = Result<T, LexError>;

/// Implements the lexical analysis stage of the overall analyzer.
///
/// The purpose of this stage is to split the file into [`Token`]s to be later used by the [`Parser`].
/// Each token is given an associated [`Span`] representing its location within the source file.
///
/// Comments are stored within a [`CommentWhitespace`] which represents sections of whitespace
/// containing comments. When a [`Token`] is taken via [`Lex::next`] all proceeding comments after the
/// previous [`Token`], including leading and trailing whitespace, are stored within a [`CommentWhitespace`].
///
/// # Examples
///
/// ```
/// use gmodls_analyzer::{
///     source::Source,
///     lex::{Lex, Token},
///     pos, span, src
/// };
///
/// let source = src!(br#"
/// local a = "Hello, world!"
/// "#);
/// let mut lex = Lex::new(&source);
///
/// assert_eq!(lex.next()?, (
///     Token::Local,
///     span!(
///         pos!(1, 1, 0),
///         pos!(5, 1, 4)
///     )
/// ));
/// ```
#[derive(Debug)]
pub struct Lex<'source> {
    source: &'source Source,

    index: usize,

    row: usize,
    col: usize,

    char: u8,

    current: Option<(Token, Span)>,
    lookahead: Option<(Token, Span)>,

    buffer: Vec<u8>,
    comments: Vec<CommentWhitespace>,
}

impl<'source> Lex<'source> {
    /// Returns a new [`Lex`] with the given `source` reference.
    pub fn new(source: &'source Source) -> Self {
        Self {
            source,

            index: 0,

            row: 0,
            col: 0,

            char: if source.len() == 0 { 0 } else { source[0] },

            current: None,
            lookahead: None,

            comments: vec![],
            buffer: vec![],
        }
    }

    /// Returns the current [`Token`] and [`Span`] tuple (the last set emitted by [`Lex::next`]).
    ///
    /// # Errors
    ///
    /// Panics if the first [`Token`] hasn't been read.
    pub fn current(&self) -> (Token, Span) {
        self.current
            .as_ref()
            .expect("current cannot be called until the first token is read")
            .clone()
    }

    /// Returns the current [`Token`] (the last emitted by [`Lex::next`]).
    ///
    /// # Errors
    ///
    /// Panics if the first [`Token`] hasn't been read.
    pub fn current_token(&self) -> Token {
        self.current
            .as_ref()
            .expect("current cannot be called until the first token is read")
            .0
            .clone()
    }

    /// Returns the current [`Span`] (the last emitted by [`Lex::next`]).
    ///
    /// # Errors
    ///
    /// Panics if the first [`Token`] hasn't been read.
    pub fn current_span(&self) -> Span {
        self.current
            .as_ref()
            .expect("current cannot be called until the first token is read")
            .1
    }

    /// Returns the lookahead [`Token`] and [`Span`] tuple (the next set to be emitted next by [`Lex::next`]).
    pub fn lookahead(&mut self) -> LexResult<(Token, Span)> {
        match &self.lookahead {
            Some(inner) => return Ok(inner.clone()),
            None => {}
        };

        self.lookahead = Some(self.scan()?);

        Ok(self.lookahead.as_ref().expect("lookahead is none").clone())
    }
    
    pub fn lookahead_token(&mut self) -> LexResult<Token> {
        Ok(self.lookahead()?.0)
    }

    pub fn lookahead_span(&mut self) -> LexResult<Span> {
        Ok(self.lookahead()?.1)
    }

    /// Returns a [`&Vec<CommentWhitespace>`] representing the comment stack.
    pub fn comments(&self) -> &Vec<CommentWhitespace> {
        &self.comments
    }

    /// Returns the next [`Token`] found in the source alongside it's associated [`Span`].
    ///
    /// Any comments processed prior to the [`Token`] will be stored within the comment stack
    /// accessible via the [`Lex::comments`] method.
    pub fn next(&mut self) -> LexResult<(Token, Span)> {
        if self.lookahead.is_some() {
            self.current = self.lookahead.clone();
            self.lookahead = None;
        } else {
            let result = self.scan()?;
            self.current = Some(result.clone());
        }
        Ok(self
            .current
            .as_ref()
            .expect("current token/span wasn't set")
            .clone())
    }

    fn scan(&mut self) -> LexResult<(Token, Span)> {
        self.buffer.clear();

        // Scan for whitespace characters and comments.
        if let Some(result) = self.scan_whitespace()? {
            return Ok(result);
        }

        macro_rules! match_double {
            ($ch2:expr, $tk1:expr, $tk2:expr) => {{
                let start = self.pos();

                match self.skip() {
                    $ch2 => {
                        let end = self.pos();

                        self.skip();

                        ($tk2, self.span(start, end))
                    }
                    _ => ($tk1, self.span(start, start)),
                }
            }};
        }

        let result = match self.char {
            EOF => (Token::Eof, self.span(self.pos(), self.pos())),
            // Match keywords or identifiers.
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => self.scan_keyword_and_name()?,
            // Match numbers.
            b'0'..=b'9' => self.scan_number()?,
            // Match strings.
            b'\'' | b'"' => self.scan_string()?,
            // Match multi-line strings.
            b'[' => {
                let start = self.pos();

                self.consume();

                match self.char {
                    b'[' | b'=' => {
                        let (contents, n, end) = if let Some(inner) = self.scan_long()? {
                            inner
                        } else {
                            return Err(LexError::MalformedString {
                                value: self.buffer_to_string()?,
                                message: "Malformed long string delimiter.".to_string(),
                            });
                        };

                        (Token::MultilineString(contents, n), self.span(start, end))
                    }
                    _ => (Token::LeftSquare, self.span(start, start)),
                }
            }
            // Match =, ==.
            b'=' => match_double!(b'=', Token::Assign, Token::Equal),
            // Match !, !=.
            b'!' => match_double!(b'=', Token::NotGmod, Token::NotEqualGmod),
            // Match ~=.
            b'~' => match_double!(b'=', Token::Unknown(b'~'), Token::NotEqual),
            // Match >, >=.
            b'>' => match_double!(b'=', Token::GreaterThan, Token::GreaterThanEqual),
            // Match <, <=.
            b'<' => match_double!(b'=', Token::LessThan, Token::LessThanEqual),
            // Match ||.
            b'|' => match_double!(b'|', Token::Unknown(b'|'), Token::OrGmod),
            // Match &&.
            b'&' => match_double!(b'&', Token::Unknown(b'&'), Token::AndGmod),
            // Match ., .., ....
            b'.' => {
                let start = self.pos();

                match self.skip() {
                    b'.' => {
                        let end = self.pos();

                        match self.skip() {
                            b'.' => {
                                let end = self.pos();

                                self.skip();

                                (Token::Dots, self.span(start, end))
                            }
                            _ => (Token::Concat, self.span(start, end)),
                        }
                    }
                    _ => (Token::Dot, self.span(start, start)),
                }
            }
            // Match :, ::NAME::.
            b':' => {
                let start = self.pos();
                self.skip(); // ':'

                match (self.char, lower!(self.peek())) {
                    (b':', b'a'..=b'z' | b'_') => self.scan_label(start)?,
                    _ => (Token::Colon, self.span(start, start)),
                }
            }
            // Match single/unknown tokens.
            char => {
                let start = self.pos();

                self.skip();

                let token = match char {
                    b'+' => Token::Add,
                    b'*' => Token::Multiply,
                    b'^' => Token::Power,
                    b'%' => Token::Modulo,
                    b'#' => Token::Length,
                    b';' => Token::SemiColon,
                    b',' => Token::Comma,
                    b'{' => Token::LeftCurly,
                    b'}' => Token::RightCurly,
                    b']' => Token::RightSquare,
                    b'(' => Token::LeftParen,
                    b')' => Token::RightParen,
                    _ => Token::Unknown(char),
                };

                (token, self.span(start, start))
            }
        };

        Ok(result)
    }

    fn scan_whitespace(&mut self) -> LexResult<Option<(Token, Span)>> {
        let mut comments = vec![];
        let ws_start = self.pos();
        let mut ws_end = self.pos();
        loop {
            self.buffer.clear();

            let (token, span) = match self.char {
                b' ' | b'\n' | b'\r' => {
                    ws_end = self.pos();
                    self.skip();
                    continue;
                }
                b'/' | b'-' => {
                    let start = self.pos();
                    let delim = self.char;

                    match (delim, self.skip()) {
                        (b'-', b'-') => self.scan_comment(start)?,
                        (b'/', b'/') => self.scan_comment_gmod(start)?,
                        (b'/', b'*') => self.scan_long_comment_gmod(start)?,
                        (b'/', _) => return Ok(Some((Token::Divide, self.span(start, start)))),
                        (b'-', _) => return Ok(Some((Token::Subtract, self.span(start, start)))),
                        (_, _) => panic!(),
                    }
                }
                _ => break,
            };

            ws_end = span.end();

            comments.push((token, span));
        }

        if comments.len() > 0 {
            self.comments.push(CommentWhitespace {
                comments,
                start: ws_start,
                end: ws_end,
            });
        }

        Ok(None)
    }

    fn scan_keyword_and_name(&mut self) -> LexResult<(Token, Span)> {
        let start = self.pos();

        // Consume valid identifier chars.
        let end = self.consume_while(|c| is_alphanumeric!(c) || c == b'_');

        // Match for a keyword, otherwise create a name token.
        let token = match &*self.buffer {
            b"true" => Token::True,
            b"false" => Token::False,
            b"nil" => Token::Nil,

            b"break" => Token::Break,
            b"do" => Token::Do,
            b"else" => Token::Else,
            b"elseif" => Token::ElseIf,
            b"end" => Token::End,
            b"for" => Token::For,
            b"function" => Token::Function,
            b"if" => Token::If,
            b"in" => Token::In,
            b"local" => Token::Local,
            b"repeat" => Token::Repeat,
            b"return" => Token::Return,
            b"then" => Token::Then,
            b"until" => Token::Until,
            b"while" => Token::While,
            b"goto" => Token::Goto,
            b"continue" => Token::Continue,

            b"and" => Token::And,
            b"or" => Token::Or,
            b"not" => Token::Not,

            _ => Token::Name(String::from_utf8(self.buffer.clone())?),
        };

        Ok((token, self.span(start, end)))
    }

    fn scan_number(&mut self) -> LexResult<(Token, Span)> {
        let start = self.pos();

        let mut xp = b'e';

        // Check for '0x' or '0X'.
        if self.char == b'0' {
            self.consume();

            if lower!(self.char) == b'x' {
                self.consume();
                xp = b'p';
            }
        }

        // Read alphanumeric characters.
        let mut prev_char = 0u8;
        let mut end = self.pos();
        loop {
            if !(is_alphanumeric!(self.char)
                || self.char == b'_'
                || self.char == b'.'
                || ((self.char == b'-' || self.char == b'+') && lower!(prev_char) == xp))
            {
                break;
            }

            prev_char = self.char;
            end = self.pos();
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

            let char = lower!(self.buffer[i]);

            match (xp, char) {
                (b'e', b'0'..=b'9') => {}
                (b'p', b'0'..=b'9' | b'a'..=b'f') => {
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

        Ok((
            Token::Number(self.buffer_to_string()?),
            self.span(start, end),
        ))
    }

    fn scan_string(&mut self) -> LexResult<(Token, Span)> {
        let quote_char = self.char;

        let start = self.pos();

        self.skip();

        while self.char != quote_char {
            match self.char {
                EOF => {
                    return Err(LexError::MalformedString {
                        value: self.buffer_to_string()?,
                        message: "Unexpected end of file.".to_string(),
                    });
                }
                b'\r' | b'\n' => {
                    return Err(LexError::MalformedString {
                        value: self.buffer_to_string()?,
                        message: "Unexpected new line.".to_string(),
                    });
                }
                b'\\' => {
                    self.skip();

                    macro_rules! convert {
                        ($d:expr) => {
                            match lower!($d) {
                                b'0'..=b'9' => $d - b'0',
                                b'a'..=b'f' => $d - b'a',
                                _ => {
                                    return Err(LexError::MalformedString {
                                        value: self.buffer_to_string()?,
                                        message: "Invalid hexadecimal escape sequence.".to_string(),
                                    })
                                }
                            }
                        };
                    }

                    let real_char = match self.char {
                        EOF => {
                            return Err(LexError::MalformedString {
                                value: self.buffer_to_string()?,
                                message: "Unexpected end of file.".to_string(),
                            });
                        }
                        b'\\' => b'\\',
                        b'\'' => b'\'',
                        b'"' => b'"',
                        b'a' => 0x07,
                        b'b' => 0x08,
                        b'f' => 0x0C,
                        b'n' => b'\n',
                        b'r' => b'\r',
                        b't' => b'\t',
                        b'v' => 0x0B,
                        b'\n' | b'\r' => b'\n',
                        b'x' => {
                            self.skip();
                            let hi = lower!(self.char);
                            self.skip();
                            let lo = lower!(self.char);

                            let hi_value = convert!(hi);
                            let lo_value = convert!(lo);

                            lo_value + (hi_value << 4u8)
                        }
                        b'u' => todo!(),
                        b'z' => {
                            self.skip();
                            self.skip_while(|c| is_whitespace!(c));
                            continue;
                        }
                        _ => {
                            return Err(LexError::MalformedString {
                                value: self.buffer_to_string()?,
                                message: "Unexpected escape character.".to_string(),
                            });
                        }
                    };

                    self.buffer.push(real_char);

                    self.skip();
                }
                _ => {
                    self.consume();
                }
            }
        }

        let end = self.pos();

        self.skip();

        Ok((
            Token::String(self.buffer_to_string()?, quote_char),
            self.span(start, end),
        ))
    }

    fn scan_long(&mut self) -> LexResult<Option<(String, u16, Pos)>> {
        let n: u16 = match self.char {
            b'[' => 0,
            _ => {
                let mut n = 0;
                while self.char == b'=' {
                    n += 1;

                    self.consume();
                }

                match self.char {
                    b'[' => {}
                    _ => return Ok(None),
                }

                n
            }
        };

        self.consume(); // '['

        let end = 'outer: loop {
            match self.char {
                b']' => {
                    self.consume();

                    let mut c = 0;

                    'inner: loop {
                        if c == n {
                            break 'inner;
                        }

                        if self.char == b'=' {
                            c += 1;
                        } else {
                            continue 'outer;
                        }

                        self.consume();
                    }

                    if c != n || self.char != b']' {
                        continue;
                    }

                    let end = self.pos();

                    self.consume();

                    break end;
                }
                EOF => {
                    return Err(LexError::UnexpectedEOF {
                        value: self.buffer_to_string()?,
                    });
                }
                _ => {
                    self.consume();
                }
            }
        };

        let contents = self.buffer_to_string()?;
        let contents = String::from(&contents[n as usize + 2..(contents.len() - n as usize - 2)]);

        Ok(Some((contents, n, end)))
    }

    fn scan_label(&mut self, start: Pos) -> LexResult<(Token, Span)> {
        self.skip(); // ':'

        self.consume_while(|c| is_alphanumeric!(c) || c == b'_'); // NAME

        macro_rules! check {
            ($c:expr) => {
                match $c {
                    b':' => {}
                    _ => {
                        return Err(LexError::InvalidLabel {
                            value: self.buffer_to_string()?,
                            message: "Invalid ending sequence for label name.".to_string(),
                        })
                    }
                }
            };
        }

        check!(self.char);
        check!(self.skip());

        let end = self.pos();

        self.skip();

        let contents = self.buffer_to_string()?;

        if is_reserved!(contents.as_str()) {
            return Err(LexError::InvalidLabel {
                value: contents,
                message: "A keyword cannot be used as the name for a label.".to_string(),
            });
        }

        Ok((Token::Label(contents), self.span(start, end)))
    }

    fn scan_comment(&mut self, start: Pos) -> LexResult<(Token, Span)> {
        self.skip(); // '-'

        match self.char {
            b'[' => {
                self.consume();

                if let Some((contents, n, end)) = self.scan_long()? {
                    return Ok((Token::MultiLineComment(contents, n), self.span(start, end)));
                }
            }
            _ => {}
        }

        let end = self.consume_while(|c| c != b'\n');

        Ok((
            Token::Comment(self.buffer_to_string()?),
            self.span(start, end),
        ))
    }

    fn scan_comment_gmod(&mut self, start: Pos) -> LexResult<(Token, Span)> {
        self.skip(); // '/'

        let end = self.consume_while(|c| c != b'\n');

        Ok((
            Token::CommentGmod(self.buffer_to_string()?),
            self.span(start, end),
        ))
    }

    fn scan_long_comment_gmod(&mut self, start: Pos) -> LexResult<(Token, Span)> {
        self.skip(); // '*'

        loop {
            match self.char {
                b'*' => {
                    self.consume(); // '*'

                    match self.char {
                        EOF => {
                            return Err(LexError::UnexpectedEOF {
                                value: self.buffer_to_string()?,
                            })
                        }
                        b'/' => break,
                        _ => continue,
                    }
                }
                _ => {
                    self.consume();
                }
            }
        }

        let end = self.pos();

        self.consume(); // '/'

        let contents = self.buffer_to_string()?;
        let contents = String::from(&contents[..contents.len() - 2]);

        Ok((Token::MultiLineCommentGmod(contents), self.span(start, end)))
    }

    #[inline]
    fn pos(&self) -> Pos {
        Pos::new(self.index, self.row, self.col)
    }

    #[inline]
    fn span(&self, start: Pos, end: Pos) -> Span {
        Span::new(start, end)
    }

    #[inline]
    fn peek(&mut self) -> u8 {
        if self.index + 1 >= self.source.len() {
            EOF
        } else {
            self.source[self.index + 1]
        }
    }

    #[inline]
    fn skip(&mut self) -> u8 {
        if self.index + 1 >= self.source.len() {
            self.char = EOF;
        } else {
            self.index += 1;
            self.col += 1;

            if self.char == b'\n' {
                self.col = 0;
                self.row += 1;
            }

            self.char = self.source[self.index];
        }

        self.char
    }

    #[inline]
    fn skip_while<F>(&mut self, f: F) -> Pos
    where
        F: Fn(u8) -> bool,
    {
        let mut end = self.pos();

        loop {
            if !f(self.char) || self.char == EOF {
                break;
            }

            end = self.pos();

            self.skip();
        }

        end
    }

    #[inline]
    fn consume(&mut self) -> u8 {
        self.buffer.push(self.char);
        self.skip()
    }

    #[inline]
    fn consume_while<F>(&mut self, f: F) -> Pos
    where
        F: Fn(u8) -> bool,
    {
        let mut end = self.pos();
        loop {
            if !f(self.char) || self.char == EOF {
                break;
            }

            end = self.pos();
            self.consume();
        }

        end
    }

    #[inline]
    fn buffer_to_string(&self) -> LexResult<String> {
        Ok(String::from_utf8(self.buffer.clone())?)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{pos, span, src};

    #[test]
    fn test_lex_comments() -> LexResult<()> {
        Ok(())
    }

    #[test]
    fn test_lex_name() -> LexResult<()> {
        let source = src!(b"abc _abc_def");
        let mut lex = Lex::new(&source);

        assert_eq!(
            lex.next()?,
            (
                Token::Name("abc".to_string()),
                span!(pos!(0, 0, 0), pos!(2, 0, 2))
            )
        );
        assert_eq!(
            lex.next()?,
            (
                Token::Name("_abc_def".to_string()),
                span!(pos!(4, 0, 4), pos!(11, 0, 11))
            )
        );

        Ok(())
    }

    #[test]
    fn test_lex_number() -> LexResult<()> {
        let source = src!(
            br"
123
123.456
123e4
123.456e7
0x0123.456e7
0x0123p456
0x0123p+456
123e4.5
0x0FFFp4.5
0x0FFFpAA
            "
        );
        let mut lex = Lex::new(&source);

        assert_eq!(
            lex.next()?,
            (
                Token::Number("123".to_string()),
                span!(pos!(1, 1, 0), pos!(3, 1, 2))
            )
        );
        assert_eq!(
            lex.next()?,
            (
                Token::Number("123.456".to_string()),
                span!(pos!(5, 2, 0), pos!(11, 2, 6))
            )
        );
        assert_eq!(
            lex.next()?,
            (
                Token::Number("123e4".to_string()),
                span!(pos!(13, 3, 0), pos!(17, 3, 4))
            )
        );
        assert_eq!(
            lex.next()?,
            (
                Token::Number("123.456e7".to_string()),
                span!(pos!(19, 4, 0), pos!(27, 4, 8))
            )
        );
        assert_eq!(
            lex.next()?,
            (
                Token::Number("0x0123.456e7".to_string()),
                span!(pos!(29, 5, 0), pos!(40, 5, 11))
            )
        );
        assert_eq!(
            lex.next()?,
            (
                Token::Number("0x0123p456".to_string()),
                span!(pos!(42, 6, 0), pos!(51, 6, 9))
            )
        );
        assert_eq!(
            lex.next()?,
            (
                Token::Number("0x0123p+456".to_string()),
                span!(pos!(53, 7, 0), pos!(63, 7, 10))
            )
        );

        assert!(lex.next().is_err(), "123e4.5 is valid but shouldn't be");
        assert!(lex.next().is_err(), "0x0FFFp4.5 is valid but shouldn't be");
        assert!(lex.next().is_err(), "0x0FFFpAA is valid but shouldnt be");

        Ok(())
    }

    #[test]
    fn test_lex_string() -> LexResult<()> {
        let source = src!(
            br#"
'some string'
"some \\\\string"
"some \bstring"
"\x41 \x42 \x43"
[====[abc]===========]def]====]
""
[[]]
"unclosed string
[[unclosed mlstring]=]
            "#
        );
        let mut lex = Lex::new(&source);

        assert_eq!(
            lex.next()?,
            (
                Token::String("some string".to_string(), b'\''),
                span!(pos!(1, 1, 0), pos!(13, 1, 12))
            )
        );
        assert_eq!(
            lex.next()?,
            (
                Token::String(r#"some \\string"#.to_string(), b'\"'),
                span!(pos!(15, 2, 0), pos!(31, 2, 16))
            )
        );
        assert_eq!(
            lex.next()?,
            (
                Token::String("some \x08string".to_string(), b'\"'),
                span!(pos!(33, 3, 0), pos!(47, 3, 14))
            )
        );
        assert_eq!(
            lex.next()?,
            (
                Token::String("A B C".to_string(), b'\"'),
                span!(pos!(49, 4, 0), pos!(64, 4, 15))
            )
        );
        assert_eq!(
            lex.next()?,
            (
                Token::MultilineString("abc]===========]def".to_string(), 4),
                span!(pos!(66, 5, 0), pos!(96, 5, 30))
            )
        );
        assert_eq!(
            lex.next()?,
            (
                Token::String("".to_string(), b'\"'),
                span!(pos!(98, 6, 0), pos!(99, 6, 1))
            )
        );
        assert_eq!(
            lex.next()?,
            (
                Token::MultilineString("".to_string(), 0),
                span!(pos!(101, 7, 0), pos!(104, 7, 3))
            )
        );
        assert!(lex.next().is_err(), "Unclosed string");
        assert!(lex.next().is_err(), "Unclosed mlstring");

        Ok(())
    }

    #[test]
    fn test_lex_labels() -> LexResult<()> {
        let source = src!(
            br#"
::labelname::
::goto::
::a
            "#
        );
        let mut lex = Lex::new(&source);

        assert_eq!(
            lex.next()?,
            (
                Token::Label("labelname".to_string()),
                span!(pos!(1, 1, 0), pos!(13, 1, 12))
            )
        );
        assert!(lex.next().is_err(), "Invalid labelname");
        assert!(lex.next().is_err(), "Invalid label syntax");

        Ok(())
    }

    #[test]
    fn test_lex_symbols() -> LexResult<()> {
        let source = src!(b"+ - * / ^ % > < >= <= == != ~= && || .. ! # = . ... ; , : { } [ ] ( )");
        let mut lex = Lex::new(&source);

        assert_eq!(
            lex.next()?,
            (Token::Add, span!(pos!(0, 0, 0), pos!(0, 0, 0)))
        );
        assert_eq!(
            lex.next()?,
            (Token::Subtract, span!(pos!(2, 0, 2), pos!(2, 0, 2)))
        );
        assert_eq!(
            lex.next()?,
            (Token::Multiply, span!(pos!(4, 0, 4), pos!(4, 0, 4)))
        );
        assert_eq!(
            lex.next()?,
            (Token::Divide, span!(pos!(6, 0, 6), pos!(6, 0, 6)))
        );
        assert_eq!(
            lex.next()?,
            (Token::Power, span!(pos!(8, 0, 8), pos!(8, 0, 8)))
        );
        assert_eq!(
            lex.next()?,
            (Token::Modulo, span!(pos!(10, 0, 10), pos!(10, 0, 10)))
        );

        assert_eq!(
            lex.next()?,
            (Token::GreaterThan, span!(pos!(12, 0, 12), pos!(12, 0, 12)))
        );
        assert_eq!(
            lex.next()?,
            (Token::LessThan, span!(pos!(14, 0, 14), pos!(14, 0, 14)))
        );
        assert_eq!(
            lex.next()?,
            (Token::GreaterThanEqual, span!(pos!(16, 0, 16), pos!(17, 0, 17)))
        );
        assert_eq!(
            lex.next()?,
            (Token::LessThanEqual, span!(pos!(19, 0, 19), pos!(20, 0, 20)))
        );

        assert_eq!(
            lex.next()?,
            (Token::Equal, span!(pos!(22, 0, 22), pos!(23, 0, 23)))
        );
        assert_eq!(
            lex.next()?,
            (Token::NotEqualGmod, span!(pos!(25, 0, 25), pos!(26, 0, 26)))
        );
        assert_eq!(
            lex.next()?,
            (Token::NotEqual, span!(pos!(28, 0, 28), pos!(29, 0, 29)))
        );
        assert_eq!(
            lex.next()?,
            (Token::AndGmod, span!(pos!(31, 0, 31), pos!(32, 0, 32)))
        );
        assert_eq!(
            lex.next()?,
            (Token::OrGmod, span!(pos!(34, 0, 34), pos!(35, 0, 35)))
        );
        assert_eq!(
            lex.next()?,
            (Token::Concat, span!(pos!(37, 0, 37), pos!(38, 0, 38)))
        );

        assert_eq!(
            lex.next()?,
            (Token::NotGmod, span!(pos!(40, 0, 40), pos!(40, 0, 40)))
        );
        assert_eq!(
            lex.next()?,
            (Token::Length, span!(pos!(42, 0, 42), pos!(42, 0, 42)))
        );

        assert_eq!(
            lex.next()?,
            (Token::Assign, span!(pos!(44, 0, 44), pos!(44, 0, 44)))
        );
        assert_eq!(
            lex.next()?,
            (Token::Dot, span!(pos!(46, 0, 46), pos!(46, 0, 46)))
        );
        assert_eq!(
            lex.next()?,
            (Token::Dots, span!(pos!(48, 0, 48), pos!(50, 0, 50)))
        );

        assert_eq!(
            lex.next()?,
            (Token::SemiColon, span!(pos!(52, 0, 52), pos!(52, 0, 52)))
        );
        assert_eq!(
            lex.next()?,
            (Token::Comma, span!(pos!(54, 0, 54), pos!(54, 0, 54)))
        );
        assert_eq!(
            lex.next()?,
            (Token::Colon, span!(pos!(56, 0, 56), pos!(56, 0, 56)))
        );

        assert_eq!(
            lex.next()?,
            (Token::LeftCurly, span!(pos!(58, 0, 58), pos!(58, 0, 58)))
        );
        assert_eq!(
            lex.next()?,
            (Token::RightCurly, span!(pos!(60, 0, 60), pos!(60, 0, 60)))
        );
        assert_eq!(
            lex.next()?,
            (Token::LeftSquare, span!(pos!(62, 0, 62), pos!(62, 0, 62)))
        );
        assert_eq!(
            lex.next()?,
            (Token::RightSquare, span!(pos!(64, 0, 64), pos!(64, 0, 64)))
        );
        assert_eq!(
            lex.next()?,
            (Token::LeftParen, span!(pos!(66, 0, 66), pos!(66, 0, 66)))
        );
        assert_eq!(
            lex.next()?,
            (Token::RightParen, span!(pos!(68, 0, 68), pos!(68, 0, 68)))
        );

        Ok(())
    }

    #[test]
    fn test_lex_keywords() -> LexResult<()> {
        let source = src!(b"break do else elseif end for function if in local repeat return then until while goto and or not true false nil");
        let mut lex = Lex::new(&source);

        assert_eq!(
            lex.next()?,
            (Token::Break, span!(pos!(0, 0, 0), pos!(4, 0, 4)))
        );
        assert_eq!(
            lex.next()?,
            (Token::Do, span!(pos!(6, 0, 6), pos!(7, 0, 7)))
        );
        assert_eq!(
            lex.next()?,
            (Token::Else, span!(pos!(9, 0, 9), pos!(12, 0, 12)))
        );
        assert_eq!(
            lex.next()?,
            (Token::ElseIf, span!(pos!(14, 0, 14), pos!(19, 0, 19)))
        );
        assert_eq!(
            lex.next()?,
            (Token::End, span!(pos!(21, 0, 21), pos!(23, 0, 23)))
        );
        assert_eq!(
            lex.next()?,
            (Token::For, span!(pos!(25, 0, 25), pos!(27, 0, 27)))
        );
        assert_eq!(
            lex.next()?,
            (Token::Function, span!(pos!(29, 0, 29), pos!(36, 0, 36)))
        );
        assert_eq!(
            lex.next()?,
            (Token::If, span!(pos!(38, 0, 38), pos!(39, 0, 39)))
        );
        assert_eq!(
            lex.next()?,
            (Token::In, span!(pos!(41, 0, 41), pos!(42, 0, 42)))
        );
        assert_eq!(
            lex.next()?,
            (Token::Local, span!(pos!(44, 0, 44), pos!(48, 0, 48)))
        );
        assert_eq!(
            lex.next()?,
            (Token::Repeat, span!(pos!(50, 0, 50), pos!(55, 0, 55)))
        );
        assert_eq!(
            lex.next()?,
            (Token::Return, span!(pos!(57, 0, 57), pos!(62, 0, 62)))
        );
        assert_eq!(
            lex.next()?,
            (Token::Then, span!(pos!(64, 0, 64), pos!(67, 0, 67)))
        );
        assert_eq!(
            lex.next()?,
            (Token::Until, span!(pos!(69, 0, 69), pos!(73, 0, 73)))
        );
        assert_eq!(
            lex.next()?,
            (Token::While, span!(pos!(75, 0, 75), pos!(79, 0, 79)))
        );
        assert_eq!(
            lex.next()?,
            (Token::Goto, span!(pos!(81, 0, 81), pos!(84, 0, 84)))
        );

        assert_eq!(
            lex.next()?,
            (Token::And, span!(pos!(86, 0, 86), pos!(88, 0, 88)))
        );
        assert_eq!(
            lex.next()?,
            (Token::Or, span!(pos!(90, 0, 90), pos!(91, 0, 91)))
        );
        assert_eq!(
            lex.next()?,
            (Token::Not, span!(pos!(93, 0, 93), pos!(95, 0, 95)))
        );

        assert_eq!(
            lex.next()?,
            (Token::True, span!(pos!(97, 0, 97), pos!(100, 0, 100)))
        );
        assert_eq!(
            lex.next()?,
            (Token::False, span!(pos!(102, 0, 102), pos!(106, 0, 106)))
        );
        assert_eq!(
            lex.next()?,
            (Token::Nil, span!(pos!(108, 0, 108), pos!(110, 0, 110)))
        );

        Ok(())
    }
}
