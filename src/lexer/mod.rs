mod errors;
mod token;
mod cursor;
mod tests;

pub use errors::LexError;
pub use token::*;

use cursor::Cursor;
use errors::LexError::*;

pub struct Lexer<'src> {
    src: Cursor<'src>
}

impl<'a> From<&'a str> for Lexer<'a> {
    fn from(s: &'a str) -> Self {
        Self {
            src: Cursor::new(s)
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token, LexError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.lex_token()
    }
}

impl Lexer<'_> {
    fn lex_token(&mut self) -> Option<Result<Token, LexError>> {
        match self.src.peek()? {
            c if c.is_ascii_whitespace() => {
                self.src.next();
                self.lex_token()
            },
            c if c.is_ascii_alphabetic() => {
                Some(self.parse_keyword_or_ident())
            },
            '\"' => Some(StringLiteralCollector::new(&mut self.src).collect()),
            '/' if self.src.lookahead(1) == Some('*') => {
                self.skip_comment();
                self.lex_token()
            },
            '0'..='9' => Some(NumberLiteralCollector::new(&mut self.src).collect()),
            _ => self.parse_single_char(),
        }
    }

    fn skip_comment(&mut self) {
        loop {
            self.src.next();

            if self.src.match_ch('*') && self.src.match_ch('/') {
                break;
            }
        }
    }

    fn parse_single_char(&mut self) -> Option<Result<Token, LexError>> {
        Some(Ok(match self.src.next()? {
            '.' => {
                if self.src.match_ch('.') && self.src.match_ch('.') {
                    Token::Ellipsis
                } else {
                    Token::Dot
                }
            },
            '>' => {
                if self.src.match_ch('>') {
                    if self.src.match_ch('=') {
                        Token::RightAssign
                    } else {
                        Token::Right
                    }
                } else if self.src.match_ch('=') {
                    Token::Ge
                } else {
                    Token::Gt
                }
            },
            '<' => {
                if self.src.match_ch('<') {
                    if self.src.match_ch('=') {
                        Token::LeftAssign
                    } else {
                        Token::Left
                    }
                } else if self.src.match_ch('=') {
                    Token::Le
                } else {
                    Token::Lt
                }
            },
            '+' => {
                if self.src.match_ch('=') {
                    Token::AddAssign
                } else if self.src.match_ch('+') {
                    Token::Increment
                } else {
                    Token::Plus
                }
            },
            '-' => {
                if self.src.match_ch('=') {
                    Token::SubAssign
                } else if self.src.match_ch('-') {
                    Token::Decrement
                } else if self.src.match_ch('>') {
                    Token::ArrowRight
                } else {
                    Token::Minus
                }
            },
            '*' => {
                if self.src.match_ch('=') {
                    Token::MulAssign
                } else {
                    Token::Asterisk
                }
            },
            '/' => {
                if self.src.match_ch('=') {
                    Token::DivAssign
                } else {
                    Token::Slash
                }
            },
            '%' => {
                if self.src.match_ch('=') {
                    Token::ModAssign
                } else {
                    Token::Mod
                }
            },
            '&' => {
                if self.src.match_ch('=') {
                    Token::AndAssign
                } else if self.src.match_ch('&') {
                    Token::And
                } else {
                    Token::Ampersand
                }
            },
            '^' => {
                if self.src.match_ch('=') {
                    Token::XorAssign
                } else {
                    Token::Circ
                }
            },
            '|' => {
                if self.src.match_ch('=') {
                    Token::OrAssign
                } else if self.src.match_ch('|') {
                    Token::Or
                } else {
                    Token::Bar
                }
            },
            '=' => {
                if self.src.match_ch('=') {
                    Token::Eq
                } else {
                    Token::Assign
                }
            },
            '!' => {
                if self.src.match_ch('=') {
                    Token::Neq
                } else {
                    Token::Not
                }
            },
            ';' => Token::Semicolon,
            '{' => Token::LeftCurly,
            '}' => Token::RightCurly,
            ',' => Token::Comma,
            ':' => Token::Colon,
            '(' => Token::LeftParen,
            ')' => Token::RightParen,
            '[' => Token::LeftBrace,
            ']' => Token::RightBrace,
            '~' => Token::Tilde,
            '?' => Token::Quest,
            _ => return Some(Err(UnexpectedCharacter(self.src.peek().unwrap())))
        }))
    }

    fn parse_keyword_or_ident(&mut self) -> Result<Token, LexError> {
        let ident: String = self.src
            .take_while(|c| c.is_ascii_alphanumeric() || c == '_');

        Ok(match ident.as_str() {
            "auto" => Token::Auto,
            "break" => Token::Break,
            "char" => Token::Char,
            "case" => Token::Case,
            "const" => Token::Const,
            "continue" => Token::Continue,
            "default" => Token::Default,
            "do" => Token::Do,
            "double" => Token::Double,
            "else" => Token::Else,
            "enum" => Token::Enum,
            "extern" => Token::Extern,
            "float" => Token::Float,
            "for" => Token::For,
            "goto" => Token::Goto,
            "if" => Token::If,
            "int" => Token::Int,
            "long" => Token::Long,
            "register" => Token::Register,
            "return" => Token::Return,
            "short" => Token::Short,
            "signed" => Token::Signed,
            "sizeof" => Token::Sizeof,
            "static" => Token::Static,
            "struct" => Token::Struct,
            "switch" => Token::Switch,
            "typedef" => Token::Typedef,
            "union" => Token::Union,
            "unsigned" => Token::Unsigned,
            "void" => Token::Void,
            "volatile" => Token::Volatile,
            "while" => Token::While,
            _ => Token::Identifier(ident)
        })
    }
}

struct NumberLiteralCollector<'src, 'a> {
    src: &'a mut Cursor<'src>
}

impl<'src, 'a> NumberLiteralCollector<'src, 'a> {
    fn new(src: &'a mut Cursor<'src>) -> Self {
        Self { src }
    }

    pub fn collect(&mut self) -> Result<Token, LexError> {
        let prefix = match self.src.peek().unwrap() {
            '0' => {
                self.src.next();
                self.parse_prefix()
            },
            _ => None
        };

        match prefix {
            Some(NumberLiteralPrefix::Hex) => self.consume_hex_digits(),
            Some(NumberLiteralPrefix::Oct) => self.consume_oct_digits(),
            Some(NumberLiteralPrefix::Bin) => self.consume_bin_digits(),
            None => self.consume_dec_digits(),
        };

        let kind = match self.src.peek() {
            Some('.') if prefix.is_none() => {
                self.src.next();
                self.src.take_while(|c| c.is_ascii_digit());

                if matches!(self.src.peek(), Some('e' | 'E')) {
                    self.consume_exponent()?;
                    NumberLiteralKind::Exponent
                } else {
                    NumberLiteralKind::Float
                }
            },
            Some('e' | 'E') => {
                if prefix.is_some() {
                    return Err(InvalidDigit('e'))
                }

                self.consume_exponent()?;
                NumberLiteralKind::Exponent
            },
            _ => NumberLiteralKind::Int
        };

        let suffix = self.parse_suffix(
            match kind {
                NumberLiteralKind::Exponent | NumberLiteralKind::Float => {
                    Self::parse_float_suffix
                },
                NumberLiteralKind::Int => {
                    Self::parse_integer_suffix
                }
            }
        )?;

        Ok(Token::NumberLiteral {
            prefix,
            suffix,
            kind
        })
    }

    fn parse_suffix<F>(
        &mut self,
        strategy: F
    ) -> Result<Option<NumberLiteralSuffix>, LexError>
    where
        F: Fn(&mut Cursor) -> Option<NumberLiteralSuffix>
    {
        let suffix = self.src.take_while(|c| c.is_ascii_alphanumeric());
        let mut cursor = Cursor::new(&suffix);
        let result = strategy(&mut cursor);

        if cursor.peek().is_some() { // characters left in suffix
            Err(InvalidNumberLiteralSuffix(suffix))
        } else {
            Ok(result)
        }
    }

    fn parse_integer_suffix(cursor: &mut Cursor) -> Option<NumberLiteralSuffix> {
        match cursor.peek() {
            Some('u' | 'U') => {
                cursor.next();

                match cursor.peek() {
                    Some('l') => {
                        cursor.next();

                        match cursor.peek() {
                            Some('l') => { // Ull or ull
                                cursor.next();
                                Some(NumberLiteralSuffix::UnsignedLongLong)
                            },
                            _ => Some(NumberLiteralSuffix::UnsignedLong) // Ul or ul
                        }
                    },
                    Some('L') => {
                        cursor.next();

                        match cursor.peek() {
                            Some('L') => { // ULL or uLL
                                cursor.next();
                                Some(NumberLiteralSuffix::UnsignedLong)
                            },
                            _ => Some(NumberLiteralSuffix::UnsignedLong) // UL or uL
                        }
                    }
                    _ => Some(NumberLiteralSuffix::Unsigned) // U or u
                }
            },
            Some('l') => {
                cursor.next();

                match cursor.peek() {
                    Some('l') => {
                        cursor.next();

                        match cursor.peek() { // llU or llu
                            Some('U' | 'u') => {
                                cursor.next();
                                Some(NumberLiteralSuffix::UnsignedLongLong)
                            },
                            _ => Some(NumberLiteralSuffix::LongLong) // ll
                        }
                    },
                    Some('u' | 'U') => { // lu or lU
                        cursor.next();
                        Some(NumberLiteralSuffix::UnsignedLong)
                    },
                    _ => Some(NumberLiteralSuffix::Long) // l
                }
            },
            Some('L') => {
                cursor.next();

                match cursor.peek() {
                    Some('L') => {
                        cursor.next();

                        match cursor.peek() {
                            Some('u' | 'U') => { // LLu or LLU
                                cursor.next();
                                Some(NumberLiteralSuffix::UnsignedLongLong)
                            },
                            _ => Some(NumberLiteralSuffix::LongLong) // LL
                        }
                    },
                    Some('u' | 'U') => { // Lu or LU
                        cursor.next();
                        Some(NumberLiteralSuffix::UnsignedLong)
                    },
                    _ => Some(NumberLiteralSuffix::Long) // L
                }
            },
            _ => None
        }
    }

    fn parse_float_suffix(cursor: &mut Cursor) -> Option<NumberLiteralSuffix> {
        match cursor.peek() {
            Some('f' | 'F') => {
                cursor.next();

                Some(NumberLiteralSuffix::Float)
            },
            Some('l' | 'L') => {
                cursor.next();

                Some(NumberLiteralSuffix::Long)
            }
            _ => None
        }
    }

    fn parse_prefix(&mut self) -> Option<NumberLiteralPrefix> {
        let prefix = match self.src.peek()? {
            'x' => Some(NumberLiteralPrefix::Hex),
            'o' => Some(NumberLiteralPrefix::Oct),
            'b' => Some(NumberLiteralPrefix::Bin),
            _ => return None
        };

        self.src.next();

        prefix
    }

    fn consume_hex_digits(&mut self) {
        self.src.take_while(|c| c.is_ascii_hexdigit());
    }

    fn consume_dec_digits(&mut self) {
        self.src.take_while(|c| matches!(c, '0'..='9'));
    }

    fn consume_oct_digits(&mut self) {
        self.src.take_while(|c| matches!(c, '0'..='7'));
    }

    fn consume_bin_digits(&mut self) {
        self.src.take_while(|c| matches!(c, '0'..='1'));
    }

    fn consume_exponent(&mut self) -> Result<(), LexError> {
        self.src.next(); //

        if matches!(self.src.peek(), Some('+' | '-')) {
            self.src.next();
        }

        if !self.src.peek().is_some_and(|c| c.is_ascii_digit()) {
            return Err(ExponentHasNoDigits)
        }

        self.consume_dec_digits();
        Ok(())
    }
}

struct StringLiteralCollector<'src, 'a> {
    src: &'a mut Cursor<'src>
}

impl<'src, 'a> StringLiteralCollector<'src, 'a> {
    fn new(src: &'a mut Cursor<'src>) -> Self {
        Self { src }
    }

    pub fn collect(&mut self) -> Result<Token, LexError> {
        self.src.next(); // "

        loop {
            match self.src.peek() {
                Some('\\') => {
                    self.parse_escape_sequence()?;
                },
                Some('"') => {
                    self.src.next();
                    break;
                },
                None => return Err(UnterminatedString),
                _ => { self.src.next(); }
            }
        }

        Ok(Token::StringLiteral)
    }

    fn parse_escape_sequence(&mut self) -> Result<(), LexError> {
        match self.src.next() {
            Some('0'..='7') => self.parse_octal_escape(),
            Some('x') => self.parse_hex_escape(),
            Some('b' | 't' | 'n' | 'f' | 'r' | '\"' | '\'' | '\\' | '?') => {
                self.src.next();
                Ok(())
            },
            Some(c) => { Err(UnknownEscapeSequenceCharacter(c)) },
            _ => Err(UnexpectedEof)
        }
    }

    fn parse_hex_escape(&mut self) -> Result<(), LexError> {
        for _ in 0..=2 {
            if !self.src.peek().is_some_and(|c| c.is_ascii_hexdigit()) {
                break;
            }

            self.src.next();
        }

        Ok(())
    }

    fn parse_octal_escape(&mut self) -> Result<(), LexError> {
        for _ in 0..=3 {
            if !matches!(self.src.peek(), Some('0'..='7')) {
                break;
            }

            self.src.next();
        }

        Ok(())
    }
}
