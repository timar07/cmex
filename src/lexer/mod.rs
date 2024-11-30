mod errors;
mod token;
mod cursor;
mod tests;

pub use errors::LexError;
pub use token::Spanned;
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
    type Item = Result<TokenTag, LexError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.lex_token()
    }
}

impl<'a> Positioned for Lexer<'a> {
    fn get_pos(&self) -> usize { self.src.pos }
}

/// Iterator over tokens' lexemes
/// todo: write tests
pub struct Lexemes<'a> {
    iter: Spanned<Lexer<'a>>
}

impl<'a> Lexemes<'a> {
    pub fn new(iter: Spanned<Lexer<'a>>) -> Self {
        Self { iter }
    }
}

impl<'a> Iterator for Lexemes<'a> {
    type Item = String;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter
            .next()
            .map(|(_, span)| {
                self.iter.iter.src.slice(span.0, span.1)
            })
    }
}

impl<'a> Spanned<Lexer<'a>> {
    pub fn lexemes(self) -> Lexemes<'a> {
        Lexemes::new(self)
    }
}

macro_rules! consume {
    ($lexer:expr, $pattern:pat) => {
        if matches!($lexer.src.peek(), $pattern) {
            $lexer.src.next();
        }
    };
}

impl Lexer<'_> {
    pub fn spanned(self) -> Spanned<Self> {
        Spanned::new(self)
    }

    fn lex_token(&mut self) -> Option<Result<TokenTag, LexError>> {
        match self.src.peek()? {
            c if c.is_ascii_whitespace() => {
                self.src.next();
                self.lex_token()
            },
            c if c.is_ascii_alphabetic() => {
                Some(self.parse_keyword_or_ident())
            },
            '\"' => Some(StringLiteralCollector::new(&mut self.src).collect()),
            '\'' => Some(CharLiteralCollector::new(&mut self.src).collect()),
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

    fn parse_single_char(&mut self) -> Option<Result<TokenTag, LexError>> {
        Some(Ok(match self.src.next()? {
            '.' => {
                if self.src.match_ch('.') && self.src.match_ch('.') {
                    TokenTag::Ellipsis
                } else {
                    TokenTag::Dot
                }
            },
            '>' => {
                if self.src.match_ch('>') {
                    if self.src.match_ch('=') {
                        TokenTag::RightAssign
                    } else {
                        TokenTag::Right
                    }
                } else if self.src.match_ch('=') {
                    TokenTag::Ge
                } else {
                    TokenTag::Gt
                }
            },
            '<' => {
                if self.src.match_ch('<') {
                    if self.src.match_ch('=') {
                        TokenTag::LeftAssign
                    } else {
                        TokenTag::Left
                    }
                } else if self.src.match_ch('=') {
                    TokenTag::Le
                } else {
                    TokenTag::Lt
                }
            },
            '+' => {
                if self.src.match_ch('=') {
                    TokenTag::AddAssign
                } else if self.src.match_ch('+') {
                    TokenTag::Increment
                } else {
                    TokenTag::Plus
                }
            },
            '-' => {
                if self.src.match_ch('=') {
                    TokenTag::SubAssign
                } else if self.src.match_ch('-') {
                    TokenTag::Decrement
                } else if self.src.match_ch('>') {
                    TokenTag::ArrowRight
                } else {
                    TokenTag::Minus
                }
            },
            '*' => {
                if self.src.match_ch('=') {
                    TokenTag::MulAssign
                } else {
                    TokenTag::Asterisk
                }
            },
            '/' => {
                if self.src.match_ch('=') {
                    TokenTag::DivAssign
                } else {
                    TokenTag::Slash
                }
            },
            '%' => {
                if self.src.match_ch('=') {
                    TokenTag::ModAssign
                } else {
                    TokenTag::Mod
                }
            },
            '&' => {
                if self.src.match_ch('=') {
                    TokenTag::AndAssign
                } else if self.src.match_ch('&') {
                    TokenTag::And
                } else {
                    TokenTag::Ampersand
                }
            },
            '^' => {
                if self.src.match_ch('=') {
                    TokenTag::XorAssign
                } else {
                    TokenTag::Circ
                }
            },
            '|' => {
                if self.src.match_ch('=') {
                    TokenTag::OrAssign
                } else if self.src.match_ch('|') {
                    TokenTag::Or
                } else {
                    TokenTag::Bar
                }
            },
            '=' => {
                if self.src.match_ch('=') {
                    TokenTag::Eq
                } else {
                    TokenTag::Assign
                }
            },
            '!' => {
                if self.src.match_ch('=') {
                    TokenTag::Neq
                } else {
                    TokenTag::Not
                }
            },
            ';' => TokenTag::Semicolon,
            '{' => TokenTag::LeftCurly,
            '}' => TokenTag::RightCurly,
            ',' => TokenTag::Comma,
            ':' => TokenTag::Colon,
            '(' => TokenTag::LeftParen,
            ')' => TokenTag::RightParen,
            '[' => TokenTag::LeftBrace,
            ']' => TokenTag::RightBrace,
            '~' => TokenTag::Tilde,
            '?' => TokenTag::Quest,
            _ => return Some(Err(UnexpectedCharacter(self.src.peek().unwrap())))
        }))
    }

    fn parse_keyword_or_ident(&mut self) -> Result<TokenTag, LexError> {
        let ident = self.src
            .take_while(|c| c.is_ascii_alphanumeric() || c == '_');

        Ok(match ident.as_str() {
            "auto" => TokenTag::Auto,
            "break" => TokenTag::Break,
            "char" => TokenTag::Char,
            "case" => TokenTag::Case,
            "const" => TokenTag::Const,
            "continue" => TokenTag::Continue,
            "default" => TokenTag::Default,
            "do" => TokenTag::Do,
            "double" => TokenTag::Double,
            "else" => TokenTag::Else,
            "enum" => TokenTag::Enum,
            "extern" => TokenTag::Extern,
            "float" => TokenTag::Float,
            "for" => TokenTag::For,
            "goto" => TokenTag::Goto,
            "if" => TokenTag::If,
            "int" => TokenTag::Int,
            "long" => TokenTag::Long,
            "register" => TokenTag::Register,
            "return" => TokenTag::Return,
            "short" => TokenTag::Short,
            "signed" => TokenTag::Signed,
            "sizeof" => TokenTag::Sizeof,
            "static" => TokenTag::Static,
            "struct" => TokenTag::Struct,
            "switch" => TokenTag::Switch,
            "typedef" => TokenTag::Typedef,
            "union" => TokenTag::Union,
            "unsigned" => TokenTag::Unsigned,
            "void" => TokenTag::Void,
            "volatile" => TokenTag::Volatile,
            "while" => TokenTag::While,
            _ => TokenTag::Identifier
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

    pub fn collect(&mut self) -> Result<TokenTag, LexError> {
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

        Ok(TokenTag::NumberLiteral {
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

        consume!(self, Some('+' | '-'));

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
    pub fn new(src: &'a mut Cursor<'src>) -> Self {
        Self { src }
    }

    pub fn collect(&mut self) -> Result<TokenTag, LexError> {
        self.src.next(); // "

        loop {
            match self.src.peek() {
                Some('\\') => {
                    EscapeSequenceCollector::new(self.src).collect()?;
                },
                Some('"') => {
                    self.src.next();
                    break;
                },
                None => return Err(UnterminatedString),
                _ => { self.src.next(); }
            }
        }

        Ok(TokenTag::StringLiteral)
    }
}

struct CharLiteralCollector<'src, 'a> {
    src: &'a mut Cursor<'src>
}

impl<'src, 'a> CharLiteralCollector<'src, 'a> {
    fn new(src: &'a mut Cursor<'src>) -> Self {
        Self { src }
    }

    pub fn collect(&mut self) -> Result<TokenTag, LexError> {
        assert!(self.src.next() == Some('\''));

        match self.src.peek() {
            Some('\\') => {
                EscapeSequenceCollector::new(self.src).collect()?;
            },
            Some('\'') => {
                self.src.next();
                return Err(EmptyCharacterConstant);
            },
            _ => { self.src.next(); }
        };

        if !self.src.match_ch('\'') {
            return Err(UnterminatedCharacterLiteral);
        }

        Ok(TokenTag::CharLiteral)
    }
}

struct EscapeSequenceCollector<'src, 'a> {
    src: &'a mut Cursor<'src>
}

impl<'src, 'a> EscapeSequenceCollector<'src, 'a> {
    fn new(src: &'a mut Cursor<'src>) -> Self {
        Self { src }
    }

    pub fn collect(&mut self) -> Result<(), LexError> {
        assert!(self.src.next() == Some('\\'));

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
