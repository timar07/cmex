pub use token::Token;
use cursor::Cursor;

mod token;
mod cursor;
mod tests;

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
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.lex_token()
    }
}

impl Lexer<'_> {
    fn lex_token(&mut self) -> Option<Token> {
        match self.src.peek()? {
            c if c.is_ascii_whitespace() => {
                self.src.next();
                self.lex_token()
            },
            c if c.is_ascii_alphabetic() => {
                Some(self.parse_keyword_or_ident())
            },
            '\"' => self.parse_string(),
            _ => self.parse_single_char()
        }
    }

    fn parse_string(&mut self) -> Option<Token> {
        Some(Token::StringLiteral(
            self.src
                .take_while(|c| c != '"')
        ))
    }

    fn parse_single_char(&mut self) -> Option<Token> {
        Some(match self.src.next()? {
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
            _ => return None
        })
    }

    fn parse_keyword_or_ident(&mut self) -> Token {
        let ident: String = self.src
            .take_while(|c| c.is_ascii_alphanumeric() || c == '_');

        match ident.as_str() {
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
        }
    }
}
