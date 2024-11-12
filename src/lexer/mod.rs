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
        Some(match self.src.peek()? {
            '+' => {
                if self.src.match_ch('=') {
                    Token::AddAssign
                } else {
                    Token::Plus
                }
            }
            _ => return None
        }).and_then(|t| {
            self.src.next();
            Some(t)
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
