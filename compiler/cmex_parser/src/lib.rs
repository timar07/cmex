mod expr;
mod lookahead;
mod macros;
mod stmt;

use cmex_lexer::{Lexer, TokenTag, Tokens};
use cmex_span::Span;
use cmex_symtable::SymTable;
pub use lookahead::Lookahead;

pub(crate) type PR<T> = Result<T, ParseError>;

pub struct Parser<'a> {
    iter: Lookahead<Tokens<'a>>,
    symbols: SymTable<String, Span>,
}

impl<'a> Parser<'a> {
    pub fn new(iter: Lexer<'a>) -> Self {
        Self {
            iter: Lookahead::from(Tokens::new(iter.spanned())),
            symbols: SymTable::new(),
        }
    }

    pub fn get_pos(&mut self) -> usize {
        self.iter
            .peek()
            .map(|(_, span)| span.0)
            .unwrap_or(0)
    }
}

type ParseError = (ParseErrorTag, Span);

#[derive(Debug)]
pub enum ParseErrorTag {
    Expected(String),
    ExpectedGot(String, Option<TokenTag>),
    DeclarationHasNoIdentifier,
    DeclarationHasNoInitializer,
    UnexpectedDeclarationSuffix,
    UnexpectedToken(TokenTag),
    NameAlreadyDefined(String),
    UnexpectedEof,
}

impl std::fmt::Display for ParseErrorTag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Expected(s) => write!(f, "expected {s}"),
            Self::DeclarationHasNoIdentifier => {
                write!(f, "declaration has no identifier")
            }
            Self::DeclarationHasNoInitializer => {
                write!(f, "declaration has no initializer")
            }
            Self::UnexpectedDeclarationSuffix => {
                write!(f, "unexpected declaration suffix")
            }
            Self::UnexpectedToken(tok) => {
                write!(f, "unexpected token `{:?}`", tok)
            }
            Self::NameAlreadyDefined(name) => {
                write!(f, "name `{}` is already defined", name)
            }
            Self::ExpectedGot(exp, tok) => {
                write!(
                    f,
                    "expected {exp}, got {}",
                    tok.clone()
                        .map(|tok| format!("`{tok}`"))
                        .unwrap_or("end of file".into())
                )
            }
            Self::UnexpectedEof => write!(f, "unexpected end of file"),
        }
    }
}

#[macro_export]
macro_rules! lookahead {
    ($parser:expr, $k:expr, $pat:pat) => {
        matches!($parser.iter.lookahead($k).val(), Some($pat))
    };
}

/// Check if the next token matches $pat, returns it if it does
#[macro_export]
macro_rules! match_tok {
    ($parser:expr, $pat:pat) => {
        if matches!($parser.iter.peek(), Some(($pat, _))) {
            $parser.iter.next()
        } else {
            None
        }
    };
}

/// Check if the next token matches $pat, returns true if it does
#[macro_export]
macro_rules! check_tok {
    ($parser:expr, $pat:pat) => {
        match_tok!($parser, $pat).is_some()
    };
}

/// Check if the next token matches $pat, raises a panic if it doesn't
#[macro_export]
macro_rules! require_tok {
    ($p:expr, $pat:pat) => {
        match $p.iter.peek() {
            Some(($pat, _)) => Ok($p.iter.next().unwrap()),
            _ => Err((
                ParseErrorTag::ExpectedGot(
                    stringify!($pat).into(),
                    $p.iter.peek().val(),
                ),
                Span::from($p.get_pos()),
            )),
        }
    };
}
