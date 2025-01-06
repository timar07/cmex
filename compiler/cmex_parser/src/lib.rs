mod expr;
mod lookahead;
mod macros;
mod stmt;
mod tests;

use cmex_ast::Stmt;
use cmex_lexer::{Lexer, Spanned, Token, TokenTag};
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
        if let Some((_, span)) = self.iter.peek() {
            span.1
        } else {
            0
        }
    }
}

impl Iterator for Parser<'_> {
    type Item = Stmt;

    fn next(&mut self) -> Option<Self::Item> {
        if self.iter.peek().is_some() {
            Some(self.statement().unwrap())
        } else {
            None
        }
    }
}

/// Wrapper above the [Lexer] for convenient error handling
#[derive(Clone)]
pub struct Tokens<'a> {
    iter: Spanned<Lexer<'a>>,
}

impl<'a> Tokens<'a> {
    pub fn new(iter: Spanned<Lexer<'a>>) -> Self {
        Self { iter }
    }
}

impl Iterator for Tokens<'_> {
    type Item = (TokenTag, Span);

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|(i, span)| match i {
            Ok(i) => (i, span),
            Err(e) => {
                println!("{e}");
                (TokenTag::Error, span)
            }
        })
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
    UnexpectedToken(Token),
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
            Self::UnexpectedToken((tok, _)) => {
                write!(f, "unexpected token `{:?}`", tok)
            }
            Self::NameAlreadyDefined(name) => {
                write!(f, "name `{}` is already defined", name)
            }
            Self::ExpectedGot(exp, tok) => {
                write!(
                    f,
                    "expected {exp} got {}",
                    tok.clone()
                        .map(|tok| format!("`{tok}`"))
                        .unwrap_or("end of file".into())
                )
            }
            Self::UnexpectedEof => write!(f, "unexpected end of file"),
        }
    }
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
                ParseErrorTag::ExpectedGot(stringify!($pat).into(), $p.iter.peek().val()),
                Span::from($p.get_pos()),
            )),
        }
    };
}
