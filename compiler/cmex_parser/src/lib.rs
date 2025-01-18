mod expr;
mod lookahead;
mod macros;
mod stmt;

use cmex_ast::{Nonterminal, NtTag};
use cmex_lexer::{Lexer, Token, TokenTag, Tokens, TokensIter};
use cmex_span::{Span, Unspan};
use cmex_symtable::SymTable;
pub use lookahead::Lookahead;

pub(crate) type PR<T> = Result<T, ParseError>;

pub struct Parser<'a> {
    pub iter: Lookahead<TokensIter<'a>>,
    symbols: SymTable<String, Span>,
}

impl<'a> Parser<'a> {
    pub fn new(iter: &'a Tokens) -> Self {
        Self {
            iter: Lookahead::from(TokensIter::from(iter)),
            symbols: SymTable::new(),
        }
    }

    pub fn get_pos(&mut self) -> usize {
        self.iter.peek().map(|(_, span)| span.0).unwrap_or(0)
    }

    pub fn next(&mut self) {
        self.iter.next();
    }

    /// Parse nonterminal. There is only on usage case so far so the function
    /// is inlined.
    #[inline]
    pub fn parse_nt(&mut self, tag: &NtTag) -> PR<Nonterminal> {
        match tag {
            NtTag::Block => {
                Ok(Nonterminal::Block(self.block()?))
            },
            NtTag::Literal => {
                match self.iter.peek().val() {
                    Some(
                        TokenTag::NumberLiteral { .. }
                        | TokenTag::CharLiteral
                        | TokenTag::StringLiteral
                    ) => {
                        Ok(Nonterminal::Literal(self.iter.next().unwrap()))
                    },
                    _ => Err((
                        ParseErrorTag::Expected("literal".into()),
                        self.iter.peek().unwrap().1
                    )),
                }
            },
            NtTag::Ident => {
                match self.iter.peek().val() {
                    Some(TokenTag::Identifier(_)) => {
                        Ok(Nonterminal::Ident(self.iter.next().unwrap()))
                    },
                    _ => Err((
                        ParseErrorTag::Expected("identifier".into()),
                        self.iter.peek().unwrap().1
                    ))
                }
            },
            NtTag::Item => {
                Ok(Nonterminal::Item(self.external_decl()?))
            },
            NtTag::Ty => {
                Ok(Nonterminal::Ty(self.type_name()?))
            },
            NtTag::Expr => {
                Ok(Nonterminal::Expr(self.expression()?))
            },
        }
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
