mod decl;
mod expr;
mod macros;
mod nonterminal;
mod stmt;

use cmex_ast::{token::{Token, TokenTag}, Nonterminal, TranslationUnit};
use cmex_errors::ErrorEmitter;
use cmex_iter::Lookahead;
use cmex_lexer::{Tokens, TokensIter};
use cmex_span::{MaybeSpannable, Spanned, Unspan};
use cmex_symtable::SymTable;

pub(crate) type PR<T> = Result<T, ParseError>;

pub struct ParseOptions {
    /// Whether to parse comma operator.
    /// Especially useful when comma is expected to be
    /// a delimimiter rather then an operator, e.g. in macros.
    pub allow_comma_op: bool,
}

pub struct Parser<'a> {
    errors: &'a ErrorEmitter<'a>,
    pub iter: Lookahead<TokensIter<'a>>,
    symbols: SymTable<String, Spanned<SymbolTag>>,
    opts: ParseOptions,
}

impl<'a> Parser<'a> {
    pub fn new(
        iter: &'a Tokens,
        emitter: &'a ErrorEmitter,
        opts: ParseOptions,
    ) -> Self {
        Self {
            errors: emitter,
            iter: Lookahead::from(TokensIter::from(iter)),
            symbols: SymTable::new(),
            opts,
        }
    }

    pub fn parse(&mut self) -> Result<TranslationUnit, Vec<ParseError>> {
        self.translation_unit()
    }

    pub fn get_pos(&mut self) -> usize {
        self.iter.peek().map(|(_, span)| span.0).unwrap_or(0)
    }

    /// Bump the parser
    #[inline]
    pub fn next(&mut self) {
        self.iter.next();
    }

    pub(crate) fn curly_wrapped<T, F>(&mut self, f: F) -> PR<T>
    where
        T: Default,
        F: Fn(&mut Self) -> PR<T>,
    {
        self.delim_wrapped((TokenTag::LeftCurly, TokenTag::RightCurly), f)
    }

    pub(crate) fn paren_wrapped<T, F>(&mut self, f: F) -> PR<T>
    where
        T: Default,
        F: Fn(&mut Self) -> PR<T>,
    {
        self.delim_wrapped((TokenTag::LeftParen, TokenTag::RightParen), f)
    }

    fn delim_wrapped<T, F>(
        &mut self,
        delims: (TokenTag, TokenTag),
        f: F,
    ) -> PR<T>
    where
        T: Default,
        F: Fn(&mut Self) -> PR<T>,
    {
        self.match_tok(delims.0);
        let inner = f(self)
            .inspect_err(|err| {
                while self.iter.peek().val().as_ref() != Some(&delims.1) {
                    self.iter.next();
                }

                self.errors.emit(err)
            })
            .unwrap_or_default();
        self.require_tok(delims.1)?;
        Ok(inner)
    }

    #[inline(always)]
    pub(crate) fn match_tok(&mut self, tok: TokenTag) -> Option<Token> {
        if self.iter.peek().val() == Some(tok) {
            self.iter.next()
        } else {
            None
        }
    }

    #[inline]
    pub(crate) fn require_tok(&mut self, tok: TokenTag) -> PR<Token> {
        if self.iter.peek().val() == Some(tok.clone()) {
            Ok(self.iter.next().unwrap())
        } else {
            Err((
                ParseErrorTag::ExpectedGot(format!("`{}`", tok), self.iter.peek().clone().val()),
                self.iter.peek().span().unwrap()
            ))
        }
    }
}

#[derive(Clone)]
pub enum SymbolTag {
    Type,
    Name,
}

type ParseError = Spanned<ParseErrorTag>;

#[derive(Debug)]
pub enum ParseErrorTag {
    Expected(String),
    InterpolationFailed(Nonterminal),
    ExpectedGot(String, Option<TokenTag>),
    DeclarationHasNoIdentifier,
    DeclarationHasNoInitializer,
    UnexpectedDeclarationSuffix,
    UnexpectedToken(TokenTag),
    UnknownTypeName(String),
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
            Self::UnknownTypeName(ty) => {
                write!(f, "unknown type name `{ty}`")
            }
            Self::NameAlreadyDefined(name) => {
                write!(f, "name `{}` is already defined", name)
            }
            Self::ExpectedGot(exp, tok) => {
                write!(
                    f,
                    "expected {exp}, got {}",
                    tok.as_ref()
                        .map(|tok| format!("`{tok}`"))
                        .unwrap_or("end of file".into())
                )
            }
            Self::InterpolationFailed(nt) => {
                write!(
                    f,
                    "cannot interpolate nonterminal type `{}` into AST",
                    nt
                )
            }
            Self::UnexpectedEof => write!(f, "unexpected end of file"),
        }
    }
}

#[macro_export]
macro_rules! skip_until {
    ($parser:expr, $pat:pat) => {
        while !matches!($parser.iter.peek().val(), Some($pat)) {
            $parser.iter.next();
        }
    };
}

/// Check if kth token ahead matches `$pat`
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
        if matches!($parser.iter.peek().val(), Some($pat)) {
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
    ($parser:expr, $pat:pat) => {
        match $parser.iter.peek().val() {
            Some($pat) => Ok($parser.iter.next().unwrap()),
            _ => {
                tracing::debug!(
                    "raised error while expecting pattern '{}'",
                    stringify!($pat)
                );
                Err((
                    ParseErrorTag::ExpectedGot(
                        stringify!($pat).into(),
                        $parser.iter.peek().val().clone(),
                    ),
                    cmex_span::Span::from($parser.get_pos()),
                ))
            },
        }
    };
}
