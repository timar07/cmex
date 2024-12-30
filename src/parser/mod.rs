mod symtable;
mod stmt;
mod expr;
mod lookahead;
mod tests;

use crate::ast::Stmt;
use crate::lexer::{Lexer, Span, Spanned, Token, TokenTag};
use lookahead::Lookahead;

pub struct Parser<'a> {
    iter: Lookahead<Tokens<'a>>
}

impl<'a> Parser<'a> {
    pub fn new(iter: Lexer<'a>) -> Self {
        Self {
            iter: Lookahead::from(Tokens::new(iter.spanned()))
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
struct Tokens<'a> {
    iter: Spanned<Lexer<'a>>
}

impl<'a> Tokens<'a> {
    pub fn new(iter: Spanned<Lexer<'a>>) -> Self {
        Self { iter }
    }
}

impl Iterator for Tokens<'_> {
    type Item = (TokenTag, Span);

    fn next(&mut self) -> Option<Self::Item> {
        self.iter
            .next()
            .map(|(i, span)| match i {
                Ok(i) => (i, span),
                Err(e) => {
                    println!("{e}");
                    (TokenTag::Error, span)
                }
            })
    }
}

#[derive(Debug)]
pub enum ParseError {
    Expected(String),
    DeclarationHasNoIdentifier,
    DeclarationHasNoInitializer,
    UnexpectedDeclarationSuffix,
    UnexpectedToken(Token),
    UnexpectedEof
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
            Some(($pat, _)) => $p.iter.next().unwrap(),
            _ => panic!(
                "expected {} at {}, got {:?}",
                stringify!($pat),
                $p.get_pos(),
                $p.iter.peek().val()
            )
        }
    };
}
