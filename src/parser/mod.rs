use std::iter::Peekable;
use crate::lexer::{
    Lexer,
    Spanned,
    TokenTag
};

mod expr;

pub struct Parser<'a> {
    iter: Peekable<Tokens<'a>>
}

impl<'a> Parser<'a> {
    pub fn new(iter: Lexer<'a>) -> Self {
        Self { iter: Tokens::new(iter.spanned()).peekable() }
    }
}

/// Wrapper above the [Lexer] for convenient error handling
struct Tokens<'a> {
    iter: Spanned<Lexer<'a>>
}

impl<'a> Tokens<'a> {
    pub fn new(iter: Spanned<Lexer<'a>>) -> Self {
        Self { iter }
    }
}

impl<'a> Iterator for Tokens<'_> {
    type Item = TokenTag;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter
            .next()
            .map(|(i, _)| match i {
                Ok(i) => i,
                Err(e) => {
                    println!("{e}");
                    TokenTag::Error
                }
            })
    }
}

#[macro_export]
macro_rules! match_tok {
    ($parser:expr, $pat:pat) => {
        if matches!($parser.iter.peek(), Some($pat)) {
            $parser.iter.next()
        } else {
            None
        }
    };
}

#[macro_export]
macro_rules! check_tok {
    ($parser:expr, $pat:pat) => {
        match_tok!($parser, $pat).is_some()
    };
}

#[macro_export]
macro_rules! require_tok {
    ($p:expr, $tok:expr) => {
        if $p.iter.peek().is_some_and(|t| *t != $tok) {
            panic!("Expected {:?}", $tok);
        } else {
            $p.iter.next();
        }
    };
}
