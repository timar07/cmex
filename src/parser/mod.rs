mod expr;
mod lookahead;
mod tests;

use crate::lexer::{
    Lexer,
    Spanned,
    TokenTag
};
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
    ($p:expr, $pat:pat) => {
        match $p.iter.peek() {
            Some($pat) => $p.iter.next().unwrap(),
            _ => panic!()
        }
    };
}
