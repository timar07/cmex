use crate::lexer::{
    Lexer,
    Spanned,
    TokenTag
};

mod expr;

pub struct Parser<'a> {
    iter: Tokens<'a>
}

impl<'a> Parser<'a> {
    pub fn new(iter: Lexer<'a>) -> Self {
        Self { iter: Tokens::new(iter.spanned()) }
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
