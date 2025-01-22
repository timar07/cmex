//! This file implements Rust-like macros.
//! For information about grammar, see:
//! <https://doc.rust-lang.org/reference/macros-by-example.html#r-macro.decl.syntax>

use crate::{require_tok, ParseErrorTag, Parser, PR};
use cmex_ast::token::TokenTag::*;
use cmex_ast::{DeclTag, DelimSpan, DelimTag, TokenTree};
use cmex_span::{Span, Unspan};

impl Parser<'_> {
    /// Parse `macro_rules!`. The pre-expansion parsing is implemented in
    /// `cmex_macros` module, for now, the definition is represented as an
    /// iterator over tokens within the body.
    pub fn macro_rules_definition(&mut self) -> PR<DeclTag> {
        self.iter.next();
        require_tok!(self, Not)?;
        let id = require_tok!(self, Identifier(_))?;

        Ok(DeclTag::Macro {
            id,
            body: self.delim_token_tree()?,
        })
    }

    pub fn delim_token_tree(&mut self) -> PR<TokenTree> {
        let mut subtree = Vec::new();

        let (end, delim_tag) = match self.iter.peek().val() {
            Some(LeftCurly) => (RightCurly, DelimTag::Curly),
            Some(LeftParen) => (RightParen, DelimTag::Paren),
            Some(LeftBrace) => (RightBrace, DelimTag::Square),
            Some(_) => {
                return Err((
                    ParseErrorTag::ExpectedGot(
                        "delimited token tree".into(),
                        self.iter.peek().val(),
                    ),
                    self.iter.peek().unwrap().1,
                ))
            }
            _ => panic!(),
        };

        let start_span = self.iter.next().unwrap().1;

        while self.iter.peek().val() != Some(end.clone()) {
            subtree.push(self.token_tree()?);
        }

        let end_span = self.iter.peek().unwrap().1;

        assert_eq!(self.iter.next().val(), Some(end));

        Ok(TokenTree::Delim(
            delim_tag,
            subtree,
            DelimSpan(start_span, end_span),
        ))
    }

    fn token_tree(&mut self) -> PR<TokenTree> {
        match self.iter.peek().val() {
            Some(LeftCurly | LeftParen | LeftBrace) => self.delim_token_tree(),
            Some(_) => Ok(TokenTree::Token(self.iter.next().unwrap())),
            None => todo!(),
        }
    }
}
