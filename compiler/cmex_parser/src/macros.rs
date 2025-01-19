//! This file implements Rust-like macros.
//! For information about grammar, see:
//! <https://doc.rust-lang.org/reference/macros-by-example.html#r-macro.decl.syntax>

use crate::{require_tok, ParseErrorTag, Parser, PR};
use cmex_ast::{Decl, DelimTag, TokenTree};
use cmex_lexer::TokenTag::*;
use cmex_span::{Span, Unspan};

impl<'a> Parser<'a> {
    /// Parse `macro_rules!`. The pre-expansion parsing is implemented in
    /// `cmex_macros` module, for now, the definition is represented as an
    /// iterator over tokens within the body.
    pub fn macro_rules_definition(&mut self) -> PR<Decl> {
        self.iter.next();
        require_tok!(self, Not)?;
        let id = require_tok!(self, Identifier(_))?;

        Ok(Decl::Macro {
            id,
            body: self.delim_token_tree()?,
        })
    }

    pub fn delim_token_tree(&mut self) -> PR<TokenTree> {
        let mut subtree = Vec::new();

        let (end, delim_tag) = match self.iter.peek().val() {
            Some(LeftCurly) => {
                self.iter.next();
                (RightCurly, DelimTag::Curly)
            }
            Some(LeftParen) => {
                self.iter.next();
                (RightParen, DelimTag::Paren)
            }
            Some(LeftBrace) => {
                self.iter.next();
                (RightBrace, DelimTag::Square)
            }
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

        while self.iter.peek().val() != Some(end.clone()) {
            subtree.push(self.token_tree()?);
        }

        assert_eq!(self.iter.next().val(), Some(end));

        Ok(TokenTree::Delim(delim_tag, subtree))
    }

    fn token_tree(&mut self) -> PR<TokenTree> {
        match self.iter.peek().val() {
            Some(LeftCurly | LeftParen | LeftBrace) => self.delim_token_tree(),
            Some(_) => Ok(TokenTree::Token(self.iter.next().unwrap())),
            None => todo!(),
        }
    }
}
