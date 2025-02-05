//! This file implements Rust-like macros.
//! For information about grammar, see:
//! <https://doc.rust-lang.org/reference/macros-by-example.html#r-macro.decl.syntax>

use cmex_ast::token::TokenTag::*;
use cmex_ast::{DeclTag, DelimSpan, DelimTag, TokenTree};
use cmex_span::{MaybeSpannable, Span, Spanned, Unspan};

use crate::{require_tok, ParseErrorTag, Parser, PR};

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
                return Err(Spanned(
                    ParseErrorTag::ExpectedGot(
                        "delimited token tree".into(),
                        self.iter.peek().val(),
                    ),
                    self.iter.peek().span().unwrap(),
                ))
            }
            _ => panic!(),
        };

        let start_span = self.iter.next().span().unwrap();

        while self.iter.peek().val().as_ref() != Some(&end) {
            subtree.push(self.token_tree()?);
        }

        let end_span = self.iter.peek().span().unwrap();

        assert_eq!(self.iter.next().val(), Some(end));

        Ok(TokenTree::Delim(
            delim_tag,
            subtree,
            DelimSpan(start_span, end_span),
        ))
    }

    pub(crate) fn token_tree(&mut self) -> PR<TokenTree> {
        match self.iter.peek().val() {
            Some(LeftCurly | LeftParen | LeftBrace) => self.delim_token_tree(),
            Some(_) => Ok(TokenTree::Token(self.iter.next().unwrap())),
            None => {
                Err(Spanned(ParseErrorTag::UnexpectedEof, Span::placeholder()))
            }
        }
    }

    /// Parse original C-style macros e.g. `#include`, `#define`
    pub(crate) fn deprecated_macro(&mut self) -> PR<DeclTag> {
        assert_eq!(self.iter.next().val(), Some(Hash));

        match self.iter.peek() {
            Some((Identifier(directive), span)) => match directive.as_ref() {
                "include" => {
                    self.iter.next();

                    Ok(DeclTag::Include {
                        path: self.parse_path()?,
                        span,
                    })
                }
                _ => Err(Spanned(
                    ParseErrorTag::Expected("macro directive".into()),
                    span,
                )),
            },
            _ => Err(Spanned(
                ParseErrorTag::Expected("macro directive".into()),
                self.iter.next().span().unwrap(),
            )),
        }
    }

    fn parse_path(&mut self) -> PR<String> {
        match self.iter.peek().val() {
            Some(StringLiteral(path)) => Ok(path),
            Some(Lt) => {
                self.iter.next();
                let path = self
                    .iter
                    .by_ref()
                    .take_while(|tok| tok.0 != Gt)
                    .map(|tok| tok.0.to_string())
                    .collect::<Vec<String>>()
                    .join("");
                Ok(path)
            }
            Some(tok) => Err(Spanned(
                ParseErrorTag::ExpectedGot("path".into(), Some(tok)),
                self.iter.next().span().unwrap(),
            )),
            _ => panic!("unexpected eof"),
        }
    }
}
