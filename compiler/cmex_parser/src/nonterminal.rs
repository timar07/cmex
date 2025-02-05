//! Nonterminals parsing facade for parser.
//! This functionality is mostly used during metavariables parsing in macros.

use cmex_ast::{token::TokenTag, Nonterminal, NtTag};
use cmex_span::{MaybeSpannable, Spanned, Unspan};

use crate::{ParseErrorTag, Parser, PR};

impl Parser<'_> {
    /// Parse nonterminal. There is only on usage case so far so the function
    /// is inlined.
    #[inline]
    pub fn parse_nt(&mut self, tag: NtTag) -> PR<Nonterminal> {
        match tag {
            NtTag::Block => Ok(Nonterminal::Block(self.block()?.0)),
            NtTag::Literal => match self.iter.peek().val() {
                Some(
                    TokenTag::NumberLiteral { .. }
                    | TokenTag::CharLiteral
                    | TokenTag::StringLiteral(_),
                ) => Ok(Nonterminal::Literal(self.iter.next().unwrap())),
                _ => Err(Spanned(
                    ParseErrorTag::Expected("literal".into()),
                    self.iter.peek().span().unwrap(),
                )),
            },
            NtTag::Ident => match self.iter.peek().val() {
                Some(TokenTag::Identifier(_)) => {
                    Ok(Nonterminal::Ident(self.iter.next().unwrap()))
                }
                _ => Err(Spanned(
                    ParseErrorTag::Expected("identifier".into()),
                    self.iter.peek().span().unwrap(),
                )),
            },
            NtTag::Tt => {
                let mut tt = vec![];
                while self.iter.peek().is_some() {
                    tt.push(self.token_tree()?);
                }
                Ok(Nonterminal::Tt(tt))
            }
            NtTag::Item => Ok(Nonterminal::Item(self.external_decl()?)),
            NtTag::Ty => Ok(Nonterminal::Ty(self.type_name()?)),
            NtTag::Expr => Ok(Nonterminal::Expr(self.expression()?)),
        }
    }
}
