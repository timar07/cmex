//! Nonterminals parsing facade for parser.
//! This functionality is mostly used during metavariables parsing in macros.

use cmex_ast::{token::TokenTag::*, Nonterminal, NtTag, Stmt};
use cmex_span::{MaybeSpannable, Span, Spanned, Unspan};
use tracing::instrument;

use crate::{require_tok, ParseErrorTag, Parser, PR};

impl Parser<'_> {
    /// Parse nonterminal. There is only on usage case so far so the function
    /// is inlined.
    #[inline]
    pub fn parse_nt(&mut self, tag: NtTag) -> PR<Nonterminal> {
        match tag {
            NtTag::Block => Ok(Nonterminal::Block(self.block()?.0)),
            NtTag::Literal => match self.iter.peek().val() {
                Some(NumberLiteral { .. } | CharLiteral | StringLiteral(_)) => {
                    Ok(Nonterminal::Literal(self.iter.next().unwrap()))
                }
                _ => Err(Spanned(
                    ParseErrorTag::Expected("literal".into()),
                    self.iter.peek().span().unwrap(),
                )),
            },
            NtTag::Ident => match self.iter.peek().val() {
                Some(Identifier(_)) => {
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

    #[instrument(skip_all)]
    pub fn block(&mut self) -> PR<(Vec<Stmt>, Span)> {
        let Spanned(_, open) = require_tok!(self, LeftCurly)?;

        let mut stmts = Vec::new();

        while !matches!(self.iter.peek().val(), Some(RightCurly)) {
            stmts.push(self.statement()?);
        }

        let Spanned(_, close) = self.iter.next().unwrap();

        Ok((stmts, Span::join(open, close)))
    }
}
