//! This file implements Rust-like macros.
//! For information about grammar, see:
//! <https://doc.rust-lang.org/reference/macros-by-example.html#r-macro.decl.syntax>

use crate::{check_tok, match_tok, require_tok, ParseErrorTag, Parser, PR};
use cmex_ast::Decl;
use cmex_lexer::TokenTag::*;
use cmex_macros::{
    MacroFragSpec, MacroMatch, MacroMatcher, MacroRules, RepOpTag, TokenTree,
};
use cmex_span::{Span, Unspan};
use std::ops::Deref;

impl Parser<'_> {
    pub fn macro_rules_definition(&mut self) -> PR<Decl> {
        self.iter.next();
        require_tok!(self, Not)?;
        let id = require_tok!(self, Identifier(_))?;

        Ok(Decl::Macro {
            id,
            rules: dbg!(self.macro_rules_def()?),
        })
    }

    fn macro_rules_def(&mut self) -> PR<Vec<MacroRules>> {
        match self.iter.peek().val() {
            Some(LeftCurly) => self.macro_rules(),
            Some(_) => Err((
                ParseErrorTag::Expected("macro rules definition".into()),
                self.iter.peek().unwrap().1,
            )),
            _ => panic!(),
        }
    }

    fn macro_rules(&mut self) -> PR<Vec<MacroRules>> {
        assert_eq!(self.iter.next().val(), Some(LeftCurly));

        let mut macro_rules = Vec::new();

        while !check_tok!(self, RightCurly) {
            macro_rules.push(self.macro_rule()?);

            if !matches!(self.iter.peek().val(), Some(RightCurly)) {
                require_tok!(self, Semicolon)?; // Delimiter semicolon requied
            }
        }

        Ok(macro_rules)
    }

    fn macro_rule(&mut self) -> PR<MacroRules> {
        let lhs = self.macro_matcher()?;
        require_tok!(self, FatArrow)?;
        let rhs = self.macro_transcriber()?;
        Ok(MacroRules(lhs, rhs))
    }

    fn macro_matcher(&mut self) -> PR<MacroMatcher> {
        let mut macro_match = None;

        match self.iter.peek().val() {
            Some(LeftParen) => {
                self.iter.next();

                if !check_tok!(self, RightParen) {
                    macro_match = Some(self.macro_match()?);
                    require_tok!(self, RightParen)?;
                }
            }
            Some(t) => {
                return Err((
                    ParseErrorTag::UnexpectedToken(t),
                    self.iter.peek().unwrap().1,
                ))
            }
            _ => todo!(),
        }

        Ok(MacroMatcher(macro_match))
    }

    fn macro_match(&mut self) -> PR<MacroMatch> {
        match self.iter.peek().val() {
            Some(Dollar) => {
                self.iter.next();

                match self.iter.peek().val() {
                    Some(t) if t.is_keyword_or_id() => {
                        self.iter.next();
                        require_tok!(self, Colon)?;
                        Ok(MacroMatch::Frag(
                            t.to_string(),
                            self.macro_frag_spec()?,
                        ))
                    }
                    _ => {
                        let mut macro_match = None;

                        require_tok!(self, LeftParen)?;
                        if !check_tok!(self, RightParen) {
                            macro_match = Some(Box::new(self.macro_match()?));
                            require_tok!(self, RightParen)?;
                        }

                        if let Some(rep_op) = self.maybe_rep_op() {
                            return Ok(MacroMatch::Rep(
                                macro_match,
                                None,
                                Some(rep_op),
                            ));
                        }

                        self.iter.peek().ok_or_else(|| {
                            (
                                ParseErrorTag::Expected("separator".into()),
                                self.iter.peek().unwrap().1,
                            )
                        })?;

                        Ok(MacroMatch::Rep(macro_match, None, None))
                    }
                }
            }
            Some(t) if !t.is_delimiter() => {
                Ok(MacroMatch::Token(self.iter.next().unwrap()))
            }
            _ => panic!(),
        }
    }

    fn maybe_rep_op(&mut self) -> Option<RepOpTag> {
        match match_tok!(self, Asterisk | Plus | Quest).val() {
            Some(Asterisk) => Some(RepOpTag::Asterisk),
            Some(Plus) => Some(RepOpTag::Plus),
            Some(Quest) => Some(RepOpTag::Quest),
            _ => None,
        }
    }

    /// Since there is no visibilies, lifetimes, attributes
    /// in C, so they were removed due to language semantics.
    fn macro_frag_spec(&mut self) -> PR<MacroFragSpec> {
        if let Some(Identifier(spec)) = self.iter.next().val() {
            match spec.deref() {
                "block" => Ok(MacroFragSpec::Block),
                "literal" => Ok(MacroFragSpec::Literal),
                "ident" => Ok(MacroFragSpec::Ident),
                "item" => Ok(MacroFragSpec::Item),
                "ty" => Ok(MacroFragSpec::Ty),
                "expr" => Ok(MacroFragSpec::Expr),
                "pat" => todo!(),
                _ => Err((
                    ParseErrorTag::Expected("fragment specifier".into()),
                    self.iter.peek().unwrap().1,
                )),
            }
        } else {
            Err((
                ParseErrorTag::Expected("fragment specifier".into()),
                self.iter.peek().unwrap().1,
            ))
        }
    }

    fn macro_transcriber(&mut self) -> PR<TokenTree> {
        self.delim_token_tree()
    }

    pub(crate) fn delim_token_tree(&mut self) -> PR<TokenTree> {
        let mut subtree = Vec::new();

        match self.iter.peek().val() {
            Some(LeftCurly | LeftParen | LeftBrace) => {
                self.iter.next();

                while !check_tok!(self, RightCurly | RightParen | RightBrace) {
                    subtree.push(self.token_tree()?);
                }
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
        }

        Ok(TokenTree::Delim(subtree))
    }

    fn token_tree(&mut self) -> PR<TokenTree> {
        match self.iter.peek().val() {
            Some(LeftCurly | LeftParen | LeftBrace) => {
                let tt = self.delim_token_tree();
                match_tok!(self, RightCurly | RightParen | RightBrace);
                tt
            }
            Some(_) => Ok(TokenTree::Token(self.iter.next().unwrap())),
            None => todo!(),
        }
    }
}
