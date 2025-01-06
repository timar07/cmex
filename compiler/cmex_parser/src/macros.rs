//! This file implements Rust-like macros.
//! For information about grammar, see:
//! <https://doc.rust-lang.org/reference/macros-by-example.html#r-macro.decl.syntax>

use crate::{check_tok, match_tok, require_tok, ParseErrorTag, Parser, PR};
use cmex_ast::Decl;
use cmex_lexer::TokenTag::*;
use cmex_span::{Span, Unspan};
use std::ops::Deref;

impl Parser<'_> {
    pub fn macro_rules_definition(&mut self) -> PR<Decl> {
        self.iter.next();
        require_tok!(self, Not)?;
        let id = require_tok!(self, Identifier(_))?;

        self.macro_rules_def()?;

        Ok(Decl::Macro {})
    }

    fn macro_rules_def(&mut self) -> PR<()> {
        match self.iter.peek().val() {
            Some(LeftCurly) => {
                self.iter.next();
                self.macro_rules()?;
                require_tok!(self, RightCurly)?;
            }
            Some(_) => {
                return Err((
                    ParseErrorTag::Expected("macro rules definition".into()),
                    self.iter.peek().unwrap().1,
                ))
            }
            _ => panic!(),
        }

        Ok(())
    }

    fn macro_rules(&mut self) -> PR<()> {
        self.macro_rule()?; // TODO: ; delimited
        Ok(())
    }

    fn macro_rule(&mut self) -> PR<()> {
        self.macro_matcher()?;
        require_tok!(self, FatArrow)?;
        self.macro_transcriber()?;
        Ok(())
    }

    fn macro_matcher(&mut self) -> PR<()> {
        match self.iter.peek().val() {
            Some(LeftParen) => {
                self.iter.next();

                if !check_tok!(self, RightParen) {
                    self.macro_match()?;
                    require_tok!(self, RightParen)?;
                }
            }
            _ => todo!(),
        }
        Ok(())
    }

    fn macro_match(&mut self) -> PR<()> {
        match self.iter.peek().val() {
            Some(Dollar) => {
                self.iter.next();

                match self.iter.next().val() {
                    Some(t @ Identifier(_)) | Some(t) if t.is_keyword() => {
                        require_tok!(self, Colon)?;
                        self.macro_frag_spec()?;
                    }
                    _ => {
                        require_tok!(self, LeftParen)?;
                        if !check_tok!(self, RightParen) {
                            self.macro_match()?;
                            require_tok!(self, RightParen)?;
                        }

                        if let Some(rep_op) = self.maybe_rep_op() {
                            return Ok(());
                        }

                        self.iter.peek().ok_or_else(|| {
                            (
                                ParseErrorTag::Expected("separator".into()),
                                self.iter.peek().unwrap().1,
                            )
                        })?;
                    }
                }
            }
            Some(RightParen) => {
                dbg!("asdf");
                self.iter.next();
            }
            Some(t) if t.is_delimiter() => {
                self.macro_matcher()?;
            }
            Some(t) => {
                todo!()
            }
            _ => panic!(),
        }

        Ok(())
    }

    fn maybe_rep_op(&mut self) -> Option<()> {
        if check_tok!(self, Asterisk | Plus | Quest) {
            Some(())
        } else {
            None
        }
    }

    /// Since there is no visibilies, lifetimes, attributes
    /// in C, so they are removed to match language semantics.
    fn macro_frag_spec(&mut self) -> PR<()> {
        if let Some(Identifier(spec)) = self.iter.peek().val() {
            match spec.deref() {
                "block" | "ident" | "item" | "literal" | "ty" => Ok(()),
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

    fn macro_transcriber(&mut self) -> PR<()> {
        self.delim_token_tree()
    }

    fn delim_token_tree(&mut self) -> PR<()> {
        match self.iter.peek().val() {
            Some(LeftCurly) => {
                self.iter.next();

                while !check_tok!(self, RightCurly) {
                    self.token_tree()?;
                }
            }
            _ => panic!(),
        }

        Ok(())
    }

    fn token_tree(&mut self) -> PR<()> {
        match self.iter.peek().val() {
            Some(LeftCurly | LeftParen | LeftBrace) => self.delim_token_tree(),
            Some(tok) => {
                self.iter.next();
                Ok(())
            }
            None => todo!(),
        }
    }
}
