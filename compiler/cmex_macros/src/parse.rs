use crate::{tt_cursor::TtCursor, DelimMtt};
use cmex_ast::token::TokenTag::{self, *};
use cmex_ast::{DelimTag, NtTag, TokenTree};
use cmex_span::{MaybeSpannable, Span, Spannable};

use crate::{MacroMatcher, MacroRule, MacroTokenTree, RepOpTag};

type PR<T> = Result<T, (String, Span)>;

macro_rules! require_tok {
    ($iter:expr, $pat:pat) => {
        if !match_tok!($iter, $pat) {
            panic!("expected {}, got {:?}", stringify!($pat), $iter.peek());
        }
    };
}

macro_rules! match_tok {
    ($iter:expr, $pat:pat) => {
        if matches!($iter.peek(), Some(TokenTree::Token(($pat, _)))) {
            $iter.next();
            true
        } else {
            false
        }
    };
}

/// Parse macros as a TokenTree -> internal macro representation.
/// Macros have their own separate parser because it's basically a
/// seperate part of the language with its own syntax.
pub struct MacroParser {
    tt: TokenTree,
}

impl MacroParser {
    pub fn new(tt: TokenTree) -> Self {
        Self { tt }
    }

    /// Parse macro body as an iterator over tokens
    pub fn parse(&mut self) -> PR<Vec<MacroRule>> {
        self.macro_rules_def()
    }

    fn macro_rules_def(&mut self) -> PR<Vec<MacroRule>> {
        match &self.tt {
            TokenTree::Delim(_, tt, _) => Ok(self.macro_rules(tt.to_vec())?),
            TokenTree::Token(_) => panic!("expected macro rules body"),
        }
    }

    fn macro_rules(&mut self, tt: Vec<TokenTree>) -> PR<Vec<MacroRule>> {
        let mut iter = tt.iter().peekable();
        let mut rules = Vec::new();

        while iter.peek().is_some() {
            let rule =
                self.macro_rule(iter.by_ref().take(3).cloned().collect())?;
            rules.push(rule);

            if iter.peek().is_some() {
                require_tok!(iter, Semicolon);
            }
        }

        Ok(rules)
    }

    fn macro_rule(&mut self, tt: Vec<TokenTree>) -> PR<MacroRule> {
        let mut iter = tt.iter().peekable();

        let matcher = match iter.next() {
            Some(TokenTree::Delim(_, tt, _)) => {
                self.macro_matcher(&mut TtCursor::new(tt))
            }
            Some(tt) => Err(("expected macro matcher".into(), tt.span())),
            None => panic!(),
        }?;

        require_tok!(iter, FatArrow);

        let rhs = match iter.next().unwrap().clone() {
            TokenTree::Delim(delim, body, span) => {
                let cursor = &mut TtCursor::new(&body);
                DelimMtt {
                    delim,
                    mtt: MacroTtParser::new(cursor).parse_mtt()?,
                    span,
                }
            }
            _ => panic!("expected delimited token tree"),
        };

        Ok(MacroRule(matcher, rhs))
    }

    fn macro_matcher<'a>(
        &mut self,
        iter: &'a mut TtCursor<'a>,
    ) -> PR<MacroMatcher> {
        Ok(MacroMatcher(MacroTtParser::new(iter).parse_mtt()?))
    }
}

pub struct MacroTtParser<'a> {
    iter: &'a mut TtCursor<'a>,
}

impl<'a> MacroTtParser<'a> {
    pub fn new(iter: &'a mut TtCursor<'a>) -> Self {
        Self { iter }
    }

    pub fn parse_mtt(&mut self) -> PR<Vec<MacroTokenTree>> {
        let mut mtts = Vec::new();

        while let Some(mtt) = self.parse_item()? {
            mtts.push(mtt);
        }

        Ok(mtts)
    }

    fn parse_item(&mut self) -> PR<Option<MacroTokenTree>> {
        Ok(match self.iter.next_tree() {
            Some(TokenTree::Token((TokenTag::Dollar, _))) => {
                match self.iter.peek_tree() {
                    Some(TokenTree::Delim(DelimTag::Paren, inner, _)) => {
                        let matcher = subparse(&inner)?;

                        self.iter.next_tree();

                        match self.iter.peek_tree() {
                            Some(TokenTree::Token((
                                TokenTag::Plus
                                | TokenTag::Asterisk
                                | TokenTag::Quest,
                                _,
                            ))) => Some(MacroTokenTree::Rep(
                                matcher,
                                None,
                                self.rep_op()?,
                            )),
                            Some(TokenTree::Token(tok)) => {
                                self.iter.next_tree();

                                Some(MacroTokenTree::Rep(
                                    matcher,
                                    Some(tok),
                                    self.rep_op()?,
                                ))
                            }
                            _ => {
                                return Err((
                                    "expected `+`, `*` or `?`".into(),
                                    self.iter.next().span().unwrap()
                                ))
                            }
                        }
                    }
                    _ => self.macro_frag(),
                }
            }
            Some(TokenTree::Token(tok)) => {
                Some(MacroTokenTree::Token(tok.clone()))
            }
            Some(TokenTree::Delim(delim, inner, span)) => {
                Some(MacroTokenTree::Delim(DelimMtt {
                    delim,
                    span,
                    mtt: subparse(&inner)?,
                }))
            }
            _ => None,
        })
    }

    fn rep_op(&mut self) -> PR<RepOpTag> {
        Ok(match self.iter.next() {
            Some((TokenTag::Plus, _)) => RepOpTag::Plus,
            Some((TokenTag::Asterisk, _)) => RepOpTag::Asterisk,
            Some((TokenTag::Quest, _)) => RepOpTag::Quest,
            Some((tok, span)) => {
                return Err((
                    format!("unknown repetition operator {:?}", tok),
                    span,
                ))
            }
            _ => panic!(),
        })
    }

    fn macro_frag(&mut self) -> Option<MacroTokenTree> {
        match self.iter.next_tree() {
            Some(TokenTree::Token(tok)) if tok.0.is_keyword_or_id() => {
                if let Some(TokenTree::Token((Colon, _))) =
                    self.iter.peek_tree()
                {
                    self.iter.next_tree();
                    Some(MacroTokenTree::Frag(
                        tok.clone(),
                        self.macro_frag_spec().ok(),
                    ))
                } else {
                    Some(MacroTokenTree::Frag(tok.clone(), None))
                }
            }
            _ => None,
        }
    }

    fn macro_frag_spec(&mut self) -> PR<NtTag> {
        if let Some(TokenTree::Token((Identifier(spec), span))) =
            self.iter.next_tree()
        {
            Ok(match spec.as_str() {
                "block" => NtTag::Block,
                "literal" => NtTag::Literal,
                "ident" => NtTag::Ident,
                "item" => NtTag::Item,
                "ty" => NtTag::Ty,
                "expr" => NtTag::Expr,
                "tt" => NtTag::Tt,
                "pat" => todo!(),
                _ => return Err(("expected fragment specifier".into(), span)),
            })
        } else {
            panic!("expected fragment specifier")
        }
    }
}

fn subparse(inner: &Vec<TokenTree>) -> PR<Vec<MacroTokenTree>> {
    MacroTtParser::new(&mut TtCursor::new(inner)).parse_mtt()
}
