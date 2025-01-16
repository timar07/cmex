use cmex_ast::{DelimTag, TokenTree};
use cmex_span::{Span, Unspan};
use cmex_lexer::TokenTag::{self, *};
use crate::tt_cursor::TtCursor;

use crate::{MacroFragSpec, MacroMatch, MacroMatcher, MacroRule, RepOpTag};

type PR<T> = Result<T, (ParseErrorTag, Span)>;

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
    }
}

/// Parse macros as a TokenTree -> internal macro representation.
/// Macros have their own separate parser because it's basically a
/// seperate part of the language with its own syntax.
pub struct MacroParser {
    tt: TokenTree,
}

impl MacroParser {
    pub fn new(tt: TokenTree) -> Self {
        Self {
            tt,
        }
    }

    /// Parse macro body as an iterator over tokens
    pub fn parse(&mut self) -> Vec<MacroRule> {
        self.macro_rules_def()
    }

    fn macro_rules_def(&mut self) -> Vec<MacroRule> {
        match &self.tt {
            TokenTree::Delim(_, tt) => self.macro_rules(tt.to_vec()),
            TokenTree::Token(_) => panic!("expected macro rules body"),
        }
    }

    fn macro_rules(&mut self, tt: Vec<TokenTree>) -> Vec<MacroRule> {
        let mut iter = tt.iter().peekable();
        let mut rules = Vec::new();

        while iter.peek().is_some() {
            let rule = self.macro_rule(
                iter
                    .by_ref()
                    .take(3)
                    .cloned()
                    .collect()
            );
            rules.push(rule);

            if iter.peek().is_some() {
                require_tok!(iter, Semicolon);
            }
        }

        rules
    }

    fn macro_rule(&mut self, tt: Vec<TokenTree>) -> MacroRule {
        let mut iter = tt.iter().peekable();

        let matcher = match iter.next() {
            Some(TokenTree::Delim(_, tt)) => {
                self.macro_matcher(&mut TtCursor::new(tt))
            },
            _ => panic!("expected macro matcher")
        };

        require_tok!(iter, FatArrow);

        MacroRule(matcher, iter.next().unwrap().clone())
    }

    fn macro_matcher(&mut self, mut iter: &mut TtCursor<'_>) -> MacroMatcher {
        let mut matcher = Vec::new();

        while let Some(mmatch) = self.macro_match(&mut iter) {
            matcher.push(mmatch);
        }

        MacroMatcher(matcher)
    }

    fn macro_match(&mut self, mut iter: &mut TtCursor<'_>) -> Option<MacroMatch> {
        match iter.next() {
            Some((TokenTag::Dollar, _)) => {
                match iter.peek_tree() {
                    Some(TokenTree::Delim(DelimTag::Paren, inner)) => {
                        let matcher = Box::new(self.macro_matcher(
                            &mut TtCursor::new(inner)
                        ));
                        iter.next_tree();

                        match iter.peek().val() {
                            Some(TokenTag::Plus | TokenTag::Asterisk | TokenTag::Quest) => {
                                Some(MacroMatch::Rep(
                                    matcher,
                                    None,
                                    self.rep_op(&mut iter)
                                ))
                            },
                            Some(_) => {
                                Some(MacroMatch::Rep(
                                    matcher,
                                    iter.next(),
                                    self.rep_op(&mut iter)
                                ))
                            },
                            _ => panic!()
                        }
                    },
                    _ => self.macro_frag(&mut iter)
                }
            },
            Some(tok) => {
                Some(MacroMatch::Token(tok.clone()))
            },
            _ => None
        }
    }

    fn rep_op(&mut self, iter: &mut TtCursor<'_>) -> RepOpTag {
        match iter.next().val() {
            Some(TokenTag::Plus) => RepOpTag::Plus,
            Some(TokenTag::Asterisk) => RepOpTag::Asterisk,
            Some(TokenTag::Quest) => RepOpTag::Quest,
            tok => panic!("unknown repetition operator {:?}", tok)
        }
    }

    fn macro_frag(&mut self, iter: &mut TtCursor<'_>) -> Option<MacroMatch> {
        match iter.next() {
            Some(tok) if tok.0.is_keyword_or_id() => {
                if let Some(Colon) = iter.next().val() {
                    Some(MacroMatch::Frag(
                        tok.0.to_string(),
                        self.macro_frag_spec(iter)
                    ))
                } else {
                    panic!("expected `:`")
                }
            },
            _ => panic!("expected matcher")
        }
    }

    fn macro_frag_spec(&mut self, iter: &mut TtCursor<'_>) -> MacroFragSpec {
        if let Some(Identifier(spec)) = iter.next().val() {
            match spec.as_str() {
                "block" => MacroFragSpec::Block,
                "literal" => MacroFragSpec::Literal,
                "ident" => MacroFragSpec::Ident,
                "item" => MacroFragSpec::Item,
                "ty" => MacroFragSpec::Ty,
                "expr" => MacroFragSpec::Expr,
                "pat" => todo!(),
                _ => panic!("incorrect fragment specifier")
            }
        } else {
            panic!("expected fragment specifier")
        }
    }
}

pub enum ParseErrorTag {
    Expected(String),
    ExpectedGot(String, Option<TokenTag>),
    ExpectedValidTokenTree
}
