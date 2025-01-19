use cmex_ast::{Nonterminal, NtTag};
use cmex_lexer::Token;
use cmex_parser::Parser;
use cmex_span::Span;

use crate::{MacroMatch, RepOpTag};

#[derive(Debug)]
pub struct MatcherPos {
    i: usize,
    matches: Vec<Nonterminal>,
}

pub enum EofMatcherPos {
    None,
    One(MatcherPos),
    Multiple,
}

/// Represents current matcher state
pub enum MatcherState {
    /// Single token
    Token(Token),
    Rep {
        rep: RepOpTag,
        index_after_rep: usize,
    },
    /// Repetition separator
    /// ```ignore
    /// $($x:expr),+
    ///        /* ^ RepSep */
    /// ```
    RepSep(Token),
    /// Repetition operator after separator
    /// ```ignore
    /// $($x:expr),+
    ///         /* ^ RepOpAfterSep */
    /// ```
    RepOpAfterSep {
        tag: RepOpTag,
        index_first: usize,
    },
    /// Repetition operator that is not preceeded by a separator
    /// ```ignore
    /// $(a)+
    ///  /* ^ RepOpNoSep */
    /// ```
    RepOpNoSep {
        tag: RepOpTag,
        index_first: usize,
    },
    MetaVarDecl {
        tag: NtTag,
    },
    /// End state
    End,
}

pub struct TtMatcher {
    curr_mps: Vec<MatcherPos>,
    nt_mps: Vec<MatcherPos>,
    next_mps: Vec<MatcherPos>,
}

impl TtMatcher {
    pub fn new() -> Self {
        Self {
            curr_mps: Vec::new(),
            nt_mps: Vec::new(),
            next_mps: Vec::new(),
        }
    }

    pub fn match_tt<'a>(
        &mut self,
        matcher: &'a [MatcherState],
        parser: &mut Parser,
    ) -> MatchResult<Vec<Nonterminal>> {
        self.curr_mps.clear();
        self.curr_mps.push(MatcherPos {
            i: 0,
            matches: vec![],
        });

        loop {
            self.next_mps.clear();
            self.nt_mps.clear();

            let res = self.parse_tt_inner(parser.iter.peek().as_ref(), matcher);

            // Result emited
            if let Some(res) = res {
                return res;
            }

            assert!(self.curr_mps.is_empty());

            match (self.next_mps.len(), self.nt_mps.len()) {
                (0, 0) => {
                    // Got no possible next positions and no non-terminals --
                    // throw an error
                    return MatchResult::Fail(
                        format!(
                            "no rules matches token `{}`",
                            parser
                                .iter
                                .peek()
                                .map(|(tok, _)| tok.to_string())
                                .unwrap_or_default()
                        ),
                        parser.iter.peek().unwrap().1,
                    );
                }
                (_, 0) => {
                    // Found a next position that is a terminal -- append
                    // tokens to queue
                    self.curr_mps.append(&mut self.next_mps);
                    // Consume the token
                    parser.next();
                }
                (0, 1) => {
                    // No possible terminal transitions and we got a
                    // non terminal in queue -- call the parser
                    let mut mp = self.nt_mps.pop().unwrap();
                    let loc = &matcher[mp.i];

                    if let MatcherState::MetaVarDecl { tag } = loc {
                        let nt = match parser.parse_nt(tag) {
                            Ok(nt) => nt,
                            Err(_) => return MatchResult::Error(
                                format!("error occured while parsing nonterminal `{tag}`"),
                                parser.iter.peek().unwrap().1
                            )
                        };

                        mp.matches.push(nt);
                        mp.i += 1;
                    }

                    self.curr_mps.push(mp);
                }
                (_, _) => {
                    panic!("ambiguity error")
                }
            }

            dbg!(&self.curr_mps);
        }
    }

    fn parse_tt_inner<'a>(
        &mut self,
        token: Option<&Token>,
        matcher: &'a [MatcherState],
    ) -> Option<MatchResult<Vec<Nonterminal>>> {
        let mut eof_mp = EofMatcherPos::None;

        while let Some(mut mp) = self.curr_mps.pop() {
            match &matcher[mp.i] {
                MatcherState::Token(tok) => {
                    if token.is_some_and(|t| tok.0 == t.0) {
                        mp.i += 1;
                        self.next_mps.push(mp);
                    }
                }
                MatcherState::MetaVarDecl { .. } => {
                    self.nt_mps.push(mp);
                }
                &MatcherState::Rep {
                    rep,
                    index_after_rep,
                } => {
                    // As a possible next position, we can try zero repetitions of it
                    if matches!(rep, RepOpTag::Asterisk | RepOpTag::Quest) {
                        self.curr_mps.push(MatcherPos {
                            i: index_after_rep,
                            matches: mp.matches.clone(),
                        });
                    }

                    // Also try one or more repetition
                    mp.i += 1;
                    self.curr_mps.push(mp);
                }
                &MatcherState::RepOpNoSep { tag, index_first } => {
                    let end = MatcherPos {
                        i: mp.i + 1,
                        matches: mp.matches.clone(),
                    };
                    self.curr_mps.push(end);
                    // TODO
                    if tag != RepOpTag::Quest {
                        // Try another repetition.
                        mp.i = index_first;
                        self.curr_mps.push(mp);
                    }
                }
                &MatcherState::RepOpAfterSep { tag, index_first } => {
                    mp.i = index_first;
                    self.curr_mps.push(mp);
                }
                MatcherState::RepSep(sep) => {
                    let end = MatcherPos {
                        i: mp.i + 2,
                        matches: mp.matches.clone(),
                    };
                    self.curr_mps.push(end);

                    println!("{:?} {:?}", token.map(|t| t.0.clone()), sep.0);

                    if token.is_some_and(|(t, _)| *t == sep.0) {
                        mp.i += 1;
                        self.next_mps.push(mp);
                    } else {
                        dbg!("expected token");
                        // TODO: Expected token
                    }
                }
                MatcherState::End => {
                    // Reached end-of-sequence
                    if token.is_none() {
                        eof_mp = match eof_mp {
                            EofMatcherPos::None => EofMatcherPos::One(mp),
                            EofMatcherPos::One(_) | EofMatcherPos::Multiple => {
                                EofMatcherPos::Multiple
                            }
                        }
                    }
                }
            }
        }

        if token.is_none() {
            Some(match eof_mp {
                EofMatcherPos::One(eof_mp) => {
                    MatchResult::Success(eof_mp.matches)
                }
                EofMatcherPos::Multiple => {
                    panic!("ambiguity error: multiple successful parses")
                }
                EofMatcherPos::None => {
                    panic!("missing tokens in macro token tree")
                }
            })
        } else {
            None
        }
    }

    fn bind_names() {
        todo!()
    }
}

pub(crate) struct StatesParser;

impl StatesParser {
    /// For matching, it's convenient to break down parsed macro match
    /// into small pieces called `MatcherState`, representing current matcher
    /// location (state).
    ///
    /// Consider following example:
    /// a a $(a),+ b; $(c)*
    ///
    /// will be converted to
    ///
    /// ```ignore
    /// [
    ///     Token,
    ///     Token,
    ///     Rep,
    ///     RepOpAfterSep,
    ///     Token,
    ///     Token,
    ///     Rep,
    ///     RepOpNoSep
    /// ]
    /// ```
    pub fn generate_substates(matcher: &[MacroMatch]) -> Vec<MatcherState> {
        let mut locs = Vec::new();
        Self::substates_from_tts(matcher, &mut locs);
        locs.push(MatcherState::End);
        locs
    }

    fn substates_from_tts(
        matcher: &[MacroMatch],
        locs: &mut Vec<MatcherState>,
    ) {
        for item in matcher {
            match item {
                MacroMatch::Token(tok) => {
                    locs.push(MatcherState::Token(tok.clone()))
                }
                MacroMatch::Rep(matcher, sep, rep) => {
                    locs.push(MatcherState::End);
                    let index_first = locs.len();
                    let index_seq = index_first - 1;
                    Self::substates_from_tts(&matcher.0, locs);

                    if let Some(sep) = sep {
                        locs.push(MatcherState::RepSep(sep.clone()));
                        locs.push(MatcherState::RepOpAfterSep {
                            tag: rep.clone(),
                            index_first,
                        })
                    } else {
                        locs.push(MatcherState::RepOpNoSep {
                            tag: rep.clone(),
                            index_first,
                        })
                    }

                    locs[index_seq] = MatcherState::Rep {
                        rep: rep.clone(),
                        index_after_rep: locs.len(),
                    };
                }
                MacroMatch::Frag(_, tag) => {
                    locs.push(MatcherState::MetaVarDecl { tag: tag.clone() });
                }
            }
        }
    }
}

#[derive(Debug)]
pub enum MatchResult<T> {
    Error(String, Span),
    Fail(String, Span),
    Success(T),
}
