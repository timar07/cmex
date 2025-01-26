use std::collections::{hash_map::Entry, HashMap};

use cmex_ast::token::Token;
use cmex_ast::{Nonterminal, NtTag};
use cmex_parser::Parser;
use cmex_span::{MaybeSpannable, Span};

use crate::{MacroTokenTree, RepOpTag};

/// Early parser position.
/// For more information, see:
/// <https://en.wikipedia.org/wiki/Earley_parser#The_algorithm>
#[derive(Debug, Clone)]
pub struct MatcherPos {
    i: usize,
    matches: Vec<BoundMatch>,
}

impl MatcherPos {
    #[inline]
    pub fn push_match(
        &mut self,
        metavar_index: usize,
        depth: usize,
        m: BoundMatch,
    ) {
        match depth {
            0 => {
                self.matches.push(m);
            }
            _ => {
                let mut curr = &mut self.matches[metavar_index];
                for _ in 0..depth - 1 {
                    match curr {
                        BoundMatch::Seq(seq) => curr = seq.last_mut().unwrap(),
                        _ => unreachable!(),
                    }
                }
                match curr {
                    BoundMatch::Seq(seq) => seq.push(m),
                    _ => unreachable!(),
                }
            }
        }
    }
}

pub enum EofMatcherPos {
    None,
    One(MatcherPos),
    Multiple,
}

/// A match bound to some metavariable.
/// In other words, represents a *meaningful* match, that will be used
/// in macros' rhs.
#[derive(Debug, Clone)]
pub enum BoundMatch {
    Seq(Vec<BoundMatch>),
    Single(Nonterminal),
}

/// Represents current matcher state
#[derive(Debug)]
pub enum MatcherState {
    /// Single token
    Token(Token),
    Delim,
    Rep {
        rep: RepOpTag,
        index_after_rep: usize,
        next_metavar: usize,
        depth: usize,
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
        #[allow(dead_code)]
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
        id: Token,
        tag: NtTag,
        next_metavar: usize,
        seq_depth: usize,
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

    pub fn match_tt(
        &mut self,
        matcher: &[MatcherState],
        parser: &mut Parser,
        span: Span,
    ) -> MatchResult<HashMap<String, Option<BoundMatch>>> {
        self.curr_mps.clear();
        self.curr_mps.push(MatcherPos {
            i: 0,
            matches: vec![],
        });

        loop {
            self.next_mps.clear();
            self.nt_mps.clear();

            let res = self.predict(parser.iter.peek().as_ref(), matcher, span);

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
                        parser.iter.peek().span().unwrap()
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
                    // nonterminal in queue -- call the parser
                    let mut mp = self.nt_mps.pop().unwrap();
                    let loc = &matcher[mp.i];

                    if let &MatcherState::MetaVarDecl {
                        tag,
                        next_metavar,
                        seq_depth,
                        ..
                    } = loc
                    {
                        let nt = match parser.parse_nt(tag) {
                            Ok(nt) => nt,
                            Err(_) => return MatchResult::Error(
                                format!("error occured while parsing nonterminal `{tag}`"),
                                parser.iter.peek().span().unwrap()
                            )
                        };

                        mp.push_match(
                            next_metavar,
                            seq_depth,
                            BoundMatch::Single(nt),
                        );
                        mp.i += 1;
                    }

                    self.curr_mps.push(mp);
                }
                (_, _) => {
                    return MatchResult::Error("ambiguity error".into(), span);
                }
            }
        }
    }

    fn predict(
        &mut self,
        token: Option<&Token>,
        matcher: &[MatcherState],
        span: Span,
    ) -> Option<MatchResult<HashMap<String, Option<BoundMatch>>>> {
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
                MatcherState::Delim => {
                    mp.i += 1;
                    self.curr_mps.push(mp);
                }
                &MatcherState::Rep {
                    rep,
                    index_after_rep,
                    depth,
                    next_metavar,
                } => {
                    // TODO: instead of +2 actually count metavar decls
                    for i in next_metavar..next_metavar + 2 {
                        mp.push_match(i, depth, BoundMatch::Seq(vec![]));
                    }

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

                    if tag != RepOpTag::Quest {
                        // Try another repetition.
                        mp.i = index_first;
                        self.curr_mps.push(mp);
                    }
                }
                &MatcherState::RepOpAfterSep { index_first, .. } => {
                    mp.i = index_first;
                    self.curr_mps.push(mp);
                }
                MatcherState::RepSep(sep) => {
                    let end = MatcherPos {
                        i: mp.i + 2,
                        matches: mp.matches.clone(),
                    };
                    self.curr_mps.push(end);

                    if token.is_some_and(|(t, _)| *t == sep.0) {
                        mp.i += 1;
                        self.next_mps.push(mp);
                    } else {
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
                    self.bind_names(matcher, eof_mp.matches.into_iter())
                }
                EofMatcherPos::Multiple => {
                    return Some(MatchResult::Fail(
                        "ambiguity error: multiple successful parses".into(),
                        span,
                    ));
                }
                EofMatcherPos::None => {
                    return Some(MatchResult::Fail(
                        "missing tokens in macro token tree".into(),
                        span,
                    ))
                }
            })
        } else {
            None
        }
    }

    /// Checks that every metavariable in matcher has only one binding.
    fn bind_names<I>(
        &self,
        matcher: &[MatcherState],
        mut iter: I,
    ) -> MatchResult<HashMap<String, Option<BoundMatch>>>
    where
        I: Iterator<Item = BoundMatch>,
    {
        let mut syms = HashMap::new();

        for state in matcher {
            if let MatcherState::MetaVarDecl { id, .. } = state {
                match syms.entry(id.0.to_string()) {
                    Entry::Occupied(_) => {
                        return MatchResult::Error(
                            format!(
                                "redefinition of metavariable name `{}`",
                                id.0
                            ),
                            id.1,
                        )
                    }
                    Entry::Vacant(cell) => {
                        cell.insert(iter.next());
                    }
                }
            }
        }

        MatchResult::Success(syms)
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
    pub fn generate_substates(
        matcher: &[MacroTokenTree],
    ) -> Result<Vec<MatcherState>, (String, Span)> {
        let mut locs = Vec::new();
        let mut next_metavar = 0;
        Self::substates_from_tts(matcher, &mut locs, &mut next_metavar, 0)?;
        locs.push(MatcherState::End);
        Ok(locs)
    }

    fn substates_from_tts(
        matcher: &[MacroTokenTree],
        locs: &mut Vec<MatcherState>,
        next_metavar: &mut usize,
        seq_depth: usize,
    ) -> Result<(), (String, Span)> {
        for item in matcher {
            match item {
                MacroTokenTree::Delim(mtt) => {
                    locs.push(MatcherState::Delim);
                    let (left_delim, right_delim) = mtt.delim.get_delims();
                    locs.push(MatcherState::Token((left_delim, mtt.span.0)));
                    Self::substates_from_tts(
                        &mtt.mtt,
                        locs,
                        next_metavar,
                        seq_depth,
                    )?;
                    locs.push(MatcherState::Token((right_delim, mtt.span.1)));
                }
                MacroTokenTree::Token(tok) => {
                    locs.push(MatcherState::Token(tok.clone()))
                }
                MacroTokenTree::Rep(matcher, sep, rep) => {
                    locs.push(MatcherState::End);
                    let temp = *next_metavar;
                    let index_first = locs.len();
                    let index_seq = index_first - 1;
                    Self::substates_from_tts(
                        matcher,
                        locs,
                        next_metavar,
                        seq_depth + 1,
                    )?;

                    if let Some(sep) = sep {
                        locs.push(MatcherState::RepSep(sep.clone()));
                        locs.push(MatcherState::RepOpAfterSep {
                            tag: *rep,
                            index_first,
                        })
                    } else {
                        locs.push(MatcherState::RepOpNoSep {
                            tag: *rep,
                            index_first,
                        })
                    }

                    locs[index_seq] = MatcherState::Rep {
                        rep: *rep,
                        index_after_rep: locs.len(),
                        depth: seq_depth,
                        next_metavar: temp,
                    };
                }
                MacroTokenTree::Frag(id, tag) => {
                    locs.push(MatcherState::MetaVarDecl {
                        id: id.clone(),
                        tag: tag.ok_or_else(|| {
                            (
                                "expected fragment kind specifier".into(),
                                id.1.hi(),
                            )
                        })?,
                        seq_depth,
                        next_metavar: *next_metavar,
                    });
                    *next_metavar += 1;
                }
            }
        }

        Ok(())
    }
}

#[derive(Debug)]
pub enum MatchResult<T> {
    Error(String, Span),
    Fail(String, Span),
    Success(T),
}
