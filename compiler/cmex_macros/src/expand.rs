use std::collections::HashMap;

use cmex_ast::token::{Token, TokenTag};
use cmex_ast::{
    Decl, DelimSpan, DelimTag, Expr, ExprTag, Initializer, InvocationTag,
    Nonterminal, NtTag, Stmt, StmtTag, TokenTree, TranslationUnit,
};
use cmex_lexer::Tokens;
use cmex_parser::{ParseErrorTag, Parser};
use cmex_span::Span;
use cmex_symtable::SymTable;

use crate::{
    matcher::{BoundMatch, MatchResult, StatesParser, TtMatcher},
    parse::MacroParser,
    tt_cursor::TtCursor,
    DelimMtt, MacroMatcher, MacroRule, MacroTokenTree, RepOpTag,
};

type ExpRes<T> = Result<T, (ExpError, Span)>;

#[derive(Debug)]
pub enum ExpError {
    NtParseError(ParseErrorTag),
    UseOfUndeclaredMacro(String),
}

impl std::fmt::Display for ExpError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NtParseError(parse_error) => {
                write!(f, "nonterminal parse error: {parse_error}")
            }
            Self::UseOfUndeclaredMacro(name) => {
                write!(f, "use of undeclared macro {name}")
            }
        }
    }
}

#[derive(Default)]
pub struct MacroExpander {
    decls: SymTable<String, Vec<MacroRule>>,
}

impl MacroExpander {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn expand_ast(&mut self, root: &mut TranslationUnit) -> ExpRes<()> {
        for decl in &mut root.0 {
            self.expand_decl(decl)?;
        }

        Ok(())
    }

    fn expand_decl(&mut self, decl: &mut Decl) -> ExpRes<()> {
        match decl {
            Decl::Func { body, .. } => {
                self.expand_stmt(body)?;
            }
            Decl::Var { decl_list, .. } => {
                for decl in decl_list {
                    if let Some(init) = &mut decl.1 {
                        self.expand_initializer(init)?;
                    }
                }
            }
            Decl::Macro { id, body } => {
                if let TokenTag::Identifier(name) = &id.0 {
                    self.decls
                        .define(
                            name.clone(),
                            MacroParser::new(body.clone()).parse(),
                        )
                        .unwrap();
                }
            }
            _ => {}
        }

        Ok(())
    }

    fn expand_initializer(&mut self, init: &mut Initializer) -> ExpRes<()> {
        match init {
            Initializer::Assign(expr) => self.expand_expr(expr)?,
            Initializer::List(vec) => {
                for init in vec {
                    self.expand_initializer(init)?;
                }
            }
        }

        Ok(())
    }

    fn expand_expr(&mut self, expr: &mut Expr) -> ExpRes<()> {
        match &mut expr.tag {
            ExprTag::Invocation(invocation_tag) => {
                *expr = self.expand_invocation(invocation_tag)?;
            }
            ExprTag::BinExpr { lhs, rhs, .. } => {
                self.expand_expr(lhs.as_mut())?;
                self.expand_expr(rhs.as_mut())?;
            }
            ExprTag::UnExpr { rhs, .. } => {
                self.expand_expr(rhs.as_mut())?;
            }
            ExprTag::Call { calle, args } => {
                self.expand_expr(calle)?;

                for arg in args {
                    self.expand_expr(arg)?;
                }
            }
            ExprTag::MemberAccess { expr, .. } => {
                self.expand_expr(expr)?;
            }
            ExprTag::SizeofExpr { expr } => {
                self.expand_expr(expr)?;
            }
            ExprTag::CastExpr { expr, .. } => {
                self.expand_expr(expr)?;
            }
            ExprTag::Conditional {
                cond,
                then,
                otherwise,
            } => {
                self.expand_expr(cond)?;
                self.expand_expr(then)?;
                self.expand_expr(otherwise)?;
            }
            _ => {} // Nothing to expand otherwise
        }

        Ok(())
    }

    fn expand_stmt(&mut self, stmt: &mut Stmt) -> ExpRes<()> {
        match &mut stmt.tag {
            StmtTag::Expr(Some(expr)) => {
                self.expand_expr(expr)?;
            }
            StmtTag::Compound(vec) => {
                for stmt in vec {
                    self.expand_stmt(stmt)?;
                }
            }
            StmtTag::Decl(decl) => {
                self.expand_decl(decl)?;
            }
            StmtTag::While { cond, stmt } => {
                self.expand_expr(cond)?;
                self.expand_stmt(stmt)?;
            }
            StmtTag::Do { cond, stmt } => {
                self.expand_expr(cond)?;
                self.expand_stmt(stmt)?;
            }
            StmtTag::For(expr, expr1, expr2, _) => {
                for expr in [expr, expr1, expr2].into_iter().flatten() {
                    self.expand_expr(expr)?;
                }
            }
            StmtTag::If(expr, then, otherwise) => {
                self.expand_expr(expr)?;
                self.expand_stmt(then)?;
                if let Some(stmt) = otherwise {
                    self.expand_stmt(stmt)?;
                }
            }
            StmtTag::Switch(expr, stmt) => {
                self.expand_expr(expr)?;
                self.expand_stmt(stmt)?;
            }
            StmtTag::Case(expr, stmt) => {
                self.expand_expr(expr)?;
                self.expand_stmt(stmt)?;
            }
            StmtTag::Label(_, stmt) => {
                self.expand_stmt(stmt)?;
            }
            StmtTag::Default(stmt) => {
                self.expand_stmt(stmt)?;
            }
            _ => {}
        }

        Ok(())
    }

    fn expand_invocation(&mut self, mac: &mut InvocationTag) -> ExpRes<Expr> {
        match mac {
            InvocationTag::Bang(id, tt) => {
                let toks = Tokens(
                    tt.clone()
                        .map(|tt| TtCursor::new(&vec![tt]).collect())
                        .unwrap_or(Vec::new()),
                );
                let mut parser = Parser::new(&toks);
                let decl =
                    self.decls.lookup(&id.0.to_string()).ok_or_else(|| {
                        (ExpError::UseOfUndeclaredMacro(id.0.to_string()), id.1)
                    })?;

                let matchers: Vec<MacroMatcher> =
                    decl.iter().map(|rule| rule.0.clone()).collect();

                let Ok((index, captures)) =
                    dbg!(match_macro(&matchers, &mut parser, id.1))
                else {
                    panic!("match failed");
                };

                let rhs = &decl[index].1;
                let tokens: Vec<(TokenTag, cmex_span::Span)> =
                    expand(captures, rhs)
                        .unwrap()
                        .iter()
                        .map(|tt| tt.flatten())
                        .reduce(|a, b| [a, b].concat())
                        .unwrap_or_else(Vec::new);

                dbg!(&tokens);
                match Parser::new(&Tokens(tokens)).parse_nt(NtTag::Expr) {
                    Ok(Nonterminal::Expr(exp)) => Ok(dbg!(exp)),
                    Err((e, span)) => Err((ExpError::NtParseError(e), span)),
                    _ => unreachable!(),
                }
            }
        }
    }
}

struct Frame<'a> {
    mtt: &'a [MacroTokenTree],
    i: usize,
    tag: FrameTag,
}

enum FrameTag {
    Delim(DelimTag, DelimSpan),
    Rep(Option<Token>, RepOpTag),
}

impl<'a> Frame<'a> {
    pub fn delim(inner: &'a DelimMtt, span: DelimSpan) -> Self {
        Self {
            mtt: &inner.mtt,
            i: 0,
            tag: FrameTag::Delim(inner.delim, span),
        }
    }

    pub fn rep(
        mtt: &'a [MacroTokenTree],
        sep: Option<Token>,
        rep_op: RepOpTag,
    ) -> Self {
        Self {
            mtt,
            i: 0,
            tag: FrameTag::Rep(sep, rep_op),
        }
    }
}

impl<'a> Iterator for Frame<'a> {
    type Item = &'a MacroTokenTree;

    fn next(&mut self) -> Option<Self::Item> {
        self.mtt.get(self.i).inspect(|_| {
            self.i += 1;
        })
    }
}

fn expand(
    captures: HashMap<String, Option<BoundMatch>>,
    rhs: &DelimMtt,
) -> Result<Vec<TokenTree>, ()> {
    let mut result = Vec::new();
    let mut result_stack = Vec::new();
    let mut stack = vec![Frame::delim(rhs, rhs.span.clone())];
    let mut repeats: Vec<(usize, usize)> = Vec::new();

    loop {
        let Some(tree) = stack.last_mut().unwrap().next() else {
            let frame = stack.last_mut().unwrap();
            if let FrameTag::Rep(sep, _) = &frame.tag {
                let (index, len) = repeats.last_mut().unwrap();
                *index += 1;

                if index < len {
                    frame.i = 0;
                    if let Some(sep) = sep {
                        result.push(TokenTree::Token(sep.clone()));
                    }
                    continue;
                }
            }

            match stack.pop().unwrap().tag {
                FrameTag::Rep(_, _) => {
                    repeats.pop();
                }
                FrameTag::Delim(delim, span) => {
                    if result_stack.is_empty() {
                        return Ok(result);
                    }

                    let tree = TokenTree::Delim(delim, result, span);
                    result = result_stack.pop().unwrap();
                    result.push(tree);
                }
            }

            continue;
        };

        match tree {
            MacroTokenTree::Frag(id, _) => {
                // TODO: `tag` option is unnecessary, fix it

                if let Some(matched) =
                    lookup_current_match(id, &captures, &repeats)
                {
                    let tt = match matched {
                        BoundMatch::Single(
                            Nonterminal::Literal(tok) | Nonterminal::Ident(tok),
                        ) => TokenTree::Token(tok),
                        BoundMatch::Single(t) => {
                            TokenTree::Token((
                                TokenTag::Interpolated(Box::new(t)),
                                id.1, // Use metavar span for it
                            ))
                        }
                        BoundMatch::Seq(..) => {
                            panic!("variable is still repeating")
                        }
                    };

                    result.push(tt);
                } else {
                    todo!()
                }
            }
            mtt @ MacroTokenTree::Rep(seq, sep, rep) => {
                match get_repetition_len(mtt, &captures, &repeats) {
                    Some(len) => {
                        repeats.push((0, len));
                        stack.push(Frame::rep(seq, sep.clone(), *rep))
                    }
                    None => todo!(),
                }
            }
            MacroTokenTree::Delim(delim) => {
                stack.push(Frame::delim(delim, delim.span.clone()));
                result_stack.push(std::mem::take(&mut result));
            }
            MacroTokenTree::Token(tok) => {
                result.push(TokenTree::Token(tok.clone()))
            }
            _ => panic!(),
        }
    }
}

fn get_repetition_len(
    tree: &MacroTokenTree,
    captures: &HashMap<String, Option<BoundMatch>>,
    repeats: &[(usize, usize)],
) -> Option<usize> {
    match tree {
        MacroTokenTree::Token(_) => None,
        MacroTokenTree::Rep(vec, _, _) => vec.iter().fold(None, |acc, mtt| {
            acc.or_else(|| get_repetition_len(mtt, captures, repeats))
        }),
        MacroTokenTree::Delim(delim) => {
            delim.mtt.iter().fold(None, |acc, mtt| {
                acc.or_else(|| get_repetition_len(mtt, captures, repeats))
            })
        }
        MacroTokenTree::Frag(id, _) => {
            match lookup_current_match(id, captures, repeats) {
                Some(matched) => match matched {
                    BoundMatch::Seq(vec) => Some(vec.len()),
                    BoundMatch::Single(_) => None,
                },
                _ => None,
            }
        }
    }
}

fn lookup_current_match(
    id: &Token,
    captures: &HashMap<String, Option<BoundMatch>>,
    repeats: &[(usize, usize)],
) -> Option<BoundMatch> {
    captures
        .get(&id.0.to_string())
        .unwrap()
        .as_ref()
        .map(|mut bound_match| {
            for &(index, _) in repeats {
                match bound_match {
                    BoundMatch::Seq(vec) => {
                        bound_match = vec.get(index).unwrap()
                    }
                    BoundMatch::Single(_) => break,
                }
            }

            bound_match.clone()
        })
}

/// Returns matched arm index and hashmap with bound matches.
fn match_macro(
    matchers: &[MacroMatcher],
    parser: &mut Parser,
    span: Span,
) -> Result<(usize, HashMap<String, Option<BoundMatch>>), (String, Span)> {
    let mut tt_matcher = TtMatcher::new();

    for (i, matcher) in matchers.iter().enumerate() {
        println!("Matcing arm {i}");
        let res = tt_matcher
            .match_tt(&StatesParser::generate_substates(&matcher.0)?, parser);

        match res {
            MatchResult::Error(msg, _) => {
                panic!("{msg}");
            }
            MatchResult::Fail(_, _) => {
                println!("failed to match macro, retrying")
            }
            MatchResult::Success(captures) => return Ok((i, captures)),
        }
    }

    Err(("no arms matched this invocation input".into(), span))
}
