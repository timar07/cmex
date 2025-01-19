use cmex_ast::{
    Decl, Expr, ExprTag, Initializer, InvocationTag, Stmt, StmtTag,
    TranslationUnit,
};
use cmex_lexer::{TokenTag, Tokens};
use cmex_parser::Parser;
use cmex_symtable::SymTable;

use crate::{
    matcher::{StatesParser, TtMatcher},
    parse::MacroParser,
    tt_cursor::TtCursor,
    MacroMatcher, MacroRule,
};

pub struct MacroExpander<'a> {
    parser: Parser<'a>,
    decls: SymTable<String, Vec<MacroRule>>,
}

impl<'a> MacroExpander<'a> {
    pub fn new(parser: Parser<'a>) -> Self {
        Self {
            parser,
            decls: SymTable::default(),
        }
    }

    pub fn expand_ast(&mut self, root: TranslationUnit) -> () {
        for mut decl in root.0 {
            self.expand_decl(&mut decl)
        }
    }

    fn expand_decl(&mut self, decl: &mut Decl) {
        match decl {
            Decl::Func { body, .. } => {
                self.expand_stmt(body);
            }
            Decl::Var { decl_list, .. } => {
                decl_list.iter_mut().for_each(|decl| {
                    if let Some(init) = &mut decl.1 {
                        self.expand_initializer(init);
                    }
                });
            }
            Decl::Macro { id, body } => {
                if let TokenTag::Identifier(name) = &id.0 {
                    println!("declared macro {}", name);
                    self.decls
                        .define(
                            name.clone(),
                            dbg!(MacroParser::new(body.clone()).parse()),
                        )
                        .unwrap();
                }
            }
            _ => {}
        }
    }

    fn expand_initializer(&mut self, init: &mut Initializer) {
        match init {
            Initializer::Assign(expr) => self.expand_expr(expr),
            Initializer::List(vec) => {
                vec.iter_mut()
                    .for_each(|init| self.expand_initializer(init));
            }
        }
    }

    fn expand_expr(&mut self, expr: &mut Expr) {
        match &mut expr.tag {
            ExprTag::Invocation(invocation_tag) => {
                self.expand_invocation(invocation_tag)
            }
            ExprTag::BinExpr { lhs, rhs, .. } => {
                self.expand_expr(lhs.as_mut());
                self.expand_expr(rhs.as_mut());
            }
            ExprTag::UnExpr { rhs, .. } => {
                self.expand_expr(rhs.as_mut());
            }
            ExprTag::Call { calle, args } => {
                self.expand_expr(calle);
                args.iter_mut().for_each(|arg| self.expand_expr(arg));
            }
            ExprTag::MemberAccess { expr, .. } => {
                self.expand_expr(expr);
            }
            ExprTag::SizeofExpr { expr } => {
                self.expand_expr(expr);
            }
            ExprTag::CastExpr { expr, .. } => {
                self.expand_expr(expr);
            }
            ExprTag::Conditional {
                cond,
                then,
                otherwise,
            } => {
                self.expand_expr(cond);
                self.expand_expr(then);
                self.expand_expr(otherwise);
            }
            _ => {} // Nothing to expand otherwise
        }
    }

    fn expand_stmt(&mut self, stmt: &mut Stmt) {
        match &mut stmt.tag {
            StmtTag::Expr(Some(expr)) => {
                self.expand_expr(expr);
            }
            StmtTag::Compound(vec) => {
                vec.iter_mut().for_each(|stmt| self.expand_stmt(stmt));
            }
            StmtTag::Decl(decl) => {
                self.expand_decl(decl);
            }
            StmtTag::While { cond, stmt } => {
                self.expand_expr(cond);
                self.expand_stmt(stmt);
            }
            StmtTag::Do { cond, stmt } => {
                self.expand_expr(cond);
                self.expand_stmt(stmt);
            }
            StmtTag::For(expr, expr1, expr2, stmt) => {
                [expr, expr1, expr2].iter_mut().for_each(|expr| {
                    if let Some(expr) = expr {
                        self.expand_expr(expr);
                    }
                });
            }
            StmtTag::If(expr, then, otherwise) => {
                self.expand_expr(expr);
                self.expand_stmt(then);
                if let Some(stmt) = otherwise {
                    self.expand_stmt(stmt);
                }
            }
            StmtTag::Switch(expr, stmt) => {
                self.expand_expr(expr);
                self.expand_stmt(stmt);
            }
            StmtTag::Case(expr, stmt) => {
                self.expand_expr(expr);
                self.expand_stmt(stmt);
            }
            StmtTag::Label(_, stmt) => {
                self.expand_stmt(stmt);
            }
            StmtTag::Default(stmt) => {
                self.expand_stmt(stmt);
            }
            _ => {}
        }
    }

    fn expand_invocation(&mut self, mac: &mut InvocationTag) -> () {
        match mac {
            InvocationTag::Bang(id, tt) => {
                let toks = Tokens(
                    tt.clone()
                        .map(|tt| TtCursor::new(&vec![tt]).collect())
                        .unwrap_or(Vec::new()),
                );
                let mut parser = Parser::new(&toks);
                println!("lookup for {}", id);
                let matchers: Vec<MacroMatcher> = self
                    .decls
                    .lookup(id)
                    .expect("use of undeclared macro")
                    .iter()
                    .map(|rule| rule.0.clone())
                    .collect();
                dbg!(match_macro(&matchers, &mut parser));
            }
        }
    }
}

fn match_macro<'a>(matchers: &'a [MacroMatcher], parser: &mut Parser) {
    let mut tt_matcher = TtMatcher::new();

    for (i, matcher) in matchers.iter().enumerate() {
        println!("Matcing arm {i}");
        dbg!(tt_matcher
            .match_tt(&StatesParser::generate_substates(&matcher.0), parser));
    }
}
