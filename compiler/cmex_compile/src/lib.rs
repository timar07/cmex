//! This file implements AST compiling to ANSI C language.

use std::{fs::File, io::Write};

use cmex_ast::*;

macro_rules! with_indent {
    ($self:expr, $stmt:expr) => {
        {
            $self.enter();
            $stmt
            $self.leave();
        }
    };
}

macro_rules! emit {
    ($self:expr, $($arg:tt)*) => {
        write!(
            $self.f,
            "{:>indent$}{}",
            "",
            format!($($arg)*),
            indent = $self.indent
        )
    }
}

pub struct CompilerOptions {
    pub indent_width: usize,
}

impl Default for CompilerOptions {
    fn default() -> Self {
        Self {
            indent_width: 4
        }
    }
}

pub struct Compiler<'a> {
    ast: &'a TranslationUnit,
    f: &'a mut File,
    options: CompilerOptions,
    indent: usize
}

impl<'a> Compiler<'a> {
    pub fn new(f: &'a mut File, ast: &'a TranslationUnit) -> Self {
        Self {
            f,
            ast,
            indent: 0,
            options: CompilerOptions::default()
        }
    }

    pub fn compile(&mut self) {
        for decl in &self.ast.0 {
            self.compile_decl(decl);
        }
    }

    fn enter(&mut self) {
        self.indent += self.options.indent_width;
    }

    fn leave(&mut self) {
        self.indent -= self.options.indent_width;
    }

    fn compile_decl(&mut self, decl: &Decl) {
        match decl {
            Decl::Record(vec) => todo!(),
            Decl::Enum(vec) => todo!(),
            Decl::Func { spec, decl, params, body } => {
                if let Some(spec) = spec {
                    emit!(self, "{} ", self.compile_specs(spec));
                }

                emit!(self, "{decl}");

                if let Some(params) = params {
                    emit!(self, "({params})");
                } else {
                    emit!(self, "()");
                }

                emit!(self, "\n");

                self.compile_stmt(body);
            },
            Decl::Var { spec, decl_list } => {
                emit!(self, "{}", self.compile_specs(spec));
            },
            Decl::Macro { .. } => { /* Macros are not compiled */ },
        }
    }

    fn compile_stmt(&mut self, stmt: &Stmt) {
        match &stmt.tag {
            StmtTag::Expr(expr) => {
                if let Some(expr) = expr {
                    if let ExprTag::StmtExpr(stmts, _) = &expr.tag {
                        emit!(self, "({{\n");
                        with_indent!(self, {
                            for stmt in stmts {
                                self.compile_stmt(stmt);
                            }
                        });
                        emit!(self, "}})");
                    } else {
                        emit!(self, "{}", self.compile_expr(expr));
                    }

                    write!(self.f, ";\n");
                }
            },
            StmtTag::Compound(vec) => {
                emit!(self, "{{\n");
                with_indent!(self, {
                    for stmt in vec {
                        self.compile_stmt(stmt);
                    }
                });
                emit!(self, "}}\n");
            },
            StmtTag::Decl(decl) => todo!(),
            StmtTag::While { cond, stmt } => todo!(),
            StmtTag::Do { cond, stmt } => todo!(),
            StmtTag::For(expr, expr1, expr2, stmt) => todo!(),
            StmtTag::If(expr, stmt, stmt1) => todo!(),
            StmtTag::Switch(expr, stmt) => todo!(),
            StmtTag::Case(expr, stmt) => todo!(),
            StmtTag::Label(_, stmt) => todo!(),
            StmtTag::Default(stmt) => todo!(),
            StmtTag::Break => todo!(),
            StmtTag::Continue => todo!(),
            StmtTag::Return(_, expr) => {
                emit!(
                    self,
                    "return {};\n",
                    expr
                        .clone()
                        .map(|expr| self.compile_expr(&expr))
                        .unwrap_or_default()
                );
            },
            StmtTag::Goto(_) => todo!(),
        }
    }

    fn compile_expr(&self, expr: &Expr) -> String {
        match &expr.tag {
            ExprTag::Primary((tok, _)) => {
                format!("{tok}")
            },
            ExprTag::BinExpr { op, lhs, rhs } => {
                format!(
                    "{} {} {}",
                    self.compile_expr(lhs),
                    op.0,
                    self.compile_expr(rhs)
                )
            }
            ExprTag::UnExpr { op, rhs } => todo!(),
            ExprTag::Call { calle, args } => {
                format!(
                    "({})({})",
                    self.compile_expr(calle),
                    args
                        .iter()
                        .map(|arg| self.compile_expr(arg))
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            },
            ExprTag::MemberAccess { expr, member } => todo!(),
            ExprTag::SizeofType { r#type } => todo!(),
            ExprTag::SizeofExpr { expr } => todo!(),
            ExprTag::CastExpr { r#type, expr } => todo!(),
            ExprTag::Conditional { cond, then, otherwise } => todo!(),
            ExprTag::Invocation(_) => panic!("attempted to compile macro invocation"),
            ExprTag::StmtExpr(vec, span) => unreachable!(),
        }
    }

    fn compile_specs(&self, specs: &Vec<DeclSpecifier>) -> String {
        specs
            .iter()
            .map(|s| s.to_string())
            .collect::<Vec<String>>()
            .join(" ")
    }
}
