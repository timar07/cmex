//! This file implements AST compiling to ANSI C language.

use std::{fs::File, io::{BufWriter, Write}};

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
        Self { indent_width: 4 }
    }
}

pub struct Compiler<'a> {
    ast: &'a TranslationUnit,
    f: &'a mut BufWriter<File>,
    options: CompilerOptions,
    indent: usize,
}

impl<'a> Compiler<'a> {
    pub fn new(f: &'a mut BufWriter<File>, ast: &'a TranslationUnit) -> Self {
        Self {
            f,
            ast,
            indent: 0,
            options: CompilerOptions::default(),
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

    fn compile_decl(&mut self, decl: &DeclTag) {
        match decl {
            DeclTag::Include { path, .. } => {
                writeln!(self.f, "#include <{path}>");
            }
            DeclTag::Record(id, fields) => {
                emit!(self, "struct");

                if let Some(id) = id {
                    write!(self.f, " {}", id.0.to_string());
                }

                write!(self.f, " {{\n");

                with_indent!(self, {
                    for field in fields {
                        emit!(self, "{}\n", self.compile_field(field));
                    }
                });

                emit!(self, "}};\n");
            }
            DeclTag::Enum(vec) => todo!(),
            DeclTag::Func { spec, decl, body } => {
                if let Some(spec) = spec {
                    emit!(self, "{} ", self.compile_specs(spec));
                }

                emit!(self, "{}", self.compile_declarator(decl));

                emit!(self, "\n");

                self.compile_stmt(body);
            }
            DeclTag::Var { spec, decl_list } => {
                emit!(
                    self,
                    "{} {}",
                    self.compile_specs(spec),
                    decl_list
                        .iter()
                        .map(|init_decl| self.compile_init_decl(init_decl))
                        .collect::<Vec<String>>()
                        .join(", ")
                );
            }
            DeclTag::Macro { .. } => { /* Macros are not compiled */ }
        }
    }

    fn compile_stmt(&mut self, stmt: &Stmt) {
        match &stmt.tag {
            StmtTag::Expr(expr) => {
                if let Some(expr) = expr {
                    if let Expr::StmtExpr(stmts, _) = &expr {
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
            }
            StmtTag::Compound(vec) => {
                emit!(self, "{{\n");
                with_indent!(self, {
                    for stmt in vec {
                        self.compile_stmt(stmt);
                    }
                });
                emit!(self, "}}\n");
            }
            StmtTag::Decl(decl) => {
                self.compile_decl(decl);
                write!(self.f, ";\n");
            }
            StmtTag::While { cond, stmt } => todo!(),
            StmtTag::Do { cond, stmt } => todo!(),
            StmtTag::For(expr, expr1, expr2, stmt) => todo!(),
            StmtTag::If(cond, then, otherwise) => {
                emit!(self, "if {}", self.compile_expr(cond));
                self.compile_stmt(then);
                if let Some(stmt) = otherwise {
                    self.compile_stmt(stmt);
                }
            }
            StmtTag::Switch(expr, stmt) => todo!(),
            StmtTag::Case(expr, stmt) => {
                emit!(self, "case {}:", self.compile_expr(expr));

                with_indent!(self, {
                    self.compile_stmt(stmt);
                });
            }
            StmtTag::Label((label, _), stmt) => {
                write!(self.f, "{label}:\n");
                self.compile_stmt(stmt);
            }
            StmtTag::Default(stmt) => {
                emit!(self, "default:\n");
                with_indent!(self, {
                    self.compile_stmt(stmt);
                });
            }
            StmtTag::Break => {
                emit!(self, "break;\n");
            }
            StmtTag::Continue => {
                emit!(self, "continue;\n");
            }
            StmtTag::Return(_, expr) => {
                emit!(
                    self,
                    "return {};\n",
                    expr.clone()
                        .map(|expr| self.compile_expr(&expr))
                        .unwrap_or_default()
                );
            }
            StmtTag::Goto((label, _)) => {
                emit!(self, "goto {label};\n");
            }
        }
    }

    fn compile_expr(&self, expr: &Expr) -> String {
        match &expr {
            Expr::Primary((tok, _)) => {
                format!("{tok}")
            }
            Expr::BinExpr { op, lhs, rhs } => {
                format!(
                    "{} {} {}",
                    self.compile_expr(lhs),
                    op.0,
                    self.compile_expr(rhs)
                )
            }
            Expr::UnExpr { op, rhs } => {
                format!("{} {}", op.0, self.compile_expr(rhs))
            }
            Expr::Call { calle, args } => {
                format!(
                    "{}({})",
                    if matches!(**calle, Expr::Primary(_)) {
                        self.compile_expr(calle)
                    } else {
                        format!("({})", self.compile_expr(calle))
                    },
                    args.iter()
                        .map(|arg| self.compile_expr(arg))
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            Expr::MemberAccess { expr, member } => {
                format!("({}).{}", self.compile_expr(&expr), member.0)
            }
            Expr::SizeofType { r#type } => {
                format!("sizeof({})", self.compile_declarator(&r#type.decl))
            }
            Expr::SizeofExpr { expr } => {
                format!("sizeof({})", self.compile_expr(&expr))
            }
            Expr::CastExpr { r#type, expr } => {
                format!(
                    "({}) {}",
                    self.compile_declarator(&r#type.decl),
                    self.compile_expr(&expr)
                )
            }
            Expr::Conditional {
                cond,
                then,
                otherwise,
            } => {
                format!(
                    "{} ? {} : {}",
                    self.compile_expr(cond),
                    self.compile_expr(then),
                    self.compile_expr(otherwise)
                )
            }
            Expr::Invocation(_) => {
                panic!("attempted to compile macro invocation")
            }
            Expr::StmtExpr(_, _) => unreachable!(),
        }
    }

    fn compile_declarator(&self, decl: &Declarator) -> String {
        format!(
            "{}{}{}",
            decl.prefix
                .iter()
                .map(|prefix| prefix.to_string())
                .collect::<Vec<String>>()
                .join(" "),
            self.compile_direct_decl(&decl.inner),
            decl.suffix
                .as_ref()
                .map(|suffix| self.compile_decl_suffix(suffix))
                .unwrap_or_default()
        )
    }

    fn compile_direct_decl(&self, decl: &DirectDeclarator) -> String {
        match decl {
            DirectDeclarator::Identifier((tok, _)) => tok.to_string(),
            DirectDeclarator::Paren(decl) => {
                format!("({})", self.compile_declarator(decl))
            }
            DirectDeclarator::Abstract => "".into(),
        }
    }

    fn compile_init_decl(&self, init_decl: &InitDeclarator) -> String {
        format!(
            "{} {}",
            self.compile_declarator(&init_decl.0),
            init_decl
                .1
                .as_ref()
                .map(|init| self.compile_initializer(init))
                .unwrap_or_default()
        )
    }

    fn compile_decl_suffix(&self, suffix: &DeclaratorSuffix) -> String {
        match suffix {
            DeclaratorSuffix::Array(expr) => {
                format!(
                    "[{}]",
                    expr.as_ref()
                        .map(|expr| self.compile_expr(expr))
                        .unwrap_or_default()
                )
            }
            DeclaratorSuffix::Func(param_list) => {
                format!(
                    "({})",
                    param_list
                        .as_ref()
                        .map(|list| self.compile_params(list))
                        .unwrap_or_default()
                )
            }
        }
    }

    fn compile_initializer(&self, init: &Initializer) -> String {
        match init {
            Initializer::Assign(expr) => {
                format!("= {}", self.compile_expr(expr))
            }
            Initializer::List(vec) => todo!(),
        }
    }

    fn compile_specs(&self, specs: &Vec<DeclSpecifier>) -> String {
        specs
            .iter()
            .map(|s| self.compile_decl_spec(s))
            .collect::<Vec<String>>()
            .join(" ")
    }

    fn compile_decl_spec(&self, spec: &DeclSpecifier) -> String {
        match spec {
            DeclSpecifier::TypeSpecifier(spec) => {
                self.compile_type_specifier(&spec)
            }
            DeclSpecifier::TypeQualifier((tok, _)) => tok.to_string(),
            DeclSpecifier::StorageClass((tok, _)) => tok.to_string(),
        }
    }

    fn compile_type_specifier(&self, spec: &TypeSpecifier) -> String {
        match spec {
            TypeSpecifier::TypeName((tok, _)) => format!("{tok}"),
            TypeSpecifier::Record(id, vec) => {
                format!(
                    "struct {}{}",
                    id.clone().map(|id| id.0.to_string()).unwrap_or_default(),
                    if !vec.is_empty() {
                        format!(
                            "{{{}}}",
                            vec.iter()
                                .map(|field| self.compile_field(field))
                                .collect::<Vec<String>>()
                                .join(" ")
                        )
                    } else {
                        String::from("")
                    }
                )
            }
            TypeSpecifier::Enum(vec) => todo!(),
        }
    }

    fn compile_field(&self, field: &FieldDecl) -> String {
        format!(
            "{} {} {};",
            field
                .specs
                .iter()
                .map(|spec| self.compile_type_specifier(spec))
                .collect::<Vec<_>>()
                .join(" "),
            self.compile_declarator(&field.decl.decl),
            if let Some(width) = &field.decl.width {
                format!(": {}", self.compile_expr(width))
            } else {
                String::from("")
            }
        )
    }

    fn compile_params(&self, params: &ParamList) -> String {
        match params {
            ParamList::Identifier(vec) => vec
                .iter()
                .map(|param| param.0.to_string())
                .collect::<Vec<String>>()
                .join(", "),
            ParamList::Type(vec) => vec
                .iter()
                .map(|param| {
                    format!(
                        "{} {}",
                        self.compile_specs(&param.spec),
                        self.compile_declarator(&param.decl)
                    )
                })
                .collect::<Vec<String>>()
                .join(", "),
        }
    }
}
