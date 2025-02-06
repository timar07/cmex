//! This file implements AST compiling to ANSI C language.

use std::io::{self, BufWriter, Write};

use cmex_ast::*;
use cmex_span::Spanned;

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
        )?
    }
}

macro_rules! emitln {
    ($self:expr, $($arg:tt)*) => {
        write!(
            $self.f,
            "{:>indent$}{}{}",
            "",
            format!($($arg)*),
            if $self.options.inline {
                ""
            } else {
                "\n"
            },
            indent = $self.indent
        ).unwrap()
    };
}

pub struct CompilerOptions {
    pub indent_width: usize,
    pub inline: bool,
}

impl Default for CompilerOptions {
    fn default() -> Self {
        Self {
            indent_width: 4,
            inline: false,
        }
    }
}

pub struct Compiler<'a, T: std::io::Write> {
    ast: &'a TranslationUnit,
    f: &'a mut BufWriter<T>,
    options: CompilerOptions,
    indent: usize,
}

impl<'a, T> Compiler<'a, T>
where
    T: std::io::Write,
{
    pub fn new(f: &'a mut BufWriter<T>, ast: &'a TranslationUnit) -> Self {
        Self {
            f,
            ast,
            indent: 0,
            options: CompilerOptions::default(),
        }
    }

    pub fn compile(&mut self) {
        for decl in &self.ast.0 {
            self.compile_decl(decl).unwrap();
        }
    }

    fn enter(&mut self) {
        self.indent += self.options.indent_width;
    }

    fn leave(&mut self) {
        self.indent -= self.options.indent_width;
    }

    fn compile_decl(&mut self, decl: &DeclTag) -> io::Result<()> {
        match decl {
            DeclTag::Include { path, .. } => {
                writeln!(self.f, "#include <{path}>")?;
            }
            DeclTag::Record(id, fields) => {
                emit!(self, "struct");

                if let Some(id) = id {
                    write!(self.f, " {}", id.0)?;
                }

                write!(self.f, " {{")?;
                self.emit_linebreak()?;

                with_indent!(self, {
                    for field in fields {
                        emit!(self, "{}", self.compile_field(field));
                        self.emit_linebreak()?;
                    }
                });

                emitln!(self, "}};");
                self.emit_linebreak()?;
            }
            DeclTag::Enum(id, consts) => {
                emit!(self, "enum");

                if let Some(id) = id {
                    write!(self.f, " {}", id.0)?;
                }

                write!(self.f, " {{")?;
                self.emit_linebreak()?;

                with_indent!(self, {
                    for cdecl in consts {
                        emit!(self, "{}", self.compile_constant(cdecl));
                        self.emit_linebreak()?;
                    }
                });

                emitln!(self, "}};");
                self.emit_linebreak()?;
            }
            DeclTag::Func { spec, decl, body } => {
                if let Some(spec) = spec {
                    emit!(self, "{} ", self.compile_specs(spec));
                }

                emit!(self, "{}", self.compile_declarator(decl));

                emitln!(self, "");

                self.compile_stmt(body)?;
                self.emit_linebreak()?;
            }
            DeclTag::Var { spec, decl_list } => {
                emit!(
                    self,
                    "{} {};",
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

        Ok(())
    }

    fn compile_stmt(&mut self, stmt: &Stmt) -> io::Result<()> {
        match &stmt.tag {
            StmtTag::Expr(expr) => {
                if let Some(expr) = expr {
                    if let Expr::StmtExpr(stmts, _) = &expr {
                        emitln!(self, "({{");
                        with_indent!(self, {
                            for stmt in stmts {
                                self.compile_stmt(stmt)?;
                            }
                        });
                        emit!(self, "}})");
                    } else {
                        emit!(self, "{}", self.compile_expr(expr));
                    }

                    write!(self.f, ";")?;
                    self.emit_linebreak()?;
                }
            }
            StmtTag::Compound(vec) => {
                emitln!(self, "{{");
                with_indent!(self, {
                    for stmt in vec {
                        self.compile_stmt(stmt)?;
                    }
                });
                emitln!(self, "}}");
            }
            StmtTag::Decl(decl) => {
                self.compile_decl(decl)?;
                write!(self.f, ";")?;
                self.emit_linebreak()?;
            }
            StmtTag::While { cond, stmt } => {
                emit!(self, "while ({})", self.compile_expr(cond));
                self.compile_stmt(stmt)?;
            }
            StmtTag::Do { cond, stmt } => {
                emit!(self, "do");
                self.compile_stmt(stmt)?;
                emit!(self, "while ({});", self.compile_expr(cond));
                self.emit_linebreak()?;
            }
            StmtTag::For(expr, expr1, expr2, stmt) => {
                emit!(
                    self,
                    "for ({})\n",
                    [expr, expr1, expr2]
                        .iter()
                        .map(|expr| {
                            (*expr)
                                .as_ref()
                                .map(|expr| self.compile_expr(expr))
                                .unwrap_or_default()
                        })
                        .collect::<Vec<_>>()
                        .join("; ")
                );

                self.compile_stmt(stmt)?;
            }
            StmtTag::If(cond, then, otherwise) => {
                emit!(self, "if ({})", self.compile_expr(cond));
                self.compile_stmt(then)?;
                if let Some(stmt) = otherwise {
                    self.compile_stmt(stmt)?;
                }
            }
            StmtTag::Switch(expr, stmt) => {
                emit!(self, "switch ({})", self.compile_expr(expr));
                self.compile_stmt(stmt)?;
            }
            StmtTag::Case(expr, stmt) => {
                emit!(self, "case {}:", self.compile_expr(expr));

                with_indent!(self, {
                    self.compile_stmt(stmt)?;
                });
            }
            StmtTag::Label(Spanned(label, _), stmt) => {
                writeln!(self.f, "{label}:")?;
                self.compile_stmt(stmt)?;
            }
            StmtTag::Default(stmt) => {
                emitln!(self, "default:");
                with_indent!(self, {
                    self.compile_stmt(stmt)?;
                });
            }
            StmtTag::Break(_) => {
                emitln!(self, "break;");
            }
            StmtTag::Continue(_) => {
                emitln!(self, "continue;");
            }
            StmtTag::Return(_, expr) => {
                emitln!(
                    self,
                    "return {};",
                    expr.as_ref()
                        .map(|expr| self.compile_expr(expr))
                        .unwrap_or_default()
                );
            }
            StmtTag::Goto(Spanned(label, _)) => {
                emit!(self, "goto {label};");
                self.emit_linebreak()?;
            }
        }

        Ok(())
    }

    fn compile_expr(&self, expr: &Expr) -> String {
        match &expr {
            Expr::Primary(Spanned(tok, _)) => {
                format!("{tok}")
            }
            Expr::Paren(expr) => {
                format!("({})", self.compile_expr(expr))
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
                format!("{}{}", op.0, self.compile_expr(rhs))
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
                format!("({}).{}", self.compile_expr(expr), member.0)
            }
            Expr::SizeofType { r#type } => {
                format!("sizeof({})", self.compile_type_name(r#type))
            }
            Expr::SizeofExpr { expr } => {
                format!("sizeof({})", self.compile_expr(expr))
            }
            Expr::CastExpr { r#type, expr } => {
                format!(
                    "({}) {}",
                    self.compile_type_name(r#type),
                    self.compile_expr(expr)
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
            Expr::StmtExpr(stmts, _) => {
                let mut buf = BufWriter::new(Vec::new());
                let mut compiler = Compiler::new(&mut buf, self.ast);
                compiler.options.inline = true;

                stmts
                    .iter()
                    .for_each(|stmt| compiler.compile_stmt(stmt).unwrap());

                format!(
                    "({{{}}})",
                    String::from_utf8(buf.into_inner().unwrap()).unwrap()
                )
            }
        }
    }

    fn compile_type_name(&self, r#type: &TypeName) -> String {
        format!(
            "{} {}",
            r#type
                .specs
                .iter()
                .map(|spec| self.compile_type_specifier(spec))
                .collect::<Vec<_>>()
                .join(" "),
            self.compile_declarator(&r#type.decl),
        )
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
            DirectDeclarator::Identifier(Spanned(tok, _)) => tok.to_string(),
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
            Initializer::List(list) => {
                format!(
                    "= {{{}}}",
                    list.iter()
                        .map(|item| match item {
                            Initializer::Assign(expr) => {
                                self.compile_expr(expr)
                            }
                            _ => unreachable!(),
                        })
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        }
    }

    fn compile_specs(&self, specs: &[DeclSpecifier]) -> String {
        specs
            .iter()
            .map(|s| self.compile_decl_spec(s))
            .collect::<Vec<String>>()
            .join(" ")
    }

    fn compile_decl_spec(&self, spec: &DeclSpecifier) -> String {
        match spec {
            DeclSpecifier::TypeSpecifier(spec) => {
                self.compile_type_specifier(spec)
            }
            DeclSpecifier::TypeQualifier(Spanned(tok, _)) => tok.to_string(),
            DeclSpecifier::StorageClass(Spanned(tok, _)) => tok.to_string(),
        }
    }

    fn compile_type_specifier(&self, spec: &TypeSpecifier) -> String {
        match spec {
            TypeSpecifier::TypeName(Spanned(tok, _)) => format!("{tok}"),
            TypeSpecifier::Record(id, vec) => {
                format!(
                    "struct {}{}",
                    id.as_ref().map(|id| id.0.to_string()).unwrap_or_default(),
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
            TypeSpecifier::Enum(id, vec) => {
                format!(
                    "enum {}{}",
                    id.as_ref().map(|id| id.0.to_string()).unwrap_or_default(),
                    if !vec.is_empty() {
                        format!(
                            "{{{}}}",
                            vec.iter()
                                .map(|const_decl| self
                                    .compile_enum_const(const_decl))
                                .collect::<Vec<String>>()
                                .join(" ")
                        )
                    } else {
                        "".into()
                    }
                )
            }
        }
    }

    fn compile_enum_const(&self, decl: &EnumConstantDecl) -> String {
        format!(
            "{}{}",
            decl.id.0,
            if let Some(cexpr) = &decl.cexpr {
                format!(" = {}", self.compile_expr(cexpr))
            } else {
                "".into()
            }
        )
    }

    fn compile_constant(&self, constant: &EnumConstantDecl) -> String {
        format!(
            "{}{}",
            constant.id.0,
            if let Some(expr) = &constant.cexpr {
                format!(" = {}", self.compile_expr(expr))
            } else {
                "".into()
            }
        )
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

    #[inline]
    fn emit_linebreak(&mut self) -> io::Result<()> {
        if !self.options.inline {
            writeln!(self.f)?;
        }

        Ok(())
    }
}
