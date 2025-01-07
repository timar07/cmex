pub mod ast_dump;
mod tree_builder;

use ast_dump::AstNodeDump;
use cmex_lexer::Token;
use cmex_span::{MaybeSpannable, Span, Spannable};
use tree_builder::TreeBuilder;

pub struct TranslationUnit(pub Vec<Vec<Decl>>);

impl AstNodeDump for TranslationUnit {
    fn dump(&self, tb: &mut TreeBuilder) {
        tb.open("TranslationUnit".into());

        for decls in self.0.clone() {
            decls.iter().for_each(|decl| decl.dump(tb));
        }

        tb.close();
    }
}

#[derive(Debug, Clone)]
pub struct Stmt {
    pub tag: StmtTag,
}

impl Spannable for Stmt {
    fn span(&self) -> Span {
        self.tag.span()
    }
}

#[derive(Debug, Clone)]
pub enum StmtTag {
    Expr(Option<Expr>),
    Compound(Vec<Stmt>),
    Decl(Decl),
    /// while (cond) stmt
    While {
        cond: Expr,
        stmt: Box<Stmt>,
    },
    /// do stmt while (cond);
    Do {
        cond: Expr,
        stmt: Box<Stmt>,
    },
    /// for (expr, expr, expr) stmt
    For(Option<Expr>, Option<Expr>, Option<Expr>, Box<Stmt>),
    /// if (expr) stmt else stmt
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    /// switch (expr) stmt
    Switch(Expr, Box<Stmt>),
    /// case expr: stmt
    Case(Expr, Box<Stmt>),
    /// id: stmt
    Label(Token, Box<Stmt>),
    /// default: stmt
    Default(Box<Stmt>),
    Break,
    Continue,
    Return,
    Goto(Token),
}

impl Spannable for StmtTag {
    fn span(&self) -> Span {
        match self {
            Self::Expr(expr) => expr.clone().unwrap().span(),
            Self::Compound(vec) => vec.span().unwrap(),
            Self::Decl(decl) => decl.span(),
            Self::While { cond: _, stmt } => stmt.tag.span(),
            Self::Do { cond: _, stmt } => stmt.tag.span(),
            Self::For(_, _, _, stmt) => stmt.tag.span(),
            Self::If(_, then, otherwise) => {
                let span = then.tag.span();

                if let Some(stmt) = otherwise {
                    Span::join(span, stmt.tag.span())
                } else {
                    span
                }
            }
            Self::Switch(_, stmt) => stmt.tag.span(),
            Self::Case(_, stmt) => stmt.tag.span(),
            Self::Label(_, stmt) => stmt.tag.span(),
            Self::Default(stmt) => stmt.tag.span(),
            Self::Break => todo!(),
            Self::Continue => todo!(),
            Self::Return => todo!(),
            Self::Goto(_) => todo!(),
        }
    }
}

impl AstNodeDump for StmtTag {
    fn dump(&self, tb: &mut TreeBuilder) {
        match self {
            StmtTag::Expr(expr) => {
                tb.open("ExprStmt".into());

                if let Some(expr) = expr {
                    expr.dump(tb);
                }

                tb.close();
            }
            StmtTag::Compound(stmts) => {
                tb.open("CompoundStmt".into());
                stmts.iter().for_each(|stmt| stmt.tag.dump(tb));
                tb.close();
            }
            StmtTag::Decl(_) => {
                tb.append_leaf("DeclStmt".into());
            }
            StmtTag::While { cond, stmt } => {
                tb.open("WhileStmt".into());
                cond.dump(tb);
                stmt.tag.dump(tb);
                tb.close();
            }
            StmtTag::Do { cond, stmt } => {
                tb.open("DoStmt".into());
                cond.dump(tb);
                stmt.tag.dump(tb);
                tb.close();
            }
            StmtTag::For(expr, expr1, expr2, stmt) => {
                tb.open("ForStmt".into());

                for expr in [expr, expr1, expr2].iter().copied().flatten() {
                    expr.dump(tb);
                }

                stmt.tag.dump(tb);
                tb.close();
            }
            StmtTag::If(expr, if_true, otherwise) => {
                tb.open("IfStmt".into());
                expr.dump(tb);
                if_true.tag.dump(tb);

                if let Some(stmt) = otherwise {
                    stmt.tag.dump(tb);
                }

                tb.close();
            }
            StmtTag::Switch(expr, stmt) => {
                tb.open("SwitchStmt".into());
                expr.dump(tb);
                stmt.tag.dump(tb);
                tb.close();
            }
            StmtTag::Case(expr, stmt) => {
                tb.open("CaseStmt".into());
                expr.dump(tb);
                stmt.tag.dump(tb);
                tb.close();
            }
            StmtTag::Label(_, stmt) => {
                tb.open("LabelStmt".into());
                stmt.tag.dump(tb);
                tb.close();
            }
            StmtTag::Default(stmt) => {
                tb.open("DefaultStmt".into());
                stmt.tag.dump(tb);
                tb.close();
            }
            StmtTag::Break => {
                tb.append_leaf("BreakStmt".into());
            }
            StmtTag::Continue => {
                tb.append_leaf("ContinueStmt".into());
            }
            StmtTag::Return => {
                tb.append_leaf("ReturnStmt".into());
            }
            StmtTag::Goto(_) => todo!(),
        };
    }
}

#[derive(Debug, Clone)]
pub enum Decl {
    Record(Vec<FieldDecl>),
    Enum(Vec<EnumConstantDecl>),
    Func {
        /// ```c
        ///    static int foo() { /* ... */ }
        /// /* ^~~~~~~~~~ spec */
        /// ```
        spec: Option<Vec<DeclSpecifier>>,
        /// ```c
        /// int foo() { /* ... */ }
        ///  /* ^~~~~ decl */
        /// ```
        decl: Box<DirectDeclarator>,
        ///```c
        /// int foo() { /* ... */ }
        ///     /* ^~ params */
        /// ```
        params: Option<ParamList>,
        ///```c
        /// int foo() { /* ... */ }
        ///        /* ^~~~~~~~ body */
        /// ```
        body: Box<Stmt>,
    },
    Var {
        /// ```c
        ///    register int a, b, c
        /// /* ^~~~~~~~~~~~ spec */
        /// ```
        spec: Vec<DeclSpecifier>,
        /// ```c
        /// int a, b, c
        ///  /* ^~~~~~~ declarators list */
        /// ```
        decl_list: Vec<InitDeclarator>,
    },
    Macro {},
}

impl Spannable for Decl {
    fn span(&self) -> Span {
        match self {
            Decl::Record(vec) => vec.span().unwrap(),
            Decl::Enum(vec) => vec.span().unwrap(),
            Decl::Func {
                spec,
                decl,
                params: _,
                body,
            } => Span::join(
                spec.clone()
                    .map(|specs| specs.span().unwrap())
                    .unwrap_or_else(|| decl.span().unwrap()),
                body.tag.span(),
            ),
            Decl::Var { spec: _, decl_list } => decl_list.span().unwrap(),
            Decl::Macro {} => todo!(),
        }
    }
}

impl AstNodeDump for Decl {
    fn dump(&self, tb: &mut TreeBuilder) {
        match self {
            Decl::Record(decls) => {
                tb.open("RecordDecl".into());
                decls.iter().for_each(|decl| decl.dump(tb));
                tb.close();
            }
            Decl::Enum(decls) => {
                tb.open("EnumDecl".into());
                decls.iter().for_each(|d| d.dump(tb));
                tb.close();
            }
            Decl::Func {
                spec: _,
                decl: _,
                body,
                params,
            } => {
                tb.open("FuncDecl".into());

                if let Some(params) = params {
                    params.dump(tb);
                }

                body.tag.dump(tb);
                tb.close();
            }
            Decl::Var { spec: _, decl_list } => {
                decl_list.iter().for_each(|decl| {
                    tb.open("VarDecl".into());
                    decl.dump(tb);
                    tb.close();
                });
            }
            Decl::Macro {} => {
                tb.append_leaf("Macro".into());
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum DeclSpecifier {
    TypeSpecifier(TypeSpecifier),
    TypeQualifier(Token),
    StorageClass(Token),
}

impl Spannable for DeclSpecifier {
    fn span(&self) -> Span {
        match self {
            DeclSpecifier::TypeSpecifier(spec) => spec.span(),
            DeclSpecifier::TypeQualifier((_, span)) => *span,
            DeclSpecifier::StorageClass((_, span)) => *span,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Initializer {
    Assign(Expr),
    List(Vec<Initializer>),
}

impl Spannable for Initializer {
    fn span(&self) -> Span {
        match self {
            Self::Assign(expr) => expr.span(),
            Self::List(list) => list.span().unwrap(),
        }
    }
}

impl AstNodeDump for Initializer {
    fn dump(&self, tb: &mut TreeBuilder) {
        match self {
            Self::Assign(expr) => expr.dump(tb),
            Self::List(init_list) => {
                init_list.iter().for_each(|init| init.dump(tb));
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct InitDeclarator(pub Declarator, pub Option<Initializer>);

impl Spannable for InitDeclarator {
    fn span(&self) -> Span {
        Span::join(
            self.0.span(),
            self.1
                .clone()
                .map(|init| init.span())
                .unwrap_or(self.0.span()),
        )
    }
}

impl AstNodeDump for InitDeclarator {
    fn dump(&self, tb: &mut TreeBuilder) {
        if let Some(init) = &self.1 {
            init.dump(tb);
        }
    }
}

#[derive(Debug, Clone)]
pub struct Declarator {
    pub inner: Box<DirectDeclarator>,
    pub suffix: Option<DeclaratorSuffix>,
}

impl std::fmt::Display for Declarator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}",
            self.inner,
            self.suffix
                .as_ref()
                .map(|suffix| format!("{}", suffix))
                .unwrap_or_default()
        )
    }
}

impl Spannable for Declarator {
    fn span(&self) -> Span {
        let inner_span = self.inner.span().unwrap();

        Span::join(
            inner_span,
            self.suffix
                .clone()
                .map(|suffix| suffix.span().unwrap_or(inner_span))
                .unwrap_or(inner_span),
        )
    }
}

#[derive(Debug, Clone)]
pub enum DirectDeclarator {
    Identifier(Token),
    Paren(Declarator),
    Abstract,
}

impl DirectDeclarator {
    pub fn is_abstract(&self) -> bool {
        match self {
            Self::Abstract => true,
            Self::Identifier(_) => false,
            Self::Paren(decl) => decl.inner.is_abstract(),
        }
    }
}

impl MaybeSpannable for DirectDeclarator {
    fn span(&self) -> Option<Span> {
        match self {
            DirectDeclarator::Identifier((_, span)) => Some(*span),
            DirectDeclarator::Paren(decl) => Some(decl.span()),
            DirectDeclarator::Abstract => None,
        }
    }
}

impl std::fmt::Display for DirectDeclarator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Identifier((tok, _)) => write!(f, "{:?}", tok),
            Self::Paren(decl) => write!(f, "{}", decl),
            Self::Abstract => write!(f, ""),
        }
    }
}

#[derive(Debug, Clone)]
pub enum DeclaratorSuffix {
    /// ```c
    /// int *c[123];
    /// /*    ^~~~~ array declarator suffix */
    /// ```
    Array(Option<Expr>),
    /// ```c
    /// int *(func)(int a);
    ///         /* ^~~~~~~ function declarator suffix */
    /// ```
    Func(Option<ParamList>),
}

impl std::fmt::Display for DeclaratorSuffix {
    fn fmt(&self, _: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DeclaratorSuffix::Array(_) => todo!(),
            DeclaratorSuffix::Func(_) => todo!(),
        }
    }
}

impl MaybeSpannable for DeclaratorSuffix {
    fn span(&self) -> Option<Span> {
        match self {
            DeclaratorSuffix::Array(expr) => expr.clone().map(|e| e.span()),
            DeclaratorSuffix::Func(param_list) => {
                param_list.clone().map(|list| list.span())
            }
        }
    }
}

/// Field declaration in some record.
/// For example:
/// ```c
/// struct foo {
///     int bar; /* field */
///     /* ... */
/// }
/// ```
#[derive(Debug, Clone)]
pub struct FieldDecl {
    pub decl: FieldDeclarator,
}

impl Spannable for FieldDecl {
    fn span(&self) -> Span {
        self.decl.span()
    }
}

impl AstNodeDump for FieldDecl {
    fn dump(&self, tb: &mut TreeBuilder) {
        tb.append_leaf("FieldDecl".into());
    }
}

#[derive(Debug, Clone)]
pub struct FieldDeclarator {
    pub decl: Declarator,
    /// A bit field width, in ANSI C you can write:
    /// ```c
    /// struct foo {
    ///     unsigned a: 5;
    ///              /* ^ width */
    /// }
    /// ```
    pub width: Option<Expr>,
}

impl Spannable for FieldDeclarator {
    fn span(&self) -> Span {
        let decl_span = self.decl.span();

        Span::join(
            decl_span,
            self.width
                .clone()
                .map(|expr| expr.span())
                .unwrap_or(decl_span),
        )
    }
}

/// Enum variant
#[derive(Debug, Clone)]
pub struct EnumConstantDecl {
    pub id: Token,
    /// `enum` variant can be initialized with a constant expression:
    /// ```c
    /// enum foo {
    ///     bar = 1,
    ///        /* ^ constant expression */
    /// }
    /// ```
    pub cexpr: Option<Expr>,
}

impl Spannable for EnumConstantDecl {
    fn span(&self) -> Span {
        Span::join(
            self.id.1,
            self.cexpr
                .clone()
                .map(|expr| expr.span())
                .unwrap_or(self.id.1),
        )
    }
}

impl AstNodeDump for EnumConstantDecl {
    fn dump(&self, tb: &mut TreeBuilder) {
        tb.open("EnumConstantDecl".into());

        if let Some(expr) = self.cexpr.clone() {
            expr.dump(tb);
        }

        tb.close();
    }
}

#[derive(Debug, Clone)]
pub enum ParamList {
    /// In ANSI C it's possible to describe parameters
    /// as a list of identifiers (K&R style)
    /// ```c
    /// int foo(bar, baz) { /* ... */ }
    /// ```
    Identifier(Vec<Token>),
    /// Typed parameters list
    Type(Vec<ParamDecl>),
}

impl Spannable for ParamList {
    fn span(&self) -> Span {
        match self {
            ParamList::Identifier(vec) => {
                vec.iter().map(|id| id.1).reduce(Span::join).unwrap()
            }
            ParamList::Type(vec) => vec.span().unwrap(),
        }
    }
}

impl AstNodeDump for ParamList {
    fn dump(&self, tb: &mut TreeBuilder) {
        match self {
            Self::Identifier(_) => todo!(),
            Self::Type(decls) => {
                decls.iter().for_each(|decl| decl.dump(tb));
            }
        }
    }
}

/// Parameter declaration
#[derive(Debug, Clone)]
pub struct ParamDecl {
    pub spec: Vec<DeclSpecifier>,
    pub decl: Box<Declarator>,
}

impl Spannable for ParamDecl {
    fn span(&self) -> Span {
        let decl_span = self.decl.span();

        Span::join(self.spec.span().unwrap_or(decl_span), decl_span)
    }
}

impl AstNodeDump for ParamDecl {
    fn dump(&self, tb: &mut TreeBuilder) {
        tb.append_leaf("ParamDecl".into());
    }
}

#[derive(Debug, Clone)]
pub enum TypeSpecifier {
    /// Type specifier that refers to a declared type name
    /// ```c
    ///     int foo = 1;
    ///  /* ^~~ type name  */
    /// ```
    TypeName(Token),
    /// Type specifier that declares the type itself
    /// ```c
    ///     struct { int a; } foo = { 1 };
    ///  /* ^~~~~~~~~~~~~~~~~ definition */
    /// ```
    Record(Vec<FieldDecl>),
    Enum(Vec<EnumConstantDecl>),
}

impl Spannable for TypeSpecifier {
    fn span(&self) -> Span {
        match self {
            TypeSpecifier::TypeName((_, span)) => *span,
            TypeSpecifier::Record(vec) => vec.span().unwrap(),
            TypeSpecifier::Enum(vec) => vec.span().unwrap(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeName {
    /// Type specifiers
    /// ```c
    /// int foo(volatile int*)
    /// /*      ^~~~~~~~ specifier */
    /// ```
    /// An abstract declarator of type
    /// ```c
    ///   (int[]*) malloc(sizeof(int*));
    /// /* ^~~~~~ abstract declarator */
    /// ```
    pub decl: Declarator,
}

impl Spannable for TypeName {
    fn span(&self) -> Span {
        self.decl.span()
    }
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub tag: ExprTag,
}

impl Spannable for Expr {
    fn span(&self) -> Span {
        self.tag.span()
    }
}

impl AstNodeDump for Expr {
    fn dump(&self, tb: &mut TreeBuilder) {
        self.tag.dump(tb);
    }
}

#[derive(Debug, Clone)]
pub enum ExprTag {
    Primary(Token),
    BinExpr {
        op: Token,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    UnExpr {
        op: Token,
        rhs: Box<Expr>,
    },
    Call {
        /// foo(5, bar)
        /// ^~~ call expression
        calle: Box<Expr>,
        /// foo(5, bar)
        ///    ^~~~~~~~ args
        args: Vec<Expr>,
    },
    MemberAccess {
        /// mystruct.member
        /// ^~~~~~~~ expression
        expr: Box<Expr>,
        /// mystruct.member
        ///          ^~~~~~ member that being accessed
        member: Token,
    },
    SizeofType {
        /// sizeof (int)
        ///         ^~~ type
        r#type: Box<TypeName>,
    },
    SizeofExpr {
        expr: Box<Expr>,
    },
    CastExpr {
        r#type: Box<TypeName>,
        expr: Box<Expr>,
    },
    Conditional {
        cond: Box<Expr>,
        then: Box<Expr>,
        otherwise: Box<Expr>,
    },
    Invocation,
}

impl Spannable for ExprTag {
    fn span(&self) -> Span {
        match self {
            Self::Primary(tok) => tok.1,
            Self::BinExpr { op: _, lhs, rhs } => {
                Span::join(lhs.span(), rhs.span())
            }
            Self::UnExpr { op, rhs } => Span::join(op.1, rhs.span()),
            Self::Call { calle, args } => {
                let calle_span = calle.span();

                Span::join(calle_span, args.span().unwrap_or(calle_span))
            }
            Self::MemberAccess { expr, member } => {
                Span::join(expr.span(), member.1)
            }
            Self::SizeofType { r#type } => r#type.span(),
            Self::SizeofExpr { expr } => expr.span(),
            Self::CastExpr { r#type: _, expr } => expr.span(),
            Self::Conditional {
                cond,
                then: _,
                otherwise,
            } => Span::join(cond.span(), otherwise.span()),
            Self::Invocation => todo!(),
        }
    }
}

impl AstNodeDump for ExprTag {
    fn dump(&self, tb: &mut TreeBuilder) {
        match self {
            ExprTag::Primary(_) => {
                tb.append_leaf("PrimaryExpr".into());
            }
            ExprTag::BinExpr { op, lhs, rhs } => {
                tb.open(format!("BinaryOperator `{}`", op.0));
                lhs.dump(tb);
                rhs.dump(tb);
                tb.close();
            }
            ExprTag::UnExpr { op, rhs } => {
                tb.open(format!("UnaryOperator `{}`", op.0));
                rhs.dump(tb);
                tb.close();
            }
            ExprTag::Call { calle: _, args } => {
                tb.open("CallExpr".into());
                args.iter().for_each(|arg| arg.dump(tb));
                tb.close();
            }
            ExprTag::MemberAccess { expr: _, member: _ } => {
                tb.append_leaf("MemberExpr".into());
            }
            ExprTag::SizeofType { r#type: _ } => {
                tb.append_leaf("SizeofType".into());
            }
            ExprTag::SizeofExpr { expr } => {
                tb.open("SizeofExpr".into());
                expr.dump(tb);
                tb.close();
            }
            ExprTag::CastExpr { r#type: _, expr } => {
                tb.open("CastExpr".into());
                expr.dump(tb);
                tb.close();
            }
            ExprTag::Conditional {
                cond,
                then,
                otherwise,
            } => {
                tb.open("ConditionalOperator".into());
                cond.dump(tb);
                then.dump(tb);
                otherwise.dump(tb);
                tb.close();
            }
            ExprTag::Invocation => {
                tb.append_leaf("Invocation".into());
            }
        }
    }
}
