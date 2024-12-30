pub mod ast_dump;

use ast_dump::{AstNodeDump, TreeBuilder};
use crate::lexer::Token;

pub struct TranslationUnit(pub Vec<Vec<Decl>>);

impl AstNodeDump for TranslationUnit {
    fn dump(&self, tb: &mut TreeBuilder) {
        tb.open("TranslationUnit".into());

        for decls in self.0.clone() {
            decls.iter().for_each(|decl| {
                decl.dump(tb)
            });
        }

        tb.close();
    }
}

#[derive(Debug, Clone)]
pub struct Stmt {
    pub tag: StmtTag
}

#[derive(Debug, Clone)]
pub enum StmtTag {
    ExprStmt(Option<Expr>),
    CompoundStmt(Vec<Stmt>),
    DeclStmt(Decl),
    /// while (cond) stmt
    WhileStmt {
        cond: Expr,
        stmt: Box<Stmt>
    },
    /// do stmt while (cond);
    DoStmt {
        cond: Expr,
        stmt: Box<Stmt>
    },
    /// for (expr, expr, expr) stmt
    ForStmt(Option<Expr>, Option<Expr>, Option<Expr>, Box<Stmt>),
    /// if (expr) stmt else stmt
    IfStmt(Expr, Box<Stmt>, Option<Box<Stmt>>),
    /// switch (expr) stmt
    SwitchStmt(Expr, Box<Stmt>),
    /// case expr: stmt
    CaseStmt(Expr, Box<Stmt>),
    /// id: stmt
    LabelStmt(Token, Box<Stmt>),
    /// default: stmt
    DefaultStmt(Box<Stmt>),
    BreakStmt,
    ContinueStmt,
    ReturnStmt,
    GotoStmt(Token)
}

impl AstNodeDump for StmtTag {
    fn dump(&self, tb: &mut TreeBuilder) -> () {
        match self {
            StmtTag::ExprStmt(expr) => {
                tb.open("ExprStmt".into());

                if let Some(expr) = expr {
                    expr.dump(tb);
                }

                tb.close();
            },
            StmtTag::CompoundStmt(stmts) => {
                tb.open("CompoundStmt".into());
                stmts.iter().for_each(|stmt| stmt.tag.dump(tb));
                tb.close();
            },
            StmtTag::DeclStmt(decl) => {
                tb.append_leaf("DeclStmt".into());
            },
            StmtTag::WhileStmt { cond, stmt } => {
                tb.open("WhileStmt".into());
                cond.dump(tb);
                stmt.tag.dump(tb);
                tb.close();
            },
            StmtTag::DoStmt { cond, stmt } => {
                tb.open("DoStmt".into());
                cond.dump(tb);
                stmt.tag.dump(tb);
                tb.close();
            },
            StmtTag::ForStmt(expr, expr1, expr2, stmt) => {
                tb.open("ForStmt".into());

                for expr in [expr, expr1, expr2].iter() {
                    if let Some(expr) = expr {
                        expr.dump(tb);
                    }
                }

                stmt.tag.dump(tb);
                tb.close();
            },
            StmtTag::IfStmt(expr, if_true, otherwise) => {
                tb.open("IfStmt".into());
                expr.dump(tb);
                if_true.tag.dump(tb);

                if let Some(stmt) = otherwise {
                    stmt.tag.dump(tb);
                }

                tb.close();
            },
            StmtTag::SwitchStmt(expr, stmt) => {
                tb.open("SwitchStmt".into());
                expr.dump(tb);
                stmt.tag.dump(tb);
                tb.close();
            },
            StmtTag::CaseStmt(expr, stmt) => {
                tb.open("CaseStmt".into());
                expr.dump(tb);
                stmt.tag.dump(tb);
                tb.close();
            },
            StmtTag::LabelStmt(_, stmt) => {
                tb.open("LabelStmt".into());
                stmt.tag.dump(tb);
                tb.close();
            },
            StmtTag::DefaultStmt(stmt) => {
                tb.open("DefaultStmt".into());
                stmt.tag.dump(tb);
                tb.close();
            },
            StmtTag::BreakStmt => {
                tb.append_leaf("BreakStmt".into());
            }
            StmtTag::ContinueStmt => {
                tb.append_leaf("ContinueStmt".into());
            },
            StmtTag::ReturnStmt => {
                tb.append_leaf("ReturnStmt".into());
            },
            StmtTag::GotoStmt(_) => todo!(),
        };
    }
}

#[derive(Debug, Clone)]
pub enum Decl {
    RecordDecl(Vec<FieldDecl>),
    EnumDecl(Vec<EnumConstantDecl>),
    FuncDecl {
        spec: Option<Vec<DeclSpecifier>>,
        decl: Box<DirectDeclarator>,
        params: Option<ParamList>,
        body: Box<Stmt>
    },
    VarDecl {
        /// ```c
        ///    register int a, b, c
        /// /* ^~~~~~~~~~~~ spec */
        /// ```
        spec: Vec<DeclSpecifier>,
        /// ```c
        /// int a, b, c
        ///  /* ^~~~~~~ declarators list */
        /// ```
        decl_list: Vec<InitDeclarator>
    }
}

impl AstNodeDump for Decl {
    fn dump(&self, tb: &mut TreeBuilder) -> () {
        match self {
            Decl::RecordDecl(decls) => {
                tb.open("RecordDecl".into());
                decls
                    .iter()
                    .for_each(|decl| decl.dump(tb));
                tb.close();
            },
            Decl::EnumDecl(decls) => {
                tb.open("EnumDecl".into());
                decls.iter().for_each(|d| d.dump(tb));
                tb.close();
            },
            Decl::FuncDecl { spec, decl, body, params } => {
                tb.open("FuncDecl".into());

                if let Some(params) = params {
                    params.dump(tb);
                }

                body.tag.dump(tb);
                tb.close();
            },
            Decl::VarDecl { spec, decl_list } => {
                decl_list.iter().for_each(|decl| {
                    tb.open("VarDecl".into());
                    decl.dump(tb);
                    tb.close();
                });
            },
        }
    }
}

#[derive(Debug, Clone)]
pub enum DeclSpecifier {
    TypeSpecifier(TypeSpecifier),
    TypeQualifier(Token),
    StorageClass(Token)
}

#[derive(Debug, Clone)]
pub enum Initializer {
    Assign(Expr),
    List(Vec<Initializer>)
}

impl AstNodeDump for Initializer {
    fn dump(&self, tb: &mut TreeBuilder) -> () {
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

impl AstNodeDump for InitDeclarator {
    fn dump(&self, tb: &mut TreeBuilder) -> () {
        if let Some(init) = &self.1 {
            init.dump(tb);
        }
    }
}

#[derive(Debug, Clone)]
pub struct Declarator {
    pub inner: Box<DirectDeclarator>,
    pub suffix: Option<DeclaratorSuffix>
}

#[derive(Debug, Clone)]
pub enum DirectDeclarator {
    Identifier(Token),
    Paren(Declarator),
    Abstract
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

impl std::fmt::Display for DirectDeclarator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Identifier((tok, _)) => write!(f, "{:?}", tok),
            Self::Paren(declarator) => todo!(),
            Self::Abstract => write!(f, "")
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
    Func(Option<ParamList>)
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
    pub decl: FieldDeclarator
}

impl AstNodeDump for FieldDecl {
    fn dump(&self, tb: &mut TreeBuilder) -> () {
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
    pub width: Option<Expr>
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
    pub cexpr: Option<Expr>
}

impl AstNodeDump for EnumConstantDecl {
    fn dump(&self, tb: &mut TreeBuilder) -> () {
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
    Type(Vec<ParamDecl>)
}

impl AstNodeDump for ParamList {
    fn dump(&self, tb: &mut TreeBuilder) -> () {
        match self {
            Self::Identifier(_) => todo!(),
            Self::Type(decls) => {
                decls
                    .iter()
                    .for_each(|decl| decl.dump(tb));
            }
        }
    }
}

/// Parameter declaration
#[derive(Debug, Clone)]
pub struct ParamDecl {
    pub spec: Vec<DeclSpecifier>,
    pub decl: Box<Declarator>
}

impl AstNodeDump for ParamDecl {
    fn dump(&self, tb: &mut TreeBuilder) -> () {
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
    Enum(Vec<EnumConstantDecl>)
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
    pub decl: Declarator
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub tag: ExprTag,
    // todo: spans and stuff
}

impl AstNodeDump for Expr {
    fn dump(&self, tb: &mut TreeBuilder) -> () {
        self.tag.dump(tb);
    }
}

#[derive(Debug, Clone)]
pub enum ExprTag {
    Primary(Token),
    BinExpr {
        op: Token,
        lhs: Box<Expr>,
        rhs: Box<Expr>
    },
    UnExpr {
        op: Token,
        rhs: Box<Expr>
    },
    Call {
        /// foo(5, bar)
        /// ^~~ call expression
        calle: Box<Expr>,
        /// foo(5, bar)
        ///    ^~~~~~~~ args
        args: Vec<Expr>
    },
    MemberAccess {
        /// mystruct.member
        /// ^~~~~~~~ expression
        expr: Box<Expr>,
        /// mystruct.member
        ///          ^~~~~~ member that being accessed
        member: Token
    },
    SizeofType {
        /// sizeof (int)
        ///         ^~~ type
        r#type: Box<TypeName>
    },
    SizeofExpr {
        expr: Box<Expr>
    },
    CastExpr {
        r#type: Box<TypeName>,
        expr: Box<Expr>
    },
    Conditional {
        cond: Box<Expr>,
        then: Box<Expr>,
        otherwise: Box<Expr>
    }
}

impl AstNodeDump for ExprTag {
    fn dump(&self, tb: &mut TreeBuilder) -> () {
        match self {
            ExprTag::Primary(_) => {
                tb.append_leaf("PrimaryExpr".into());
            },
            ExprTag::BinExpr { op, lhs, rhs } => {
                tb.append_leaf(format!("BinaryOperator `{:?}`", op.0));
            },
            ExprTag::UnExpr { op, rhs } => {
                tb.append_leaf(format!("UnaryOperator `{:?}`", op.0));
            },
            ExprTag::Call { calle, args } => {
                tb.open("CallExpr".into());
                args.iter().for_each(|arg| arg.dump(tb));
                tb.close();
            },
            ExprTag::MemberAccess { expr, member } => {
                tb.append_leaf("MemberExpr".into());
            },
            ExprTag::SizeofType { r#type } => {
                tb.append_leaf("SizeofType".into());
            },
            ExprTag::SizeofExpr { expr } => {
                tb.open("SizeofExpr".into());
                expr.dump(tb);
                tb.close();
            },
            ExprTag::CastExpr { r#type, expr } => {
                tb.open("CastExpr".into());
                expr.dump(tb);
                tb.close();
            },
            ExprTag::Conditional { cond, then, otherwise } => {
                tb.open("ConditionalOperator".into());
                cond.dump(tb);
                then.dump(tb);
                otherwise.dump(tb);
                tb.close();
            },
        }
    }
}
