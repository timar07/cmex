use crate::lexer::{Token, TokenTag};

#[derive(Debug)]
pub struct Stmt {
    pub tag: StmtTag
}

#[derive(Debug)]
pub enum StmtTag {
    ExprStmt(Option<Expr>),
    CompoundStmt(Vec<Stmt>),
    DeclStmt(DeclStmt),
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

#[derive(Debug)]
pub enum DeclStmt {
    RecordDecl(Vec<FieldDecl>),
    EnumDecl(Vec<EnumConstantDecl>)
}

/// Field declaration in some record.
/// For example:
/// ```c
/// struct foo {
///     int bar; /* field */
///     /* ... */
/// }
/// ```
#[derive(Debug)]
pub struct FieldDecl {

}

/// Enum variant
#[derive(Debug)]
pub struct EnumConstantDecl {
    pub id: Token,
    /// enum variant can be a constant expression:
    /// ```c
    /// enum foo {
    ///     bar = 1,
    ///  /* ^~~~~~~ constant expression */
    /// }
    /// ```
    pub cexpr: Option<Expr>
}

#[derive(Debug)]
pub struct Expr {
    pub tag: ExprTag,
    // todo: spans and stuff
}

#[derive(Debug)]
pub enum ExprTag {
    Primary,
    BinExpr {
        op: TokenTag,
        lhs: Box<Expr>,
        rhs: Box<Expr>
    },
    UnExpr {
        op: TokenTag,
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
        member: TokenTag
    },
    SizeofType {
        /// sizeof (int)
        ///         ^~~ type
        r#type: TokenTag
    },
    SizeofExpr {
        expr: Box<Expr>
    },
    CastExpr {
        r#type: TokenTag,
        expr: Box<Expr>
    },
    Conditional {
        cond: Box<Expr>,
        then: Box<Expr>,
        otherwise: Box<Expr>
    }
}
