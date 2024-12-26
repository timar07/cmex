use crate::lexer::Token;

#[derive(Debug)]
pub struct Stmt {
    pub tag: StmtTag
}

#[derive(Debug)]
pub enum StmtTag {
    ExprStmt(Option<Expr>),
    CompoundStmt(Vec<Stmt>),
    DeclStmt(DeclTag),
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
pub enum DeclTag {
    RecordDecl(Vec<FieldDecl>),
    EnumDecl(Vec<EnumConstantDecl>),
    VarDecl
}

#[derive(Debug)]
pub enum DeclSpecifier {
    TypeSpecifier(TypeSpecifier),
    TypeQualifier(Token)
}

/// Represents single declaration in AST.
#[derive(Debug)]
pub struct Decl {
    /// ```c
    ///    register int a, b, c
    /// /* ^~~~~~~~~~~~ spec */
    /// ```
    pub spec: Vec<DeclSpecifier>,
    /// ```c
    /// int a, b, c
    ///  /* ^~~~~~~ declarators list */
    /// ```
    pub decl_list: Vec<InitDeclarator>
}

#[derive(Debug)]
pub enum Initializer {
    Assign(Expr),
    List(Vec<Initializer>)
}

#[derive(Debug)]
pub struct InitDeclarator(pub Declarator, pub Option<Initializer>);

#[derive(Debug)]
pub struct Declarator {
    pub inner: Box<DirectDeclarator>,
    pub suffix: Option<DeclaratorSuffix>
}

#[derive(Debug)]
pub enum DirectDeclarator {
    Identifier(Token),
    Paren(Declarator)
}

#[derive(Debug)]
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
    Func(Vec<ParamDecl>)
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
    pub decl: FieldDeclarator
}

#[derive(Debug)]
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
#[derive(Debug)]
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

/// Parameter declaration
#[derive(Debug)]
pub struct ParamDecl {
    pub spec: Vec<DeclSpecifier>,
    pub decl: Box<Declarator>
}

#[derive(Debug)]
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
    TypeDecl(DeclTag)
}

#[derive(Debug)]
pub struct Expr {
    pub tag: ExprTag,
    // todo: spans and stuff
}

#[derive(Debug)]
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
        r#type: Token
    },
    SizeofExpr {
        expr: Box<Expr>
    },
    CastExpr {
        r#type: Token,
        expr: Box<Expr>
    },
    Conditional {
        cond: Box<Expr>,
        then: Box<Expr>,
        otherwise: Box<Expr>
    }
}
