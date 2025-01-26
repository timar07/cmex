pub mod ast_dump;
pub mod token;
mod tree_builder;

use cmex_span::{MaybeSpannable, Span, Spannable};

use token::{Token, TokenTag};

#[derive(Clone)]
pub struct TranslationUnit(pub Vec<DeclTag>);

#[derive(Clone)]
pub enum Item {
    Invocation(InvocationTag),
    Decl(DeclTag),
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
    Decl(DeclTag),
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
    Return(Token, Option<Expr>),
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
            Self::Return((_, span), _) => *span,
            Self::Goto(tok) => tok.1,
        }
    }
}

#[derive(Debug, Clone)]
pub enum DeclTag {
    /// `include` are represented directly in AST.
    Include {
        path: String,
        span: Span,
    },
    Record(Option<Token>, Vec<FieldDecl>),
    Enum(Option<Token>, Vec<EnumConstantDecl>),
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
        decl: Box<Declarator>,
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
    Macro {
        id: Token,
        body: TokenTree,
    },
}

impl Spannable for DeclTag {
    fn span(&self) -> Span {
        match self {
            DeclTag::Include { span, .. } => *span,
            DeclTag::Record(_, vec) => vec.span().unwrap(),
            DeclTag::Enum(_, vec) => vec.span().unwrap(),
            DeclTag::Func { spec, decl, body } => Span::join(
                spec.clone()
                    .map(|specs| specs.span().unwrap())
                    .unwrap_or_else(|| decl.span()),
                body.tag.span(),
            ),
            DeclTag::Var { spec: _, decl_list } => decl_list.span().unwrap(),
            DeclTag::Macro { id, .. } => id.1,
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

#[derive(Debug, Clone)]
pub struct Declarator {
    pub prefix: Vec<DeclaratorPrefix>,
    pub inner: Box<DirectDeclarator>,
    pub suffix: Option<DeclaratorSuffix>,
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

/*
impl std::fmt::Display for DirectDeclarator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Identifier((tok, _)) => write!(f, "{}", tok),
            Self::Paren(decl) => write!(f, "({})", decl),
            Self::Abstract => write!(f, ""),
        }
    }
}
    */

#[derive(Debug, Clone)]
pub enum DeclaratorPrefix {
    Pointer(Vec<Token>),
}

impl std::fmt::Display for DeclaratorPrefix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DeclaratorPrefix::Pointer(vec) => {
                write!(
                    f,
                    "* {}",
                    vec.iter()
                        .map(|(tok, _)| tok.to_string())
                        .collect::<Vec<String>>()
                        .join(" ")
                )
            }
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
    pub specs: Vec<TypeSpecifier>,
    pub decl: FieldDeclarator,
}

impl Spannable for FieldDecl {
    fn span(&self) -> Span {
        self.decl.span()
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
    Record(Option<Token>, Vec<FieldDecl>),
    Enum(Option<Token>, Vec<EnumConstantDecl>),
}

impl Spannable for TypeSpecifier {
    fn span(&self) -> Span {
        match self {
            TypeSpecifier::TypeName((_, span)) => *span,
            TypeSpecifier::Record(_, vec) => vec.span().unwrap(),
            TypeSpecifier::Enum(_, vec) => vec.span().unwrap(),
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
    pub specs: Vec<TypeSpecifier>,
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
pub enum Expr {
    Primary(Token),
    Paren(Box<Expr>),
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
    Invocation(InvocationTag),
    StmtExpr(Vec<Stmt>, Span),
}

impl Spannable for Expr {
    fn span(&self) -> Span {
        match self {
            Self::Paren(expr) => expr.span(),
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
            Self::Invocation(tag) => tag.span(),
            Self::StmtExpr(_, span) => *span,
        }
    }
}

/// Other types of invocations may be implemented
#[derive(Debug, Clone)]
pub enum InvocationTag {
    Bang(Token, Option<TokenTree>),
}

impl Spannable for InvocationTag {
    fn span(&self) -> Span {
        match self {
            InvocationTag::Bang((_, span), _) => *span,
        }
    }
}

/// Abstract tokens collection, mostly used in macros
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenTree {
    /// A primary node of the tree - the token itself
    Token(Token),
    /// Delimited token tree, i.e. token tree wrapped with braces,
    /// e.g. curlies, brackets or parenthesis
    Delim(DelimTag, Vec<TokenTree>, DelimSpan),
}

impl TokenTree {
    pub fn flatten(&self) -> Vec<Token> {
        match self {
            TokenTree::Token(tok) => vec![tok.clone()],
            TokenTree::Delim(delim_tag, vec, delim_span) => {
                let mut toks = vec![];
                let (l, r) = delim_tag.get_delims();
                let DelimSpan(lspan, rspan) = delim_span.clone();
                toks.push((l, lspan));
                toks.append(
                    &mut vec
                        .to_vec()
                        .iter()
                        .map(|tt| tt.flatten())
                        .reduce(|a, b| [a, b].concat())
                        .unwrap_or_else(Vec::new),
                );
                toks.push((r, rspan));
                toks
            }
        }
    }
}

impl Spannable for TokenTree {
    fn span(&self) -> Span {
        match self {
            Self::Token(tok) => tok.1,
            Self::Delim(_, _, span) => span.span(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DelimTag {
    /// Curly braces `{` `}`
    Curly,
    /// Square braces `[` `]`
    Square,
    /// Parenthesis `(` `)`
    Paren,
}

impl DelimTag {
    pub fn get_delims(&self) -> (TokenTag, TokenTag) {
        match self {
            DelimTag::Curly => (TokenTag::LeftCurly, TokenTag::RightCurly),
            DelimTag::Square => (TokenTag::LeftBrace, TokenTag::RightBrace),
            DelimTag::Paren => (TokenTag::LeftParen, TokenTag::RightParen),
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct DelimSpan(pub Span, pub Span);

impl Spannable for DelimSpan {
    fn span(&self) -> Span {
        Span::join(self.0, self.1)
    }
}

#[derive(Debug, Clone)]
pub enum Nonterminal {
    Block(Vec<Stmt>),
    Literal(Token),
    Ident(Token),
    Item(Vec<DeclTag>),
    Ty(TypeName),
    Expr(Expr),
    Tt(Vec<TokenTree>),
}

impl std::fmt::Display for Nonterminal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Nonterminal::Block(_) => write!(f, "block"),
            Nonterminal::Literal(_) => write!(f, "literal"),
            Nonterminal::Ident(_) => write!(f, "ident"),
            Nonterminal::Item(_) => write!(f, "item"),
            Nonterminal::Ty(_) => write!(f, "type name"),
            Nonterminal::Expr(_) => write!(f, "expr"),
            Nonterminal::Tt(_) => write!(f, "token tree"),
        }
    }
}

impl Eq for Nonterminal {}

impl PartialEq for Nonterminal {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Block(_), Self::Block(_)) => true,
            (Self::Literal(l0), Self::Literal(r0)) => l0 == r0,
            (Self::Ident(l0), Self::Ident(r0)) => l0 == r0,
            (Self::Item(_), Self::Item(_)) => true,
            (Self::Ty(_), Self::Ty(_)) => true,
            (Self::Expr(_), Self::Expr(_)) => true,
            _ => false,
        }
    }
}

/// Nonterminal tag
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum NtTag {
    /// Zero or more satements wrapped with { ... }
    Block,
    /// Any literal, e.g. number, string, char
    Literal,
    /// Identifier
    Ident,
    /// Any item, declaration in terms of C
    Item,
    /// Type
    Ty,
    /// Token tree
    Tt,
    /// Expression
    Expr,
}

impl std::fmt::Display for NtTag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Block => write!(f, "block"),
            Self::Literal => write!(f, "literal"),
            Self::Ident => write!(f, "ident"),
            Self::Item => write!(f, "item"),
            Self::Ty => write!(f, "ty"),
            Self::Tt => write!(f, "tt"),
            Self::Expr => write!(f, "expr"),
        }
    }
}
