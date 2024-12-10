use crate::lexer::TokenTag;

pub struct Stmt {
    pub tag: StmtTag
}

pub enum StmtTag {
    Declaration
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
