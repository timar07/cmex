use crate::lexer::TokenTag;

#[derive(Debug)]
pub struct Expr {
    pub tag: ExprTag,
    // todo: spans and stuff
}

/// An operation performed between two operands
#[derive(Debug)]
pub enum BinOp {
    /// Addition,'+' token
    Add,
    /// Substraction
    Sub,
}

/// An operation performed over the one operand
#[derive(Debug)]
pub enum UnOp {
    /// Unary minus, '-' token
    Minus
}

#[derive(Debug)]
pub enum ExprTag {
    Primary,
    BinExpr {
        op: BinOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>
    },
    UnExpr {
        op: UnOp,
        rhs: Box<Expr>
    },
    TernExpr,
    Call {
        /// foo(5, bar)
        /// ^~~ call expression
        calle: Box<Expr>,
        /// foo(5, bar)
        ///    ^~~~~~~~ args
        args: Vec<Expr>
    }
}
