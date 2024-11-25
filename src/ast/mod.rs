pub struct Expr {
    tag: ExprTag,
    // todo: spans and stuff
}

/// An operation performed between two operands
pub enum BinOp {
    /// Addition,'+' token
    Add,
    /// Substraction
    Sub,
}

/// An operation performed over the one operand
pub enum UnOp {
    /// Unary minus, '-' token
    Minus
}

pub enum ExprTag {
    BinExpr {
        op: BinOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>
    },
    UnExpr {
        op: UnOp,
        rhs: Box<Expr>
    },
    TernExpr
}


