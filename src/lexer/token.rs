#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Assign,
    AddAssign,
    And,
    Ampersand,
    AndAssign,
    ArrowRight,
    Asterisk,
    Auto,
    Not,
    Break,
    Case,
    Char,
    Comma,
    Colon,
    Const,
    Continue,
    Decrement,
    Default,
    Slash,
    DivAssign,
    Do,
    Dot,
    Double,
    Else,
    Enum,
    Ellipsis,
    Eq,
    Extern,
    Float,
    For,
    Goto,
    Ge,
    Gt,
    Hash,
    Identifier(String),
    Increment,
    If,
    Int,
    Le,
    Lt,
    Left,
    LeftAssign,
    LeftBrace,
    LeftCurly,
    LeftParen,
    Long,
    ModAssign,
    Mod,
    MulAssign,
    Neq,
    OrAssign,
    Or,
    Bar,
    Plus,
    Register,
    Return,
    Right,
    RightAssign,
    RightBrace,
    RightCurly,
    RightParen,
    Semicolon,
    Short,
    Signed,
    Sizeof,
    Static,
    StringLiteral,
    Struct,
    Switch,
    Minus,
    NumberLiteral {
        prefix: Option<NumberLiteralPrefix>,
        suffix: Option<NumberLiteralSuffix>,
        kind: NumberLiteralKind
    },
    SubAssign,
    TypeName(String),
    Typedef,
    Tilde,
    Union,
    Unsigned,
    Void,
    Volatile,
    While,
    Circ,
    Quest,
    XorAssign,
}

#[derive(Debug, PartialEq, Eq)]
pub enum NumberLiteralKind {
    Exponent,
    Float,
    Int
}

#[derive(Debug, PartialEq, Eq)]
pub enum NumberLiteralPrefix {
    Bin,
    Oct,
    Hex
}

impl std::fmt::Display for NumberLiteralPrefix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bin => write!(f, "0b"),
            Self::Oct => write!(f, "0o"),
            Self::Hex => write!(f, "0x"),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum NumberLiteralSuffix {
    Double,
    Float,
    Unsigned,
    Long,
    UnsignedLong,
    UnsignedLongLong,
    LongLong
}
