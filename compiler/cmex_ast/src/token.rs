use cmex_span::Span;

use crate::Nonterminal;

pub type Token = (TokenTag, Span);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenTag {
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
    CharLiteral,
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
    #[allow(dead_code)]
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
    StringLiteral(String),
    Struct,
    Switch,
    Minus,
    NumberLiteral {
        literal: String,
        prefix: Option<NumberLiteralPrefix>,
        suffix: Option<NumberLiteralSuffix>,
        kind: NumberLiteralKind,
    },
    SubAssign,
    Typedef,
    Tilde,
    Union,
    Unsigned,
    Void,
    Volatile,
    While,
    Circ,
    Quest,
    Dollar,
    XorAssign,

    FatArrow,
    MacroRules,

    /// Interpolated token, used in macros expansion
    Interpolated(Box<Nonterminal>),

    /// Error recovery token
    Error,
}

impl TokenTag {
    pub fn is_delimiter(&self) -> bool {
        matches!(
            self,
            TokenTag::LeftCurly
                | TokenTag::RightCurly
                | TokenTag::LeftBrace
                | TokenTag::RightBrace
                | TokenTag::LeftParen
                | TokenTag::RightParen
        )
    }

    pub fn is_keyword_or_id(&self) -> bool {
        matches!(self, Self::Identifier(_)) || self.is_keyword()
    }

    pub fn is_keyword(&self) -> bool {
        matches!(
            self,
            TokenTag::Auto
                | TokenTag::Break
                | TokenTag::Case
                | TokenTag::Char
                | TokenTag::Const
                | TokenTag::Continue
                | TokenTag::Default
                | TokenTag::Do
                | TokenTag::Double
                | TokenTag::Else
                | TokenTag::Enum
                | TokenTag::Extern
                | TokenTag::Float
                | TokenTag::For
                | TokenTag::Goto
                | TokenTag::If
                | TokenTag::Int
                | TokenTag::Long
                | TokenTag::Register
                | TokenTag::Return
                | TokenTag::Short
                | TokenTag::Signed
                | TokenTag::Sizeof
                | TokenTag::Static
                | TokenTag::Struct
                | TokenTag::Switch
                | TokenTag::Typedef
                | TokenTag::Union
                | TokenTag::Unsigned
                | TokenTag::Void
                | TokenTag::Volatile
                | TokenTag::While
        )
    }
}

impl std::fmt::Display for TokenTag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Assign => write!(f, "="),
            Self::AddAssign => write!(f, "+="),
            Self::And => write!(f, "&&"),
            Self::Ampersand => write!(f, "&"),
            Self::AndAssign => write!(f, "&="),
            Self::ArrowRight => write!(f, "->"),
            Self::Asterisk => write!(f, "*"),
            Self::Auto => write!(f, "auto"),
            Self::Not => write!(f, "!"),
            Self::Break => write!(f, "break"),
            Self::Case => write!(f, "case"),
            Self::Char => write!(f, "char"),
            Self::Comma => write!(f, ","),
            Self::CharLiteral => write!(f, "character literal"),
            Self::Colon => write!(f, ":"),
            Self::Const => write!(f, "const"),
            Self::Continue => write!(f, "continue"),
            Self::Decrement => write!(f, "--"),
            Self::Default => write!(f, "default"),
            Self::Slash => write!(f, "/"),
            Self::DivAssign => write!(f, "/="),
            Self::Do => write!(f, "do"),
            Self::Dot => write!(f, "."),
            Self::Double => write!(f, "double"),
            Self::Else => write!(f, "else"),
            Self::Enum => write!(f, "enum"),
            Self::Ellipsis => write!(f, "..."),
            Self::Eq => write!(f, "=="),
            Self::Extern => write!(f, "extern"),
            Self::Float => write!(f, "float"),
            Self::For => write!(f, "for"),
            Self::Goto => write!(f, "goto"),
            Self::Ge => write!(f, ">="),
            Self::Gt => write!(f, ">"),
            Self::Hash => write!(f, "#"),
            Self::Identifier(s) => write!(f, "{s}"),
            Self::Increment => write!(f, "++"),
            Self::If => write!(f, "if"),
            Self::Int => write!(f, "int"),
            Self::Le => write!(f, "<="),
            Self::Lt => write!(f, "<"),
            Self::Left => write!(f, "<<"),
            Self::LeftAssign => write!(f, "<<="),
            Self::LeftBrace => write!(f, "["),
            Self::LeftCurly => write!(f, "{{"),
            Self::LeftParen => write!(f, "("),
            Self::Long => write!(f, "long"),
            Self::ModAssign => write!(f, "%="),
            Self::Mod => write!(f, "%"),
            Self::MulAssign => write!(f, "*="),
            Self::Neq => write!(f, "!="),
            Self::OrAssign => write!(f, "|="),
            Self::Or => write!(f, "||"),
            Self::Bar => write!(f, "|"),
            Self::Plus => write!(f, "+"),
            Self::Register => write!(f, "register"),
            Self::Return => write!(f, "return"),
            Self::Right => write!(f, ">>"),
            Self::RightAssign => write!(f, ">>="),
            Self::RightBrace => write!(f, "]"),
            Self::RightCurly => write!(f, "}}"),
            Self::RightParen => write!(f, ")"),
            Self::Semicolon => write!(f, ";"),
            Self::Short => write!(f, "short"),
            Self::Signed => write!(f, "signed"),
            Self::Sizeof => write!(f, "sizeof"),
            Self::Static => write!(f, "static"),
            Self::StringLiteral(s) => write!(f, "\"{s}\""),
            Self::Struct => write!(f, "struct"),
            Self::Switch => write!(f, "switch"),
            Self::Minus => write!(f, "-"),
            Self::NumberLiteral {
                literal,
                prefix,
                suffix,
                ..
            } => write!(
                f,
                "{}{literal}{}",
                prefix.map(|prefix| prefix.to_string()).unwrap_or_default(),
                suffix.map(|suffix| suffix.to_string()).unwrap_or_default()
            ),
            Self::SubAssign => write!(f, "-="),
            Self::Typedef => write!(f, "typedef"),
            Self::Tilde => write!(f, "~"),
            Self::Union => write!(f, "union"),
            Self::Unsigned => write!(f, "unsigned"),
            Self::Void => write!(f, "void"),
            Self::Volatile => write!(f, "volatile"),
            Self::While => write!(f, "while"),
            Self::Circ => write!(f, "^"),
            Self::Quest => write!(f, "?"),
            Self::Dollar => write!(f, "$"),
            Self::XorAssign => write!(f, "^="),
            Self::FatArrow => write!(f, "=>"),
            Self::MacroRules => write!(f, "macro_rules"),
            Self::Interpolated(_) => write!(f, "<interpolated>"),
            Self::Error => write!(f, "<error>"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NumberLiteralKind {
    Exponent,
    Float,
    Int,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NumberLiteralPrefix {
    Bin,
    Oct,
    Hex,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NumberLiteralSuffix {
    Float,
    Unsigned,
    Long,
    UnsignedLong,
    UnsignedLongLong,
    LongLong,
}

impl std::fmt::Display for NumberLiteralSuffix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NumberLiteralSuffix::Float => write!(f, "f"),
            NumberLiteralSuffix::Unsigned => write!(f, "u"),
            NumberLiteralSuffix::Long => write!(f, "l"),
            NumberLiteralSuffix::UnsignedLong => write!(f, "ul"),
            NumberLiteralSuffix::UnsignedLongLong => write!(f, "ull"),
            NumberLiteralSuffix::LongLong => write!(f, "ll"),
        }
    }
}
