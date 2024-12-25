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
    Identifier,
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

    /// Error recovery token
    Error
}

impl TokenTag {
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NumberLiteralKind {
    Exponent,
    Float,
    Int
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NumberLiteralSuffix {
    Float,
    Unsigned,
    Long,
    UnsignedLong,
    UnsignedLongLong,
    LongLong
}

pub trait Unspanable<T> {
    fn val(self) -> Option<T>;
}

pub type SpannedOption<I> = Option<(I, Span)>;

impl<T> Unspanable<T> for SpannedOption<T> {
    /// A handy method to convert `Option<(T, Span)>` to `Option<T>`
    fn val(self) -> Option<T> {
        self.map(|(val, _)| val)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Span(pub usize, pub usize);

impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}:{}", self.0, self.1))
    }
}

pub struct Spanned<I> {
    pub iter: I,
}

impl<I> Spanned<I> {
    pub fn new(iter: I) -> Self {
        Self { iter }
    }
}

impl<I> Iterator for Spanned<I>
where
    I: Positioned + Iterator
{
    type Item = (<I as Iterator>::Item, Span);

    fn next(&mut self) -> Option<Self::Item> {
        let start = self.iter.get_pos();
        let item = self.iter.next()?;

        Some((item, Span(
            start,
            self.iter.get_pos()
        )))
    }
}

pub trait Positioned {
    fn get_pos(&self) -> usize;
}
