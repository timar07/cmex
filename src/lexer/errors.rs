#[derive(Debug, PartialEq, Eq)]
pub enum LexError {
    UnexpectedCharacter(char),
    UnknownEscapeSequenceCharacter(char),
    UnterminatedString,
    UnexpectedEof,
    InvalidDigit(char),
    ExponentHasNoDigits,
    InvalidNumberLiteralSuffix(String)
}

impl std::fmt::Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnterminatedString => {
                write!(f, "unterminated string")
            },
            Self::UnexpectedCharacter(c) => {
                write!(f, "unexpected character `{c}`")
            },
            Self::UnknownEscapeSequenceCharacter(c) => {
                write!(f, "unknown escape sequence character `{c}`")
            },
            Self::UnexpectedEof => {
                write!(f, "unexpected end of file")
            },
            Self::InvalidDigit(c) => {
                write!(f, "invalid digit `{c}`")
            },
            Self::ExponentHasNoDigits => {
                write!(f, "exponent has no digits")
            },
            Self::InvalidNumberLiteralSuffix(s) => {
                write!(f, "invalid suffix `{s}`")
            }
        }
    }
}
