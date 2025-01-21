mod expand;
mod matcher;
mod parse;
mod tt_cursor;

use cmex_ast::token::Token;
use cmex_ast::{DelimSpan, DelimTag, NtTag};
pub use expand::MacroExpander;

/// Macro rule consists of lhs (i.e. matcher) and rhs (some token tree).
/// For example:
/// ```ignore
/// macro rules! foo {
///     ($e:expr) => { /* ... */ }
///  /* ^~~~~~~~~    ^~~~~~~~~~~~~ */
///  /* matcher      token tree */
/// }
/// ```
#[derive(Debug, Clone)]
pub struct MacroRule(pub MacroMatcher, pub DelimMtt);

/// A part of the macro that defines how token stream will be matched.
/// ```ignore
/// macro_rules! foo {
///     ($e:expr) => { /* ... */ }
///  /* ^~~~~~~~~ matcher */
/// }
/// ```
#[derive(Debug, Clone)]
pub struct MacroMatcher(pub Vec<MacroTokenTree>);

#[derive(Debug, Clone)]
pub struct DelimMtt {
    pub delim: DelimTag,
    pub mtt: Vec<MacroTokenTree>,
    pub span: DelimSpan,
}

#[derive(Debug, Clone)]
pub enum MacroTokenTree {
    /// Any token except `$` and delimiters.
    Token(Token),
    /// Fragment-specified variable, for example
    /// ```ignore
    /// macro_rules! foo {
    ///     ($e:expr) => { /* ... */ }
    /// }
    /// ```
    Frag(Token, Option<NtTag>),
    /// Repitition macro form, it has the following syntax:
    /// `$ ( ... ) sep? rep` for lhs and `$( ... ) rep` for rhs
    Rep(Vec<MacroTokenTree>, Option<Token>, RepOpTag),
    /// Delimited token tree e.g. `(a b c)`
    Delim(DelimMtt),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RepOpTag {
    /// One or more repitition
    Plus,
    /// Zero or more repitition
    Asterisk,
    /// Zero or one repitition
    Quest,
}

impl std::fmt::Display for RepOpTag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Plus => write!(f, "+"),
            Self::Asterisk => write!(f, "*"),
            Self::Quest => write!(f, "?"),
        }
    }
}
