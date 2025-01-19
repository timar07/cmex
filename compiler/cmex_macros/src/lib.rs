mod expand;
mod matcher;
mod parse;
mod tt_cursor;

use cmex_ast::{NtTag, TokenTree};
use cmex_lexer::{Token, TokenTag};
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
pub struct MacroRule(pub MacroMatcher, pub TokenTree);

/// A part of the macro that defines how token stream will be matched.
/// ```ignore
/// macro_rules! foo {
///     ($e:expr) => { /* ... */ }
///  /* ^~~~~~~~~ matcher */
/// }
/// ```
#[derive(Debug, Clone)]
pub struct MacroMatcher(pub Vec<MacroMatch>);

#[derive(Debug, Clone)]
pub enum MacroMatch {
    /// Any token except `$` and delimiters.
    Token(Token),
    /// Fragment-specified variable, for example
    /// ```ignore
    /// macro_rules! foo {
    ///     ($e:expr) => { /* ... */ }
    /// }
    /// ```
    Frag(String, NtTag),
    /// Repitition macro form, it has the following syntax:
    /// `$ ( ... ) sep? rep`
    Rep(Box<MacroMatcher>, Option<Token>, RepOpTag),
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
