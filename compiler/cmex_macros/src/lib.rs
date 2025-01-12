mod parse;
mod expand;
mod tt_cursor;

use cmex_ast::TokenTree;
use cmex_lexer::Token;
pub use expand::MacroExpander;

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
pub struct MacroMatcher(pub Option<MacroMatch>);

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
    Frag(String, MacroFragSpec),
    /// Repitition macro form, it has the following syntax:
    /// `$ ( ... ) sep rep`
    Rep(Option<Box<MacroMatch>>, Option<Token>, Option<RepOpTag>),
}

#[derive(Debug, Clone)]
pub enum MacroFragSpec {
    Block,
    Literal,
    Ident,
    Item,
    Ty,
    Expr,
}

impl std::fmt::Display for MacroFragSpec {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Block => write!(f, "block"),
            Self::Literal => write!(f, "literal"),
            Self::Ident => write!(f, "ident"),
            Self::Item => write!(f, "item"),
            Self::Ty => write!(f, "ty"),
            Self::Expr => write!(f, "expr"),
        }
    }
}

#[derive(Debug, Clone)]
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

