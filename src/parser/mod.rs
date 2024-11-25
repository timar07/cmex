use crate::lexer::Lexer;

mod expr;

pub struct Parser<'a> {
    iter: Lexer<'a>
}
