use crate::ast::Expr;

use super::Parser;

impl<'a> Parser<'a> {
    fn parse_expression(&mut self) -> Expr {
        todo!()
    }

    fn primary(&mut self) -> Option<Expr> {
        match self.iter.next()? {
            _ => todo!()
        }
    }
}
