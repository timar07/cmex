///! This file implements ANSI C language parser.
///! For more information about grammar, see
///! <https://www.lysator.liu.se/c/ANSI-C-grammar-y.html>

use crate::ast::{Expr, ExprTag};
use crate::lexer::TokenTag;
use crate::{check_tok, match_tok, require_tok};

use super::Parser;

impl<'a> Parser<'a> {
    pub fn expression(&mut self) -> Expr {
        self.postfix()
    }

    fn assignment(&mut self) -> Expr {
        todo!();
    }

    fn postfix(&mut self) -> Expr {
        let expr = Box::new(self.primary());

        match self.iter.peek().unwrap() {
            // todo: somehow make this transformation with macros
            TokenTag::LeftBrace => { // sugar
                self.iter.next();
                self.expression();
                require_tok!(self, TokenTag::RightBrace);
                todo!()
            },
            TokenTag::LeftParen => self.parse_call(expr),
            TokenTag::Dot => {
                self.iter.next();
                require_tok!(self, TokenTag::Identifier);
                todo!()
            },
            TokenTag::ArrowRight => { // sugar
                self.iter.next();
                require_tok!(self, TokenTag::Identifier);
                todo!()
            },
            TokenTag::Increment | TokenTag::Decrement => { // sugar
                self.iter.next();
                todo!()
            },
            _ => panic!()
        }
    }

    fn parse_call(&mut self, calle: Box<Expr>) -> Expr {
        self.iter.next();

        if check_tok!(self, TokenTag::LeftParen) {
            return Expr {
                tag: ExprTag::Call {
                    calle,
                    args: Vec::with_capacity(0),
                }
            }
        }

        Expr {
            tag: ExprTag::Call {
                calle,
                args: self.parse_args(),
            }
        }
    }

    fn parse_args(&mut self) -> Vec<Expr> {
        let mut args = Vec::new();
        args.push(self.assignment());

        while check_tok!(self, TokenTag::Comma) {
            args.push(self.assignment());
        }

        args
    }

    fn primary(&mut self) -> Expr {
        match self.iter.peek().unwrap() {
            TokenTag::Identifier
            | TokenTag::StringLiteral
            | TokenTag::CharLiteral
            | TokenTag::NumberLiteral { .. } => {
                self.iter.next();
                Expr {
                    tag: ExprTag::Primary
                }
            },
            TokenTag::LeftParen => {
                self.iter.next();
                let expr = self.expression();
                require_tok!(self, TokenTag::RightParen);
                expr
            }
            tok => panic!("unexpected token `{:?}`", tok)
        }
    }
}
