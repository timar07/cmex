///! This file implements ANSI C language parser.
///! For more information about grammar, see
///! <https://www.lysator.liu.se/c/ANSI-C-grammar-y.html>

use crate::ast::{Expr, ExprTag};
use crate::lexer::TokenTag;
use crate::{check_tok, match_tok, require_tok};

use super::Parser;

impl<'a> Parser<'a> {
    pub fn expression(&mut self) -> Expr {
        todo!()
    }

    fn assignment(&mut self) -> Expr {
        todo!();
    }

    fn cast(&mut self) -> Expr {
        todo!();
    }

    fn unary(&mut self) -> Expr {
        match self.iter.peek() {
            Some(
                TokenTag::Ampersand
                | TokenTag::Asterisk
                | TokenTag::Plus
                | TokenTag::Minus
                | TokenTag::Tilde
                | TokenTag::Not
            ) => {
                Expr {
                    tag: ExprTag::UnExpr {
                        op: self.iter.next().unwrap(),
                        rhs: Box::new(self.cast())
                    }
                }
            },
            Some(TokenTag::Sizeof) => {
                if check_tok!(self, TokenTag::LeftParen) {
                    let sizeof_expr = Expr {
                        tag: ExprTag::SizeofType {
                            r#type: require_tok!(self, TokenTag::Identifier)
                        }
                    };
                    require_tok!(self, TokenTag::RightParen);
                    sizeof_expr
                } else {
                    Expr {
                        tag: ExprTag::SizeofExpr {
                            expr: Box::new(self.unary())
                        }
                    }
                }
            },
            _ => self.postfix()
        }
    }

    fn postfix(&mut self) -> Expr {
        let mut expr = self.primary();

        while let Some(tok) = match_tok!(
            self,
            TokenTag::LeftBrace
            | TokenTag::LeftParen
            | TokenTag::Dot
            | TokenTag::ArrowRight
            | TokenTag::Decrement
        ) {
            expr = match tok {
                // todo: somehow make this transformation with macros
                TokenTag::LeftBrace => { // sugar
                    self.expression();
                    require_tok!(self, TokenTag::RightBrace);
                    todo!()
                },
                TokenTag::LeftParen => self.parse_call(expr),
                TokenTag::Dot => {
                    Expr {
                        tag: ExprTag::MemberAccess {
                            expr: Box::new(expr),
                            member: tok
                        }
                    }
                },
                TokenTag::ArrowRight => { // sugar
                    require_tok!(self, TokenTag::Identifier);
                    todo!()
                },
                TokenTag::Increment | TokenTag::Decrement => { // sugar
                    todo!()
                },
                _ => panic!()
            }
        }

        expr
    }

    fn parse_call(&mut self, calle: Expr) -> Expr {
        self.iter.next();

        if check_tok!(self, TokenTag::LeftParen) {
            return Expr {
                tag: ExprTag::Call {
                    calle: Box::new(calle),
                    args: Vec::with_capacity(0),
                }
            }
        }

        Expr {
            tag: ExprTag::Call {
                calle: Box::new(calle),
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
