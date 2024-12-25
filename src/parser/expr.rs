///! This file implements ANSI C language parser.
///! For more information about grammar, see
///! <https://www.lysator.liu.se/c/ANSI-C-grammar-y.html>

use crate::ast::{Expr, ExprTag};
use crate::lexer::{TokenTag::*, Unspanable};
use crate::lexer::Token;
use crate::{check_tok, match_tok, require_tok};

use super::Parser;

impl<'a> Parser<'a> {
    pub fn constant_expression(&mut self) -> Expr {
        self.conditional()
    }

    pub fn expression(&mut self) -> Expr {
        let mut expr = self.assignment();

        while let Some(op) = match_tok!(self, Comma) {
            expr = Expr {
                tag: ExprTag::BinExpr {
                    op,
                    lhs: Box::new(expr),
                    rhs: Box::new(self.assignment())
                }
            }
        }

        expr
    }

    pub(crate) fn assignment(&mut self) -> Expr {
        let expr = self.conditional();

        if let Some(op) = match_tok!(
            self,
            Assign
            | MulAssign
            | DivAssign
            | ModAssign
            | AddAssign
            | SubAssign
            | LeftAssign
            | RightAssign
            | AndAssign
            | XorAssign
            | OrAssign
        ) {
            return Expr {
                tag: ExprTag::BinExpr {
                    op,
                    lhs: Box::new(expr),
                    rhs: Box::new(self.assignment())
                }
            }
        }

        expr
    }

    fn conditional(&mut self) -> Expr {
        let cond = self.logical_or();

        if check_tok!(self, Quest) {
            let then = self.expression();
            require_tok!(self, Colon);

            return Expr {
                tag: ExprTag::Conditional {
                    cond: Box::new(cond),
                    then: Box::new(then),
                    otherwise: Box::new(self.conditional())
                }
            }
        }

        return cond;
    }

    fn logical_or(&mut self) -> Expr {
        let mut expr = self.logical_and();

        while let Some(op) = match_tok!(self, Or) {
            expr = Expr {
                tag: ExprTag::BinExpr {
                    op,
                    lhs: Box::new(expr),
                    rhs: Box::new(self.logical_and())
                }
            }
        }

        expr
    }

    fn logical_and(&mut self) -> Expr {
        let mut expr = self.inclusive_or();

        while let Some(op) = match_tok!(self, And) {
            expr = Expr {
                tag: ExprTag::BinExpr {
                    op,
                    lhs: Box::new(expr),
                    rhs: Box::new(self.inclusive_or())
                }
            }
        }

        expr
    }

    fn inclusive_or(&mut self) -> Expr {
        let mut expr = self.exclusive_or();

        while let Some(op) = match_tok!(self, Bar) {
            expr = Expr {
                tag: ExprTag::BinExpr {
                    op,
                    lhs: Box::new(expr),
                    rhs: Box::new(self.exclusive_or())
                }
            }
        }

        expr
    }

    fn exclusive_or(&mut self) -> Expr {
        let mut expr = self.equality();

        while let Some(op) = match_tok!(self, Circ) {
            expr = Expr {
                tag: ExprTag::BinExpr {
                    op,
                    lhs: Box::new(expr),
                    rhs: Box::new(self.and())
                }
            }
        }

        expr
    }

    fn and(&mut self) -> Expr {
        let mut expr = self.equality();

        while let Some(op) = match_tok!(self, Ampersand) {
            expr = Expr {
                tag: ExprTag::BinExpr {
                    op,
                    lhs: Box::new(expr),
                    rhs: Box::new(self.equality())
                }
            }
        }

        expr
    }

    fn equality(&mut self) -> Expr {
        let mut expr = self.relational();

        while let Some(op) = match_tok!(self, Eq | Neq) {
            expr = Expr {
                tag: ExprTag::BinExpr {
                    op,
                    lhs: Box::new(expr),
                    rhs: Box::new(self.relational())
                }
            }
        }

        expr
    }

    fn relational(&mut self) -> Expr {
        let mut expr = self.shift();

        while let Some(op) = match_tok!(self, Lt | Le | Gt | Ge) {
            expr = Expr {
                tag: ExprTag::BinExpr {
                    op,
                    lhs: Box::new(expr),
                    rhs: Box::new(self.shift())
                }
            }
        }

        expr
    }

    fn shift(&mut self) -> Expr {
        let mut expr = self.additive();

        while let Some(op) = match_tok!(self, Left | Right) {
            expr = Expr {
                tag: ExprTag::BinExpr {
                    op,
                    lhs: Box::new(expr),
                    rhs: Box::new(self.additive())
                }
            }
        }

        expr
    }

    fn additive(&mut self) -> Expr {
        let mut expr = self.multiplicative();

        while let Some(op) = match_tok!(self, Plus | Minus) {
            expr = Expr {
                tag: ExprTag::BinExpr {
                    op,
                    lhs: Box::new(expr),
                    rhs: Box::new(self.multiplicative())
                }
            };
        }

        expr
    }

    fn multiplicative(&mut self) -> Expr {
        let mut expr = self.cast();

        while let Some(op) = match_tok!(self, Asterisk | Slash | Mod) {
            expr = Expr {
                tag: ExprTag::BinExpr {
                    op,
                    lhs: Box::new(expr),
                    rhs: Box::new(self.cast())
                }
            }
        }

        expr
    }

    fn cast(&mut self) -> Expr {
        if matches!(self.iter.peek().val(), Some(LeftParen))
            && matches!(self.iter.lookahead(2).val(), Some(Identifier))
        {
            self.iter.next();
            let type_name = require_tok!(self, Identifier); // TODO: Parse type name
            require_tok!(self, RightParen);

            return Expr {
                tag: ExprTag::CastExpr {
                    r#type: type_name,
                    expr: Box::new(self.cast())
                }
            }
        }

        return self.unary();
    }

    fn unary(&mut self) -> Expr {
        match self.iter.peek().val() {
            Some(
                Ampersand
                | Asterisk
                | Plus
                | Minus
                | Tilde
                | Not
            ) => {
                Expr {
                    tag: ExprTag::UnExpr {
                        op: self.iter.next().unwrap(),
                        rhs: Box::new(self.cast())
                    }
                }
            },
            Some(Sizeof) => {
                if check_tok!(self, LeftParen) {
                    let sizeof_expr = Expr {
                        tag: ExprTag::SizeofType {
                            r#type: require_tok!(self, Identifier) // TODO: Parse type name
                        }
                    };
                    require_tok!(self, RightParen);
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
            LeftBrace
            | LeftParen
            | Dot
            | ArrowRight
            | Decrement
        ) {
            expr = match tok.0 {
                // todo: somehow make this transformation with macros
                LeftBrace => { // sugar
                    self.expression();
                    require_tok!(self, RightBrace);
                    todo!()
                },
                LeftParen => self.parse_call(expr),
                Dot => {
                    Expr {
                        tag: ExprTag::MemberAccess {
                            expr: Box::new(expr),
                            member: tok
                        }
                    }
                },
                ArrowRight => { // sugar
                    require_tok!(self, Identifier);
                    todo!()
                },
                Increment | Decrement => { // sugar
                    todo!()
                },
                _ => panic!()
            }
        }

        expr
    }

    fn parse_call(&mut self, calle: Expr) -> Expr {
        if check_tok!(self, RightParen) { // Empty args
            return Expr {
                tag: ExprTag::Call {
                    calle: Box::new(calle),
                    args: Vec::with_capacity(0),
                }
            }
        }

        let expr = Expr {
            tag: ExprTag::Call {
                calle: Box::new(calle),
                args: self.parse_args(),
            }
        };

        require_tok!(self, RightParen);
        expr
    }

    fn parse_args(&mut self) -> Vec<Expr> {
        dbg!(self.iter.peek());
        let mut args = Vec::new();
        args.push(self.assignment());

        while check_tok!(self, Comma) {
            args.push(self.assignment());
        }

        args
    }

    fn primary(&mut self) -> Expr {
        match self.iter.peek().val() {
            Some(
                Identifier
                | StringLiteral
                | CharLiteral
                | NumberLiteral { .. }
            ) => {
                Expr {
                    tag: ExprTag::Primary(self.iter.next().unwrap())
                }
            },
            Some(LeftParen) => {
                self.iter.next();
                let expr = self.expression();
                require_tok!(self, RightParen);
                expr
            }
            tok => panic!("unexpected token `{:?}`", tok)
        }
    }
}
