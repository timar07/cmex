//! This file implements ANSI C language parser.
//! For more information about grammar, see
//! <https://www.lysator.liu.se/c/ANSI-C-grammar-y.html>

use super::{ParseErrorTag, Parser, PR};
use crate::{check_tok, match_tok, require_tok};
use cmex_ast::{Expr, ExprTag};
use cmex_lexer::TokenTag::*;
use cmex_span::{Span, Unspan};

impl Parser<'_> {
    pub fn constant_expression(&mut self) -> PR<Expr> {
        self.conditional()
    }

    pub fn expression(&mut self) -> PR<Expr> {
        let mut expr = self.assignment()?;

        while let Some(op) = match_tok!(self, Comma) {
            expr = Expr {
                tag: ExprTag::BinExpr {
                    op,
                    lhs: Box::new(expr),
                    rhs: Box::new(self.assignment()?),
                },
            }
        }

        Ok(expr)
    }

    pub(crate) fn assignment(&mut self) -> PR<Expr> {
        let expr = self.conditional()?;

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
            return Ok(Expr {
                tag: ExprTag::BinExpr {
                    op,
                    lhs: Box::new(expr),
                    rhs: Box::new(self.assignment()?),
                },
            });
        }

        Ok(expr)
    }

    fn conditional(&mut self) -> PR<Expr> {
        let cond = self.logical_or()?;

        if check_tok!(self, Quest) {
            let then = self.expression()?;
            require_tok!(self, Colon)?;

            return Ok(Expr {
                tag: ExprTag::Conditional {
                    cond: Box::new(cond),
                    then: Box::new(then),
                    otherwise: Box::new(self.conditional()?),
                },
            });
        }

        Ok(cond)
    }

    fn logical_or(&mut self) -> PR<Expr> {
        let mut expr = self.logical_and()?;

        while let Some(op) = match_tok!(self, Or) {
            expr = Expr {
                tag: ExprTag::BinExpr {
                    op,
                    lhs: Box::new(expr),
                    rhs: Box::new(self.logical_and()?),
                },
            }
        }

        Ok(expr)
    }

    fn logical_and(&mut self) -> PR<Expr> {
        let mut expr = self.inclusive_or()?;

        while let Some(op) = match_tok!(self, And) {
            expr = Expr {
                tag: ExprTag::BinExpr {
                    op,
                    lhs: Box::new(expr),
                    rhs: Box::new(self.inclusive_or()?),
                },
            }
        }

        Ok(expr)
    }

    fn inclusive_or(&mut self) -> PR<Expr> {
        let mut expr = self.exclusive_or()?;

        while let Some(op) = match_tok!(self, Bar) {
            expr = Expr {
                tag: ExprTag::BinExpr {
                    op,
                    lhs: Box::new(expr),
                    rhs: Box::new(self.exclusive_or()?),
                },
            }
        }

        Ok(expr)
    }

    fn exclusive_or(&mut self) -> PR<Expr> {
        let mut expr = self.equality()?;

        while let Some(op) = match_tok!(self, Circ) {
            expr = Expr {
                tag: ExprTag::BinExpr {
                    op,
                    lhs: Box::new(expr),
                    rhs: Box::new(self.and()?),
                },
            }
        }

        Ok(expr)
    }

    fn and(&mut self) -> PR<Expr> {
        let mut expr = self.equality()?;

        while let Some(op) = match_tok!(self, Ampersand) {
            expr = Expr {
                tag: ExprTag::BinExpr {
                    op,
                    lhs: Box::new(expr),
                    rhs: Box::new(self.equality()?),
                },
            }
        }

        Ok(expr)
    }

    fn equality(&mut self) -> PR<Expr> {
        let mut expr = self.relational()?;

        while let Some(op) = match_tok!(self, Eq | Neq) {
            expr = Expr {
                tag: ExprTag::BinExpr {
                    op,
                    lhs: Box::new(expr),
                    rhs: Box::new(self.relational()?),
                },
            }
        }

        Ok(expr)
    }

    fn relational(&mut self) -> PR<Expr> {
        let mut expr = self.shift()?;

        while let Some(op) = match_tok!(self, Lt | Le | Gt | Ge) {
            expr = Expr {
                tag: ExprTag::BinExpr {
                    op,
                    lhs: Box::new(expr),
                    rhs: Box::new(self.shift()?),
                },
            }
        }

        Ok(expr)
    }

    fn shift(&mut self) -> PR<Expr> {
        let mut expr = self.additive()?;

        while let Some(op) = match_tok!(self, Left | Right) {
            expr = Expr {
                tag: ExprTag::BinExpr {
                    op,
                    lhs: Box::new(expr),
                    rhs: Box::new(self.additive()?),
                },
            }
        }

        Ok(expr)
    }

    fn additive(&mut self) -> PR<Expr> {
        let mut expr = self.multiplicative();

        while let Some(op) = match_tok!(self, Plus | Minus) {
            expr = Ok(Expr {
                tag: ExprTag::BinExpr {
                    op,
                    lhs: Box::new(expr?),
                    rhs: Box::new(self.multiplicative()?),
                },
            });
        }

        expr
    }

    fn multiplicative(&mut self) -> PR<Expr> {
        let mut expr = self.cast()?;

        while let Some(op) = match_tok!(self, Asterisk | Slash | Mod) {
            expr = Expr {
                tag: ExprTag::BinExpr {
                    op,
                    lhs: Box::new(expr),
                    rhs: Box::new(self.cast()?),
                },
            }
        }

        Ok(expr)
    }

    fn cast(&mut self) -> PR<Expr> {
        if check_tok!(self, LeftParen) {
            if self.is_type_specifier() {
                let type_name = self.type_name()?;
                require_tok!(self, RightParen)?;

                return Ok(Expr {
                    tag: ExprTag::CastExpr {
                        r#type: Box::new(type_name),
                        expr: Box::new(self.cast()?),
                    },
                });
            } else {
                let expr = self.expression();
                require_tok!(self, RightParen)?;

                return expr;
            }
        }

        self.unary()
    }

    fn unary(&mut self) -> PR<Expr> {
        match self.iter.peek().val() {
            Some(Ampersand | Asterisk | Plus | Minus | Tilde | Not) => {
                Ok(Expr {
                    tag: ExprTag::UnExpr {
                        op: self.iter.next().unwrap(),
                        rhs: Box::new(self.cast()?),
                    },
                })
            }
            Some(Sizeof) => {
                self.iter.next();

                if check_tok!(self, LeftParen) {
                    let sizeof_expr = Expr {
                        tag: ExprTag::SizeofType {
                            r#type: Box::new(self.type_name()?),
                        },
                    };
                    require_tok!(self, RightParen)?;
                    Ok(sizeof_expr)
                } else {
                    Ok(Expr {
                        tag: ExprTag::SizeofExpr {
                            expr: Box::new(self.unary()?),
                        },
                    })
                }
            }
            _ => self.postfix(),
        }
    }

    fn postfix(&mut self) -> PR<Expr> {
        let mut expr = self.primary();

        while let Some(tok) = match_tok!(
            self,
            LeftBrace | LeftParen | Dot | Not | ArrowRight | Decrement
        ) {
            expr = match tok.0 {
                // todo: somehow make this transformation with macros
                LeftBrace => {
                    // sugar
                    self.expression()?;
                    require_tok!(self, RightBrace)?;
                    todo!()
                }
                Not => {
                    require_tok!(self, LeftParen)?;
                    if !check_tok!(self, RightParen) {
                        self.delim_token_tree()?;
                        require_tok!(self, RightParen)?;
                    }
                    Ok(Expr {
                        tag: ExprTag::Invocation,
                    })
                }
                LeftParen => self.parse_call(expr?),
                Dot => Ok(Expr {
                    tag: ExprTag::MemberAccess {
                        expr: Box::new(expr?),
                        member: tok,
                    },
                }),
                ArrowRight => {
                    // sugar
                    require_tok!(self, Identifier(_))?;
                    todo!()
                }
                Increment | Decrement => {
                    // sugar
                    todo!()
                }
                _ => panic!(),
            }
        }

        expr
    }

    fn parse_call(&mut self, calle: Expr) -> PR<Expr> {
        if check_tok!(self, RightParen) {
            // Empty args
            return Ok(Expr {
                tag: ExprTag::Call {
                    calle: Box::new(calle),
                    args: Vec::with_capacity(0),
                },
            });
        }

        let expr = Expr {
            tag: ExprTag::Call {
                calle: Box::new(calle),
                args: self.parse_args()?,
            },
        };

        require_tok!(self, RightParen)?;
        Ok(expr)
    }

    fn parse_args(&mut self) -> PR<Vec<Expr>> {
        let mut args = Vec::new();
        args.push(self.assignment()?);

        while check_tok!(self, Comma) {
            args.push(self.assignment()?);
        }

        Ok(args)
    }

    fn primary(&mut self) -> PR<Expr> {
        match self.iter.peek().val() {
            Some(
                Identifier(_)
                | StringLiteral
                | CharLiteral
                | NumberLiteral { .. },
            ) => Ok(Expr {
                tag: ExprTag::Primary(self.iter.next().unwrap()),
            }),
            Some(LeftParen) => {
                self.iter.next();
                let expr = self.expression();
                require_tok!(self, RightParen)?;
                expr
            }
            Some(t) => Err((
                ParseErrorTag::UnexpectedToken(t),
                self.iter.peek().unwrap().1,
            )),
            _ => panic!(),
        }
    }
}
