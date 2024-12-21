///! This file implements ANSI C language parser.
///! For more information about grammar, see
///! <https://www.lysator.liu.se/c/ANSI-C-grammar-y.html>

use crate::ast::{DeclStmt, EnumConstantDecl, Expr, FieldDecl, Stmt, StmtTag};
use crate::lexer::TokenTag::*;
use crate::{check_tok, match_tok, require_tok};
use super::Parser;

macro_rules! curly_wrapped {
    ($self:expr, $expr:expr) => {
        {
            match_tok!($self, LeftCurly);
            let inner = $expr;
            require_tok!($self, RightCurly);
            inner
        }
    };
}

macro_rules! paren_wrapped {
    ($self:expr, $expr:expr) => {
        {
            match_tok!($self, LeftParen);
            let inner = $expr;
            require_tok!($self, RightParen);
            inner
        }
    };
}

impl<'a> Parser<'a> {
    pub fn statement(&mut self) -> Stmt {
        todo!();
    }

    fn expression_statement(&mut self) -> Option<Expr> {
        let expr = if !check_tok!(self, Semicolon) {
            Some(self.expression())
        } else {
            None
        };

        require_tok!(self, Semicolon);
        expr
    }

    fn iteration_statement(&mut self) -> Stmt {
        match self.iter.peek() {
            Some(While) => {
                self.iter.next();

                Stmt {
                    tag: StmtTag::WhileStmt {
                        cond: paren_wrapped!(self, {
                            self.expression()
                        }),
                        stmt: Box::new(self.statement())
                    }
                }
            },
            Some(Do) => {
                self.iter.next();
                let stmt = Box::new(self.statement());
                require_tok!(self, While);
                let cond = paren_wrapped!(self, {
                    self.expression()
                });
                require_tok!(self, Semicolon);

                Stmt {
                    tag: StmtTag::DoStmt {
                        cond,
                        stmt
                    }
                }
            },
            Some(For) => {
                self.iter.next();
                let header = paren_wrapped!(self, {
                    (
                        self.expression_statement(),
                        self.expression_statement(),
                        if !check_tok!(self, RightParen) {
                            Some(self.expression())
                        } else {
                            None
                        }
                    )
                });

                Stmt {
                    tag: StmtTag::ForStmt(
                        header.0,
                        header.1,
                        header.2,
                        Box::new(self.statement())
                    )
                }
            },
            _ => panic!()
        }
    }

    fn jump_statement(&mut self) {
        match self.iter.peek() {
            Some(Goto) => {
                self.iter.next();
                require_tok!(self, Identifier);
                require_tok!(self, Semicolon);
            },
            Some(Continue) => {
                self.iter.next();
                require_tok!(self, Semicolon);
            },
            Some(Break) => {
                self.iter.next();
                require_tok!(self, Semicolon);
            },
            Some(Return) => {
                self.iter.next();

                if !check_tok!(self, Semicolon) {
                    self.expression();
                }

                require_tok!(self, Semicolon);
            },
            _ => panic!()
        }
    }

    fn declaration(&mut self) -> Stmt {
        let spec = self.declaration_specifiers();

        if check_tok!(self, Semicolon) {
            todo!()
        }

        let decl_list = self.init_declarator_list();
        require_tok!(self, Semicolon);
        todo!()
    }

    fn declaration_specifiers(&mut self) -> () {
        if self.type_speficier().is_ok() {
            panic!("Expected type specifier")
        }


    }

    fn is_storage_class_specifier(&mut self) -> bool {
        matches!(self.iter.peek(), Some(
            Typedef
            | Extern
            | Static
            | Auto
            | Register
        ))
    }

    fn is_type_specifier(&mut self) -> bool {
        match self.iter.peek() {
            Some(
                Void
                | Char
                | Short
                | Int
                | Long
                | Float
                | Double
                | Signed
                | Unsigned
                | Identifier // TODO: check
            ) => true,
            Some(Struct | Union | Enum) => {
                return matches!(
                    self.iter.lookahead(2), // no need do lookahead more
                    Some(Identifier | LeftCurly)
                )
            },
            _ => false
        }
    }

    fn type_speficier(&mut self) -> Result<(), ()> {
        match self.iter.peek() {
            Some(
                Void
                | Char
                | Short
                | Int
                | Long
                | Float
                | Double
                | Signed
                | Unsigned
            ) => { self.iter.next(); },
            Some(Struct | Union) => {
                self.struct_or_union();
            },
            Some(Enum) => {
                self.enum_specifier();
            },
            Some(Identifier) => {
                self.iter.next();
            },
            _ => return Err(())
        };

        Ok(())
    }

    fn is_type_qualifier(&mut self) -> bool {
        matches!(self.iter.peek(), Some(Const | Volatile))
    }

    fn init_declarator_list(&mut self) -> Stmt {
        self.init_declarator();

        while check_tok!(self, Comma) {
            self.init_declarator();
        }

        todo!()
    }

    fn init_declarator(&mut self) -> Stmt {
        self.declarator();

        if check_tok!(self, Assign) {
            self.initializer();
        }

        todo!()
    }

    fn struct_or_union(&mut self) -> Stmt {
        require_tok!(self, Struct | Union);

        let maybe_id = match_tok!(self, Identifier);

        if !matches!(self.iter.peek(), Some(LeftCurly)) {
            if maybe_id.is_none() {
                panic!("declaration has no declarator");
            }

            return Stmt {
                tag: StmtTag::DeclStmt(DeclStmt::RecordDecl(vec![]))
            }
        }

        // Struct/Union has a body
        curly_wrapped!(self, {
            self.struct_declarator_list();
        });

        return Stmt {
            tag: StmtTag::DeclStmt(DeclStmt::RecordDecl(
                self.struct_declaration_list()
            ))
        }
    }

    fn struct_declaration_list(&mut self) -> Vec<FieldDecl> {
        if self.is_type_qualifier() || self.is_type_specifier() {
            self.iter.next();

            self.struct_declarator_list();
            todo!()
        }

        return vec![]
    }

    fn struct_declarator_list(&mut self) -> () {
        self.struct_declarator();

        while check_tok!(self, Comma) {
            self.struct_declarator();
        }
    }

    fn struct_declarator(&mut self) -> () {
        if check_tok!(self, Colon) {
            self.constant_expression();
        } else {
            self.declarator();

            if check_tok!(self, Colon) {
                self.constant_expression();
            }
        }
    }

    fn enum_specifier(&mut self) -> Stmt {
        require_tok!(self, Enum);

        let maybe_id = match_tok!(self, Identifier);

        if !matches!(self.iter.peek(), Some(LeftCurly)) {
            if maybe_id.is_none() {
                panic!("declaration has no initializer");
            }

            return Stmt {
                tag: StmtTag::DeclStmt(DeclStmt::EnumDecl(vec![]))
            };
        }

        Stmt {
            tag: StmtTag::DeclStmt(DeclStmt::EnumDecl(
                curly_wrapped!(self, {
                    self.enumerator_list()
                })
            ))
        }
    }

    fn enumerator_list(&mut self) -> Vec<EnumConstantDecl> {
        self.enumerator();

        while check_tok!(self, Comma) {
            self.enumerator();
        }

        todo!();
    }

    fn enumerator(&mut self) -> EnumConstantDecl {
        let id = require_tok!(self, Identifier);
        let cexpr = if check_tok!(self, Assign) {
            Some(self.constant_expression())
        } else {
            None
        };

        EnumConstantDecl {
            id: todo!(),
            cexpr
        }
    }

    fn declarator(&mut self) {
        if matches!(self.iter.peek(), Some(Asterisk)) {
            self.pointer();
        }

        self.direct_declarator();
        todo!()
    }

    fn direct_declarator(&mut self) -> Stmt {
        if let Some(id) = match_tok!(self, Identifier) {
            return todo!()
        }

        if matches!(self.iter.peek(), Some(LeftParen)) {
            return paren_wrapped!(self, {
                self.declarator();
                todo!()
            });
        }

        self.direct_declarator();

        match self.iter.next() {
            Some(LeftBrace) => {
                if check_tok!(self, RightBrace) {
                    return todo!();
                }

                self.constant_expression();
                require_tok!(self, RightBrace);
                todo!()
            }
            Some(LeftParen) => {
                if check_tok!(self, RightParen) {
                    todo!()
                }

                if matches!(self.iter.peek(), Some(Identifier)) {
                    self.identifier_list();
                    return todo!();
                }

                self.parameter_type_list();
                todo!()
            },
            _ => panic!()
        }
    }

    fn pointer(&mut self) -> Stmt {
        require_tok!(self, Asterisk);

        if matches!(self.iter.peek(), Some(Const | Volatile)) {
            self.type_qualifier_list();
        }

        self.pointer();
        todo!()
    }

    fn type_qualifier_list(&mut self) -> Stmt {
        while check_tok!(self, Const | Volatile) {
            todo!()
        }

        todo!()
    }

    fn parameter_type_list(&mut self) -> Stmt {
        self.parameter_list();

        if check_tok!(self, Comma) {
            require_tok!(self, Ellipsis);
        }

        todo!()
    }

    fn parameter_list(&mut self) -> Stmt {
        self.parameter_declaration();

        while check_tok!(self, Comma) {
            self.parameter_declaration();
        }

        todo!()
    }

    fn parameter_declaration(&mut self) -> Stmt {
        todo!()
    }

    fn identifier_list(&mut self) -> Stmt {
        match_tok!(self, Identifier);

        while check_tok!(self, Comma) {
            match_tok!(self, Identifier);
        }

        todo!()
    }

    fn type_name(&mut self) -> Stmt {
        todo!();
    }

    fn abstract_declarator(&mut self) -> Stmt {
        todo!();
    }

    fn direct_abstract_declarator(&mut self) -> Stmt {
        todo!();
    }

    fn initializer(&mut self) -> Stmt {
        todo!();
    }
}

