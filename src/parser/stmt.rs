///! This file implements ANSI C language parser.
///! For more information about grammar, see
///! <https://www.lysator.liu.se/c/ANSI-C-grammar-y.html>

use crate::ast::{Stmt, StmtTag};
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
            require_tok!($self, RightRight);
            inner
        }
    };
}

impl<'a> Parser<'a> {
    pub fn statement(&mut self) {
        todo!();
    }

    fn declaration(&mut self) -> Stmt {
        let spec = self.declaration_specifiers();

        if check_tok!(self, Semicolon) {
            return Stmt {
                tag: StmtTag::Declaration
            }
        }

        let decl_list = self.init_declarator_list();

        return Stmt {
            tag: StmtTag::Declaration
        }
    }

    fn declaration_specifiers(&mut self) -> Stmt {
        todo!()
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
                panic!();
            }

            return todo!();
        }

        // Struct/Union has a body
        curly_wrapped!(self, {
            self.struct_declarator_list();
        });

        todo!()
    }

    fn struct_declarator_list(&mut self) -> Stmt {
        todo!()
    }

    fn struct_declarator(&mut self) -> Stmt {
        todo!()
    }

    fn enum_specifier(&mut self) -> Stmt {
        require_tok!(self, Enum);

        let maybe_id = match_tok!(self, Identifier);

        if !matches!(self.iter.peek(), Some(LeftCurly)) {
            if maybe_id.is_none() {
                panic!();
            }

            return todo!();
        }

        curly_wrapped!(self, {
            self.enumerator_list();
        });

        todo!()
    }

    fn enumerator_list(&mut self) -> Stmt {
        self.enumerator();

        while check_tok!(self, Comma) {
            self.enumerator();
        }

        todo!();
    }

    fn enumerator(&mut self) -> Stmt {
        require_tok!(self, Identifier);

        if check_tok!(self, Assign) {
            self.constant_expression();
            return todo!();
        }

        todo!()
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

