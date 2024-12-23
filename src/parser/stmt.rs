///! This file implements ANSI C language parser.
///! For more information about grammar, see
///! <https://www.lysator.liu.se/c/ANSI-C-grammar-y.html>

use crate::ast::{DeclStmt, EnumConstantDecl, Expr, FieldDecl, Stmt, StmtTag};
use crate::lexer::TokenTag::{self, *};
use crate::lexer::Unspanable;
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
        match self.iter.peek().val() {
            Some(
                While
                | Do
                | For
            ) => self.iteration_statement(),
            Some(If | Switch) => self.selection_statement(),
            Some(LeftCurly) => {
                self.compound_statement()
            },
            Some(Case | Default) => self.labeled_statement(),
            Some(Identifier) => {
                if let Some(Colon) = self.iter.lookahead(1).val() {
                    self.labeled_statement()
                } else {
                    Stmt {
                        tag: StmtTag::ExprStmt(self.expression_statement())
                    }
                }
            },
            Some(Goto | Continue | Break | Return) => self.jump_statement(),
            _ => Stmt {
                tag: StmtTag::ExprStmt(self.expression_statement())
            },
        }
    }

    fn compound_statement(&mut self) -> Stmt {
        self.iter.next(); // {

        let mut stmts = Vec::new();

        while !check_tok!(self, RightCurly) {
            stmts.push(self.statement());
        }

        Stmt {
            tag: StmtTag::CompoundStmt(stmts)
        }
    }

    fn expression_statement(&mut self) -> Option<Expr> {
        let expr = if !check_tok!(self, Semicolon) {
            Some(self.expression())
        } else {
            None
        };

        self.iter.peek();

        require_tok!(self, Semicolon);
        expr
    }

    fn iteration_statement(&mut self) -> Stmt {
        match self.iter.peek().val() {
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

    fn selection_statement(&mut self) -> Stmt {
        match self.iter.peek().val() {
            Some(If) => {
                self.iter.next();
                let cond = paren_wrapped!(self, {
                    self.expression()
                });
                let if_stmt = Box::new(self.statement());
                let else_stmt = if check_tok!(self, Else) {
                    Some(Box::new(self.statement()))
                } else {
                    None
                };

                Stmt {
                    tag: StmtTag::IfStmt(cond, if_stmt, else_stmt)
                }
            },
            Some(Switch) => {
                self.iter.next();
                Stmt {
                    tag: StmtTag::SwitchStmt(
                        paren_wrapped!(self, {
                            self.expression()
                        }),
                        Box::new(self.statement())
                    )
                }
            },
            _ => panic!()
        }
    }

    fn labeled_statement(&mut self) -> Stmt {
        match self.iter.peek().val() {
            Some(Identifier) => {
                let id = self.iter.next();
                require_tok!(self, Colon);
                Stmt {
                    tag: StmtTag::LabelStmt(
                        id.unwrap(),
                        Box::new(self.statement())
                    )
                }
            },
            Some(Case) => {
                self.iter.next();
                let expr = self.constant_expression();
                require_tok!(self, Colon);
                Stmt {
                    tag: StmtTag::CaseStmt(
                        expr,
                        Box::new(self.statement())
                    )
                }
            },
            Some(Default) => {
                self.iter.next();
                require_tok!(self, Colon);
                Stmt {
                    tag: StmtTag::DefaultStmt(
                        Box::new(self.statement())
                    )
                }
            },
            _ => panic!()
        }
    }

    fn jump_statement(&mut self) -> Stmt {
        match self.iter.peek().val() {
            Some(Goto) => {
                self.iter.next();
                let id = require_tok!(self, Identifier);
                require_tok!(self, Semicolon);
                Stmt {
                    tag: StmtTag::GotoStmt(id)
                }
            },
            Some(Continue) => {
                self.iter.next();
                require_tok!(self, Semicolon);
                Stmt {
                    tag: StmtTag::ContinueStmt
                }
            },
            Some(Break) => {
                self.iter.next();
                require_tok!(self, Semicolon);
                Stmt {
                    tag: StmtTag::BreakStmt
                }
            },
            Some(Return) => {
                self.iter.next();

                if !check_tok!(self, Semicolon) {
                    self.expression();
                }

                require_tok!(self, Semicolon);
                Stmt {
                    tag: StmtTag::ReturnStmt
                }
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
        self.maybe_parse_type_specifier()
            .expect("expected type specifier");
    }

    fn is_storage_class_specifier(&mut self) -> bool {
        self.iter.peek().is_some_and(|t| {
            matches!(
                t.0,
                Typedef
                | Extern
                | Static
                | Auto
                | Register
            )
        })
    }

    fn maybe_parse_type_specifier(&mut self) -> Option<TokenTag> {
        if self.is_type_specifier() {
            return Some(self.type_speficier());
        }

        None
    }

    fn type_speficier(&mut self) -> TokenTag {
        match self.iter.peek().val() {
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
            _ => panic!()
        };

        todo!()
    }

    fn is_type_specifier(&mut self) -> bool {
        match self.iter.peek().val() {
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
                    self.iter.lookahead(2).val(), // no need to lookahead more
                    Some(Identifier | LeftCurly)
                )
            },
            _ => false
        }
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

        if !matches!(self.iter.peek().val(), Some(LeftCurly)) {
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
        while self.is_specifier_qualifier() {
        }

        return vec![]
    }

    fn struct_declaration(&mut self) {
        self.specifier_qualifier_list();
    }

    fn specifier_qualifier_list(&mut self) {
        self.specifier_qualifier();

        while self.is_specifier_qualifier() {
            self.specifier_qualifier();
        }
    }

    fn specifier_qualifier(&mut self) -> Option<TokenTag> {
        self.maybe_parse_type_qualifier()
            .or_else(|| self.maybe_parse_type_specifier())
    }

    fn is_specifier_qualifier(&mut self) -> bool {
        self.is_type_qualifier() || self.is_type_specifier()
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

        if !matches!(self.iter.peek().val(), Some(LeftCurly)) {
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

        EnumConstantDecl { id, cexpr }
    }

    fn declarator(&mut self) -> Stmt {
        if matches!(self.iter.peek().val(), Some(Asterisk)) {
            self.pointer();
        }

        self.direct_declarator()
    }

    fn direct_declarator(&mut self) -> Stmt {
        match self.iter.peek().val() {
            Some(Identifier) => {
                self.iter.next();
                todo!()
            },
            Some(LeftParen) => {
                return paren_wrapped!(self, {
                    self.declarator();
                    todo!()
                });
            },
            _ => panic!()
        }

        self.declarator_suffix();
    }

    fn declarator_suffix(&mut self) -> () {
        match self.iter.next().val() {
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

                if matches!(self.iter.peek().val(), Some(Identifier)) {
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

        if matches!(self.iter.peek().val(), Some(Const | Volatile)) {
            self.type_qualifier_list();
        }

        self.pointer();
        todo!()
    }

    fn type_qualifier_list(&mut self) -> Vec<TokenTag> {
        let mut qualifiers = Vec::new();

        while let Some(tok) = self.maybe_parse_type_qualifier() {
            qualifiers.push(tok);
        }

        qualifiers
    }

    fn maybe_parse_type_qualifier(&mut self) -> Option<TokenTag> {
        if self.is_type_qualifier() {
            return self.iter.next().val();
        }

        None
    }

    fn is_type_qualifier(&mut self) -> bool {
        matches!(self.iter.peek().val(), Some(Const | Volatile))
    }

    fn parameter_type_list(&mut self) -> Stmt {
        self.parameter_list();

        if matches!(self.iter.peek().val(), Some(Comma))
            && matches!(self.iter.lookahead(2).val(), Some(Ellipsis))
        {
            self.iter.next(); // ,
            self.iter.next(); // ...
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
        self.declaration_specifiers();

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
        if matches!(self.iter.peek().val(), Some(Asterisk)) {
            self.pointer();
            self.maybe_parse_direct_abstract_declarator();
        } else {
            self.direct_abstract_declarator();
        }

        todo!();
    }

    fn maybe_parse_direct_abstract_declarator(&mut self) -> Option<Stmt> {
        if self.is_direct_abstract_declarator() {
            Some(self.direct_abstract_declarator())
        } else {
            None
        }
    }

    fn direct_abstract_declarator(&mut self) -> Stmt {
        if matches!(self.iter.peek().val(), Some(LeftParen)) {
            paren_wrapped!(self, {
                self.abstract_declarator();
            });
        } else {
            self.abstract_declarator_suffix();
        }

        while self.is_abstract_declarator_suffix() {
            self.abstract_declarator_suffix();
        }

        todo!()
    }

    fn is_direct_abstract_declarator(&mut self) -> bool {
        matches!(self.iter.peek().val(), Some( // todo: is it enough to determine?
            LeftParen
            | LeftBrace
        ))
    }

    fn maybe_parse_abstract_declarator_suffix(&mut self) -> Option<Stmt> {
        if self.is_abstract_declarator_suffix() {
            Some(self.abstract_declarator_suffix())
        } else {
            None
        }
    }

    fn abstract_declarator_suffix(&mut self) -> Stmt {
        match self.iter.peek().val() {
            Some(LeftBrace) => {
                self.iter.next();

                if check_tok!(self, RightBrace) {
                    // ...
                }

                self.constant_expression();
                require_tok!(self, RightBrace);
            },
            Some(LeftParen) => {
                self.iter.next();

                if check_tok!(self, RightParen) {
                    // ...
                }

                self.parameter_type_list();
                require_tok!(self, RightParen);
            },
            _ => panic!()
        }

        todo!()
    }

    fn is_abstract_declarator_suffix(&mut self) -> bool {
        matches!(self.iter.peek().val(), Some(
            LeftBrace | LeftParen
        ))
    }

    fn initializer(&mut self) -> Stmt {
        match self.iter.peek().val() {
            Some(LeftCurly) => {
                self.iter.next();
                self.initializer_list();
                match_tok!(self, Comma);
            },
            _ => {
                self.assignment();
            }
        }

        todo!()
    }

    fn initializer_list(&mut self) -> Stmt {
        self.initializer();

        while check_tok!(self, Comma) {
            self.initializer();
        }

        todo!()
    }
}

