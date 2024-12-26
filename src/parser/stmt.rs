///! This file implements ANSI C language parser.
///! For more information about grammar, see
///! <https://www.lysator.liu.se/c/ANSI-C-grammar-y.html>

use crate::ast::*;
use crate::lexer::TokenTag::*;
use crate::lexer::{Token, Unspanable};
use crate::{check_tok, match_tok, require_tok};
use super::{ParseError, Parser};

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

type PR<T> = Result<T, ParseError>;

impl<'a> Parser<'a> {
    pub fn parse(&mut self) {
        self.translation_unit();
    }

    fn translation_unit(&mut self) {
        self.external_declaration();
    }

    fn external_declaration(&mut self) { // todo: functions
        dbg!(self.declaration());
    }

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
            _ => unreachable!()
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
            _ => unreachable!()
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
            _ => unreachable!()
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
            _ => unreachable!()
        }
    }

    fn declaration(&mut self) -> PR<Decl> {
        let spec = self.declaration_specifiers()?;

        if matches!(self.iter.peek().val(), Some(Semicolon)) {
            return Ok(Decl {
                spec,
                decl_list: Vec::with_capacity(0)
            })
        }

        let decl_list = self.init_declarator_list()?;
        require_tok!(self, Semicolon);

        Ok(Decl {
            spec,
            decl_list
        })
    }

    fn declaration_specifiers(&mut self) -> PR<Vec<DeclSpecifier>> {
        let mut specs = Vec::new();

        while let Some(spec) = self.maybe_parse_declaration_secifier()? {
            specs.push(spec)
        }

        Ok(specs)
    }

    fn maybe_parse_declaration_secifier(&mut self) -> PR<Option<DeclSpecifier>> {
        Ok(self.maybe_parse_type_specifier()?
            .and_then(|spec| Some(DeclSpecifier::TypeSpecifier(spec)))
            .or_else(|| {
                self.maybe_parse_type_qualifier()
                    .and_then(|qual| Some(DeclSpecifier::TypeQualifier(qual)))
            })
        )
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

    fn maybe_parse_type_specifier(&mut self) -> PR<Option<TypeSpecifier>> {
        if self.is_type_specifier() {
            return Ok(Some(self.type_speficier()?));
        }

        Ok(None)
    }

    fn type_speficier(&mut self) -> PR<TypeSpecifier> {
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
            ) => {
                Ok(TypeSpecifier::Simple(
                    self.iter
                        .next()
                        .unwrap()
                ))
            },
            Some(Struct | Union) => {
                self.struct_or_union();
                todo!()
            },
            Some(Enum) => {
                self.enum_specifier();
                todo!()
            },
            Some(Identifier) if self.is_type_specifier() => {
                Ok(TypeSpecifier::Simple(
                    self.iter
                        .next()
                        .unwrap()
                ))
            },
            _ => Err(ParseError::Expected("type specifier".into()))
        }
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
            ) => true,
            Some(Struct | Union | Enum) => {
                return matches!(
                    self.iter.lookahead(2).val(), // no need to lookahead more
                    Some(Identifier | LeftCurly)
                )
            },
            Some(Identifier) => false, // TODO: check
            _ => false
        }
    }

    fn init_declarator_list(&mut self) -> PR<Vec<InitDeclarator>> {
        let mut init_decl_list = Vec::new();
        init_decl_list.push(self.init_declarator()?);

        while check_tok!(self, Comma) {
            init_decl_list.push(self.init_declarator()?);
        }

        Ok(init_decl_list)
    }

    fn init_declarator(&mut self) -> PR<InitDeclarator> {
        Ok(InitDeclarator(
            self.declarator()?,
            if check_tok!(self, Assign) {
                Some(self.initializer())
            } else {
                None
            }
        ))
    }

    fn struct_or_union(&mut self) -> PR<Stmt> {
        require_tok!(self, Struct | Union);

        let maybe_id = match_tok!(self, Identifier);

        // Bodyless struct/union
        if !matches!(self.iter.peek().val(), Some(LeftCurly)) {
            if maybe_id.is_none() {
                return Err(ParseError::DeclarationHasNoIdentifier);
            }

            return Ok(Stmt {
                tag: StmtTag::DeclStmt(DeclStmt::RecordDecl(vec![]))
            })
        }

        // Struct/Union has a body
        curly_wrapped!(self, {
            self.struct_declarator_list();
        });

        Ok(Stmt {
            tag: StmtTag::DeclStmt(DeclStmt::RecordDecl(
                self.struct_declaration_list()
            ))
        })
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

    fn specifier_qualifier(&mut self) -> Option<()> {
        if !self.maybe_parse_type_qualifier().is_some() {
            self.maybe_parse_type_specifier();
        }
        todo!()
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

    fn enum_specifier(&mut self) -> PR<Stmt> {
        require_tok!(self, Enum);

        let maybe_id = match_tok!(self, Identifier);

        if !matches!(self.iter.peek().val(), Some(LeftCurly)) {
            if maybe_id.is_none() {
                return Err(ParseError::DeclarationHasNoInitializer);
            }

            return Ok(Stmt {
                tag: StmtTag::DeclStmt(DeclStmt::EnumDecl(vec![]))
            });
        }

        Ok(Stmt {
            tag: StmtTag::DeclStmt(DeclStmt::EnumDecl(
                curly_wrapped!(self, {
                    self.enumerator_list()
                })
            ))
        })
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

    fn declarator(&mut self) -> PR<Declarator> {
        if matches!(self.iter.peek().val(), Some(Asterisk)) {
            self.pointer();
        }

        self.direct_declarator()
    }

    fn direct_declarator(&mut self) -> PR<Declarator> {
        let dir_decl = match self.iter.peek().val() {
            Some(Identifier) => {
                DirectDeclarator::Identifier(
                    self.iter.next().unwrap()
                )
            },
            Some(LeftParen) => {
                paren_wrapped!(self, {
                    DirectDeclarator::Paren(
                        self.declarator()?
                    )
                })
            },
            Some(_) => return Err(ParseError::UnexpectedToken(
                self.iter.peek().unwrap()
            )),
            _ => return Err(ParseError::UnexpectedEof)
        };

        Ok(Declarator {
            inner: Box::new(dir_decl),
            suffix: self.maybe_parse_declarator_suffix()?
        })
    }

    fn maybe_parse_declarator_suffix(&mut self) -> PR<Option<DeclaratorSuffix>> {
        if self.is_declarator_suffix() {
            Ok(Some(self.declarator_suffix()?))
        } else {
            Ok(None)
        }
    }

    fn declarator_suffix(&mut self) -> PR<DeclaratorSuffix> {
        match self.iter.next().val() {
            Some(LeftBrace) => {
                if check_tok!(self, RightBrace) {
                    return Ok(DeclaratorSuffix::Array(None))
                }

                let expr = self.constant_expression();
                require_tok!(self, RightBrace);
                Ok(DeclaratorSuffix::Array(Some(expr)))
            }
            Some(LeftParen) => {
                if check_tok!(self, RightParen) {
                    return Ok(DeclaratorSuffix::Func(vec![]));
                }

                if matches!(self.iter.peek().val(), Some(Identifier)) {
                    self.identifier_list();
                    return todo!();
                }

                paren_wrapped!(self, {
                    Ok(DeclaratorSuffix::Func(self.parameter_type_list()?))
                })
            },
            _ => panic!()
        }
    }

    fn is_declarator_suffix(&mut self) -> bool {
        matches!(self.iter.peek().val(), Some(LeftBrace | LeftParen))
    }

    fn pointer(&mut self) -> () {
        require_tok!(self, Asterisk);

        if matches!(self.iter.peek().val(), Some(Const | Volatile)) {
            self.type_qualifier_list();
        }

        if matches!(self.iter.peek().val(), Some(Asterisk)) {
            self.pointer();
        }
    }

    fn type_qualifier_list(&mut self) -> Vec<Token> {
        let mut qualifiers = Vec::new();

        while let Some(tok) = self.maybe_parse_type_qualifier() {
            qualifiers.push(tok);
        }

        qualifiers
    }

    fn maybe_parse_type_qualifier(&mut self) -> Option<Token> {
        if self.is_type_qualifier() {
            return self.iter.next();
        }

        None
    }

    fn is_type_qualifier(&mut self) -> bool {
        matches!(self.iter.peek().val(), Some(Const | Volatile))
    }

    fn parameter_type_list(&mut self) -> PR<Vec<ParamDecl>> {
        let param_list = self.parameter_list();

        if matches!(self.iter.peek().val(), Some(Comma))
            && matches!(self.iter.lookahead(2).val(), Some(Ellipsis))
        {
            self.iter.next(); // ,
            self.iter.next(); // ...
        }

        param_list
    }

    fn parameter_list(&mut self) -> PR<Vec<ParamDecl>> {
        let mut param_list = Vec::new();
        param_list.push(self.parameter_declaration()?);

        while check_tok!(self, Comma) {
            param_list.push(self.parameter_declaration()?);
        }

        Ok(param_list)
    }

    fn parameter_declaration(&mut self) -> PR<ParamDecl> {
        Ok(ParamDecl {
            spec: self.declaration_specifiers()?,
            decl: Box::new(self.declarator()?)
        })
    }

    fn identifier_list(&mut self) -> Vec<Token> {
        let mut id_list = Vec::new();
        id_list.push(require_tok!(self, Identifier));

        while check_tok!(self, Comma) {
            if let Some(id) = match_tok!(self, Identifier) {
                id_list.push(id);
            } else {
                break;
            }
        }

        id_list
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

    fn initializer(&mut self) -> Initializer {
        match self.iter.peek().val() {
            Some(LeftCurly) => {
                self.iter.next();
                let init = self.initializer_list();
                require_tok!(self, RightCurly);
                match_tok!(self, Comma); // dangling comma
                init
            },
            _ => {
                Initializer::Assign(self.assignment())
            }
        }
    }

    fn initializer_list(&mut self) -> Initializer {
        let mut init_list = Vec::new();
        init_list.push(self.initializer());

        while check_tok!(self, Comma) {
            init_list.push(self.initializer());
        }

        Initializer::List(init_list)
    }
}

