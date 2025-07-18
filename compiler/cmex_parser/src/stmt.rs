//! This file implements ANSI C language parser.
//! For more information about grammar, see
//! <https://www.lysator.liu.se/c/ANSI-C-grammar-y.html>
//! <https://github.com/antlr/grammars-v3/blob/master/ANSI-C/C.g>

use super::{ParseErrorTag, Parser, PR};
use crate::{
    check_tok, lookahead, match_tok, require_tok, skip_until, SymbolTag,
};
use cmex_ast::token::{Token, TokenTag::*};
use cmex_ast::*;
use cmex_span::{MaybeSpannable, Spannable, Unspan};
use tracing::instrument;

impl Parser<'_> {
    #[instrument(skip_all)]
    pub(crate) fn translation_unit(&mut self) -> TranslationUnit {
        let mut decls = Vec::new();

        while self.iter.peek().is_some() {
            match self.external_decl() {
                Ok(mut decl) => {
                    decls.append(&mut decl);
                }
                Err(err) => {
                    self.errors.emit(&err);
                }
            }
        }

        TranslationUnit(decls)
    }

    #[instrument(skip_all)]
    pub fn statement(&mut self) -> PR<Box<Stmt>> {
        // Declarations after statements is a C99 feature, but anyway it's
        // supported here.
        if let Some(spec) = self.maybe_decl_specifiers()? {
            let init_decl = self.init_declarator()?;
            let decl = self.decl(spec, init_decl)?;
            require_tok!(self, Semicolon)?;

            return Ok(Box::new(Stmt {
                tag: StmtTag::Decl(decl),
            }));
        }

        match self.iter.peek().val() {
            Some(While | Do | For) => self.iteration_statement(),
            Some(If | Switch) => self.selection_statement(),
            Some(Case | Default) => self.labeled_statement(),
            Some(LeftCurly) => self.compound_statement(),
            Some(Identifier(_)) => {
                if lookahead!(self, 1, Colon) {
                    self.labeled_statement()
                } else {
                    Ok(Box::new(Stmt {
                        tag: StmtTag::Expr(self.expression_statement()?),
                    }))
                }
            }
            Some(Goto | Continue | Break | Return) => self.jump_statement(),
            _ => Ok(Box::new(Stmt {
                tag: StmtTag::Expr(self.expression_statement()?),
            })),
        }
    }

    #[instrument(skip_all)]
    pub(crate) fn compound_statement(&mut self) -> PR<Box<Stmt>> {
        assert_eq!(self.iter.next().val(), Some(LeftCurly));
        self.symbols.enter();

        let mut stmts = Vec::new();

        while !check_tok!(self, RightCurly) {
            match self.statement() {
                Ok(stmt) => {
                    stmts.push(*stmt); // FIXME: not good
                }
                Err(err) => self.errors.emit(&err),
            }
        }

        self.symbols.leave();

        Ok(Box::new(Stmt {
            tag: StmtTag::Compound(stmts),
        }))
    }

    #[instrument(skip_all)]
    fn expression_statement(&mut self) -> PR<Option<Expr>> {
        let expr = if !check_tok!(self, Semicolon) {
            Some(
                self.expression()
                    .inspect_err(|_| skip_until!(self, Semicolon))?,
            )
        } else {
            None
        };

        require_tok!(self, Semicolon)
            .map_err(|err| {
                match &expr {
                    Some(Expr::Primary((Identifier(id), span)))
                        if check_tok!(self, Identifier(_)) =>
                    {
                        // If we found two identifiers together, this is
                        // probably meant to be a declaration
                        self.external_decl_tail(None, Vec::new())
                            .inspect_err(|err| self.errors.emit(err))
                            .ok();

                        (
                            ParseErrorTag::UnknownTypeName(id.to_string()),
                            span.to_owned(),
                        )
                    }
                    _ => err,
                }
            })
            .inspect_err(|err| self.errors.emit(err))
            .ok();
        Ok(expr)
    }

    #[instrument(skip_all)]
    fn iteration_statement(&mut self) -> PR<Box<Stmt>> {
        Ok(match self.iter.peek().val() {
            Some(While) => {
                self.iter.next();

                Box::new(Stmt {
                    tag: StmtTag::While {
                        cond: self
                            .paren_wrapped(|parser| parser.expression())?,
                        stmt: self.statement()?,
                    },
                })
            }
            Some(Do) => {
                self.iter.next();
                let stmt = self.statement()?;
                require_tok!(self, While)?;
                let cond = self.paren_wrapped(|parser| parser.expression())?;
                require_tok!(self, Semicolon)?;

                Box::new(Stmt {
                    tag: StmtTag::Do { cond, stmt },
                })
            }
            Some(For) => {
                self.iter.next();
                let header = self.paren_wrapped(|parser| {
                    Ok((
                        parser.expression_statement()?,
                        parser.expression_statement()?,
                        if !check_tok!(parser, RightParen) {
                            Some(parser.expression()?)
                        } else {
                            None
                        },
                    ))
                })?;

                Box::new(Stmt {
                    tag: StmtTag::For(
                        header.0,
                        header.1,
                        header.2,
                        self.statement()?,
                    ),
                })
            }
            _ => unreachable!(),
        })
    }

    #[instrument(skip_all)]
    fn selection_statement(&mut self) -> PR<Box<Stmt>> {
        match self.iter.peek().val() {
            Some(If) => {
                self.iter.next();
                let cond = self.paren_wrapped(|parser| parser.expression())?;
                let if_stmt = self.statement()?;
                let else_stmt = if check_tok!(self, Else) {
                    Some(self.statement()?)
                } else {
                    None
                };

                Ok(Box::new(Stmt {
                    tag: StmtTag::If(cond, if_stmt, else_stmt),
                }))
            }
            Some(Switch) => {
                self.iter.next();
                Ok(Box::new(Stmt {
                    tag: StmtTag::Switch(
                        self.paren_wrapped(|parser| parser.expression())?,
                        self.statement()?,
                    ),
                }))
            }
            _ => unreachable!(),
        }
    }

    #[instrument(skip_all)]
    fn labeled_statement(&mut self) -> PR<Box<Stmt>> {
        Ok(match self.iter.peek().val() {
            Some(Identifier(_)) => {
                let id = self.iter.next().unwrap();
                require_tok!(self, Colon)?;
                Box::new(Stmt {
                    tag: StmtTag::Label(id, self.statement()?),
                })
            }
            Some(Case) => {
                self.iter.next();
                let expr = self.constant_expression()?;
                require_tok!(self, Colon)?;
                Box::new(Stmt {
                    tag: StmtTag::Case(expr, self.statement()?),
                })
            }
            Some(Default) => {
                self.iter.next();
                require_tok!(self, Colon)?;
                Box::new(Stmt {
                    tag: StmtTag::Default(self.statement()?),
                })
            }
            _ => unreachable!(),
        })
    }

    #[instrument(skip_all)]
    fn jump_statement(&mut self) -> PR<Box<Stmt>> {
        match self.iter.peek().val() {
            Some(Goto) => {
                self.iter.next();
                let id = require_tok!(self, Identifier(_))?;
                require_tok!(self, Semicolon)?;
                Ok(Box::new(Stmt {
                    tag: StmtTag::Goto(id),
                }))
            }
            Some(Continue) => {
                let span = self.iter.next().span().unwrap();
                require_tok!(self, Semicolon)?;
                Ok(Box::new(Stmt {
                    tag: StmtTag::Continue(span),
                }))
            }
            Some(Break) => {
                let span = self.iter.next().span().unwrap();
                require_tok!(self, Semicolon)?;
                Ok(Box::new(Stmt {
                    tag: StmtTag::Break(span),
                }))
            }
            Some(Return) => {
                let token = self.iter.next().unwrap();

                let expr = if !check_tok!(self, Semicolon) {
                    Some(self.expression()?)
                } else {
                    None
                };

                require_tok!(self, Semicolon)?;
                Ok(Box::new(Stmt {
                    tag: StmtTag::Return(token, expr),
                }))
            }
            _ => unreachable!(),
        }
    }

    #[instrument(skip_all)]
    pub(crate) fn decl(
        &mut self,
        spec: Vec<DeclSpecifier>,
        init_decl: InitDeclarator,
    ) -> PR<DeclTag> {
        let decl_list = self.init_declarator_list(init_decl)?;

        Ok(DeclTag::Var { spec, decl_list })
    }

    #[instrument(skip_all)]
    fn decl_specifier(&mut self) -> PR<Vec<DeclSpecifier>> {
        let mut specs = Vec::new();

        while let Some(spec) = self.maybe_decl_specifier()? {
            specs.push(spec)
        }

        Ok(specs)
    }

    pub(crate) fn maybe_decl_specifiers(
        &mut self,
    ) -> PR<Option<Vec<DeclSpecifier>>> {
        let mut spec_list = Vec::with_capacity(0);

        while let Some(spec) = self.maybe_decl_specifier()? {
            spec_list.push(spec);
        }

        Ok(if !spec_list.is_empty() {
            Some(spec_list)
        } else {
            None
        })
    }

    fn maybe_decl_specifier(&mut self) -> PR<Option<DeclSpecifier>> {
        Ok(self
            .maybe_type_specifier()?
            .map(DeclSpecifier::TypeSpecifier)
            .or_else(|| {
                self.maybe_type_qualifier()
                    .map(DeclSpecifier::TypeQualifier)
                    .or_else(|| self.maybe_storage_class_specifier())
            }))
    }

    fn maybe_storage_class_specifier(&mut self) -> Option<DeclSpecifier> {
        if self.is_storage_class_specifier() {
            return Some(DeclSpecifier::StorageClass(
                self.iter.next().unwrap(),
            ));
        }

        None
    }

    #[instrument(skip_all)]
    fn is_storage_class_specifier(&mut self) -> bool {
        self.iter.peek().is_some_and(|t| {
            matches!(t.0, Typedef | Extern | Static | Auto | Register)
        })
    }

    fn maybe_type_specifier(&mut self) -> PR<Option<TypeSpecifier>> {
        if self.is_type_specifier() {
            return Ok(Some(self.type_speficier()?));
        }

        Ok(None)
    }

    #[instrument(skip_all)]
    fn type_speficier(&mut self) -> PR<TypeSpecifier> {
        match self.iter.peek().val() {
            Some(
                Void | Char | Short | Int | Long | Float | Double | Signed
                | Unsigned,
            ) => Ok(TypeSpecifier::TypeName(self.iter.next().unwrap())),
            Some(Struct | Union) => self.struct_or_union_specifier(),
            Some(Enum) => self.enum_specifier(),
            Some(Identifier(_)) if self.is_type_specifier() => {
                Ok(TypeSpecifier::TypeName(self.iter.next().unwrap()))
            }
            Some(_) => Err((
                ParseErrorTag::Expected("type specifier".into()),
                self.iter.peek().span().unwrap(),
            )),
            _ => panic!(),
        }
    }

    pub(crate) fn is_type_specifier(&mut self) -> bool {
        match self.iter.peek().val() {
            Some(
                Void | Char | Short | Int | Long | Float | Double | Signed
                | Unsigned,
            ) => true,
            Some(Struct | Union | Enum) => {
                lookahead!(self, 1, Identifier(_) | LeftCurly)
            }
            Some(Identifier(id)) => {
                matches!(self.symbols.lookup(&id), Some((SymbolTag::Type, _)))
            }
            _ => false,
        }
    }

    #[instrument(skip_all)]
    fn init_declarator_list(
        &mut self,
        tail: InitDeclarator,
    ) -> PR<Vec<InitDeclarator>> {
        let mut init_decl_list = Vec::new();
        init_decl_list.push(tail);

        while check_tok!(self, Comma) {
            init_decl_list.push(self.init_declarator()?);
        }

        Ok(init_decl_list)
    }

    #[instrument(skip_all)]
    pub(crate) fn declarator_list(&mut self) -> PR<Vec<Declarator>> {
        let mut decl_list = Vec::new();
        decl_list.push(self.declarator()?);

        while check_tok!(self, Comma) {
            decl_list.push(self.declarator()?)
        }

        Ok(decl_list)
    }

    #[instrument(skip_all)]
    pub(crate) fn init_declarator(&mut self) -> PR<InitDeclarator> {
        Ok(InitDeclarator(
            self.declarator()?,
            if check_tok!(self, Assign) {
                if let Some(tok) = match_tok!(self, Semicolon) {
                    return Err((
                        ParseErrorTag::Expected("initializer".into()),
                        tok.span(),
                    ));
                }

                Some(self.initializer()?)
            } else {
                None
            },
        ))
    }

    #[instrument(skip_all)]
    fn struct_or_union_specifier(&mut self) -> PR<TypeSpecifier> {
        require_tok!(self, Struct | Union)?;

        let maybe_id = match_tok!(self, Identifier(_));

        // Bodyless struct/union
        if !matches!(self.iter.peek().val(), Some(LeftCurly)) {
            if maybe_id.is_none() {
                return Err((
                    ParseErrorTag::DeclarationHasNoIdentifier,
                    self.iter.peek().span().unwrap(),
                ));
            }

            return Ok(TypeSpecifier::Record(maybe_id, vec![]));
        }

        Ok(TypeSpecifier::Record(
            maybe_id,
            self.curly_wrapped(|parser| parser.struct_decl_list())?,
        ))
    }

    #[instrument(skip_all)]
    fn struct_decl_list(&mut self) -> PR<Vec<FieldDecl>> {
        let mut struct_decl_list = Vec::new();
        struct_decl_list.append(&mut self.struct_decl()?);

        while let Some(mut struct_decl) = self.maybe_struct_decl()? {
            struct_decl_list.append(&mut struct_decl);
        }

        Ok(struct_decl_list)
    }

    fn maybe_struct_decl(&mut self) -> PR<Option<Vec<FieldDecl>>> {
        if self.is_struct_decl() {
            Ok(Some(self.struct_decl()?))
        } else {
            Ok(None)
        }
    }

    #[instrument(skip_all)]
    fn struct_decl(&mut self) -> PR<Vec<FieldDecl>> {
        let specs = self.specifier_qualifier_list()?;
        let decl_list = self.struct_declarator_list()?;
        require_tok!(self, Semicolon)?;

        Ok(decl_list
            .into_iter()
            .map(|decl| FieldDecl {
                specs: specs.clone(), // TODO: this is bad
                decl
            })
            .collect())
    }

    fn is_struct_decl(&mut self) -> bool {
        self.is_specifier_qualifier()
    }

    #[instrument(skip_all)]
    fn specifier_qualifier_list(&mut self) -> PR<Vec<TypeSpecifier>> {
        let mut specs = vec![];

        while self.is_specifier_qualifier() {
            if let Some(spec) = self.specifier_qualifier()? {
                specs.push(spec);
            }
        }

        Ok(specs)
    }

    #[instrument(skip_all)]
    fn specifier_qualifier(&mut self) -> PR<Option<TypeSpecifier>> {
        if self.maybe_type_qualifier().is_none() {
            return self.maybe_type_specifier();
        }

        Ok(None)
    }

    fn is_specifier_qualifier(&mut self) -> bool {
        self.is_type_qualifier() || self.is_type_specifier()
    }

    #[instrument(skip_all)]
    fn struct_declarator_list(&mut self) -> PR<Vec<FieldDeclarator>> {
        let mut decl_list = Vec::new();
        decl_list.push(self.struct_declarator()?);

        while check_tok!(self, Comma) {
            decl_list.push(self.struct_declarator()?);
        }

        Ok(decl_list)
    }

    #[instrument(skip_all)]
    fn struct_declarator(&mut self) -> PR<FieldDeclarator> {
        if check_tok!(self, Colon) {
            self.constant_expression()?;
            todo!()
        } else {
            Ok(FieldDeclarator {
                decl: self.declarator()?,
                width: if check_tok!(self, Colon) {
                    Some(self.constant_expression()?)
                } else {
                    None
                },
            })
        }
    }

    #[instrument(skip_all)]
    fn enum_specifier(&mut self) -> PR<TypeSpecifier> {
        require_tok!(self, Enum)?;

        let maybe_id = match_tok!(self, Identifier(_));

        if !matches!(self.iter.peek().val(), Some(LeftCurly)) {
            if maybe_id.is_none() {
                return Err((
                    ParseErrorTag::DeclarationHasNoInitializer,
                    self.iter.peek().span().unwrap(),
                ));
            }

            return Ok(TypeSpecifier::Enum(maybe_id, vec![]));
        }

        Ok(TypeSpecifier::Enum(
            maybe_id,
            self.curly_wrapped(|parser| parser.enumerator_list())?,
        ))
    }

    #[instrument(skip_all)]
    fn enumerator_list(&mut self) -> PR<Vec<EnumConstantDecl>> {
        let mut enum_list = Vec::new();
        enum_list.push(self.enumerator()?);

        while check_tok!(self, Comma) {
            enum_list.push(self.enumerator()?);
        }

        Ok(enum_list)
    }

    #[instrument(skip_all)]
    fn enumerator(&mut self) -> PR<EnumConstantDecl> {
        let id = require_tok!(self, Identifier(_))?;
        let cexpr = if check_tok!(self, Assign) {
            Some(self.constant_expression()?)
        } else {
            None
        };

        Ok(EnumConstantDecl { id, cexpr })
    }

    #[instrument(skip_all)]
    fn declarator(&mut self) -> PR<Declarator> {
        let prefix = self.pointer()?;
        let inner = self.direct_declarator()?;

        Ok(Declarator {
            prefix,
            suffix: if inner.is_abstract() {
                self.maybe_abstract_declarator_suffix()?
            } else {
                self.maybe_declarator_suffix()?
            },
            inner: Box::new(inner),
        })
    }

    #[instrument(skip_all)]
    fn direct_declarator(&mut self) -> PR<DirectDeclarator> {
        match self.iter.peek().val() {
            Some(Identifier(_)) => {
                Ok(DirectDeclarator::Identifier(self.iter.next().unwrap()))
            }
            Some(LeftParen) => Ok(self.paren_wrapped(|parser| {
                Ok(DirectDeclarator::Paren(parser.declarator()?))
            })?),
            Some(_) => Ok(DirectDeclarator::Abstract),
            _ => panic!(),
        }
    }

    fn maybe_declarator_suffix(&mut self) -> PR<Option<DeclaratorSuffix>> {
        if self.is_declarator_suffix() {
            Ok(Some(self.declarator_suffix()?))
        } else {
            Ok(None)
        }
    }

    #[instrument(skip_all)]
    fn declarator_suffix(&mut self) -> PR<DeclaratorSuffix> {
        match self.iter.next().val() {
            Some(LeftBrace) => {
                if check_tok!(self, RightBrace) {
                    return Ok(DeclaratorSuffix::Array(None));
                }

                let expr = self.constant_expression()?;
                require_tok!(self, RightBrace)?;
                Ok(DeclaratorSuffix::Array(Some(expr)))
            }
            Some(LeftParen) => {
                if check_tok!(self, RightParen) {
                    return Ok(DeclaratorSuffix::Func(None));
                }

                #[cfg(feature = "kr_func_decl")]
                if matches!(self.iter.peek().val(), Some(Identifier)) {
                    return Ok(DeclaratorSuffix::Func(Some(
                        ParamList::Identifier(self.identifier_list()),
                    )));
                }

                self.paren_wrapped(|parser| {
                    Ok(DeclaratorSuffix::Func(Some(
                        parser.parameter_type_list()?,
                    )))
                })
            }
            _ => panic!(),
        }
    }

    fn is_declarator_suffix(&mut self) -> bool {
        matches!(self.iter.peek().val(), Some(LeftBrace | LeftParen))
    }

    #[instrument(skip_all)]
    fn pointer(&mut self) -> PR<Vec<DeclaratorPrefix>> {
        let mut prefix = Vec::new();

        while check_tok!(self, Asterisk) {
            if matches!(self.iter.peek().val(), Some(Const | Volatile)) {
                prefix.push(DeclaratorPrefix::Pointer(
                    self.type_qualifier_list(),
                ));
            } else {
                prefix.push(DeclaratorPrefix::Pointer(Vec::with_capacity(0)))
            }
        }

        Ok(prefix)
    }

    #[instrument(skip_all)]
    fn type_qualifier_list(&mut self) -> Vec<Token> {
        let mut qualifiers = Vec::new();

        while let Some(tok) = self.maybe_type_qualifier() {
            qualifiers.push(tok);
        }

        qualifiers
    }

    fn maybe_type_qualifier(&mut self) -> Option<Token> {
        if self.is_type_qualifier() {
            return self.iter.next();
        }

        None
    }

    fn is_type_qualifier(&mut self) -> bool {
        matches!(self.iter.peek().val(), Some(Const | Volatile))
    }

    #[instrument(skip_all)]
    fn parameter_type_list(&mut self) -> PR<ParamList> {
        let param_list = self.parameter_list()?;

        if matches!(self.iter.peek().val(), Some(Comma))
            && lookahead!(self, 2, Ellipsis)
        {
            self.iter.next(); // ,
            self.iter.next(); // ...
        }

        Ok(ParamList::Type(param_list))
    }

    #[instrument(skip_all)]
    fn parameter_list(&mut self) -> PR<Vec<ParamDecl>> {
        let mut param_list = Vec::new();
        param_list.push(self.parameter_decl()?);

        while check_tok!(self, Comma) {
            param_list.push(self.parameter_decl()?);
        }

        Ok(param_list)
    }

    #[instrument(skip_all)]
    fn parameter_decl(&mut self) -> PR<ParamDecl> {
        Ok(ParamDecl {
            spec: self.decl_specifier()?,
            decl: Box::new(self.declarator()?),
        })
    }

    #[allow(unused)]
    #[instrument(skip_all)]
    fn identifier_list(&mut self) -> PR<Vec<Token>> {
        let mut id_list = Vec::new();
        id_list.push(require_tok!(self, Identifier(_))?);

        while check_tok!(self, Comma) {
            if let Some(id) = match_tok!(self, Identifier(_)) {
                id_list.push(id);
            } else {
                break;
            }
        }

        Ok(id_list)
    }

    #[instrument(skip_all)]
    pub(crate) fn type_name(&mut self) -> PR<TypeName> {
        let specs = self.specifier_qualifier_list()?;

        self.declarator().and_then(|decl| {
            if !decl.inner.is_abstract() {
                return Err((
                    ParseErrorTag::Expected("abstract declarator".into()),
                    decl.span(),
                ));
            }
            Ok(TypeName { specs, decl })
        })
    }

    fn maybe_abstract_declarator_suffix(
        &mut self,
    ) -> PR<Option<DeclaratorSuffix>> {
        if self.is_abstract_declarator_suffix() {
            Ok(Some(self.abstract_declarator_suffix()?))
        } else {
            Ok(None)
        }
    }

    #[instrument(skip_all)]
    fn abstract_declarator_suffix(&mut self) -> PR<DeclaratorSuffix> {
        match self.iter.peek().val() {
            Some(LeftBrace) => {
                self.iter.next();

                if check_tok!(self, RightBrace) {
                    return Ok(DeclaratorSuffix::Array(None));
                }

                let expr = self.constant_expression()?;
                require_tok!(self, RightBrace)?;

                Ok(DeclaratorSuffix::Array(Some(expr)))
            }
            Some(LeftParen) => {
                self.iter.next();

                if check_tok!(self, RightParen) {
                    return Ok(DeclaratorSuffix::Func(None));
                }

                let plist = self.parameter_type_list()?;
                require_tok!(self, RightParen)?;

                Ok(DeclaratorSuffix::Func(Some(plist)))
            }
            _ => panic!(),
        }
    }

    fn is_abstract_declarator_suffix(&mut self) -> bool {
        matches!(self.iter.peek().val(), Some(LeftBrace | LeftParen))
    }

    #[instrument(skip_all)]
    fn initializer(&mut self) -> PR<Initializer> {
        match self.iter.peek().val() {
            Some(LeftCurly) => {
                self.iter.next();
                let init = self.initializer_list();
                require_tok!(self, RightCurly)?;
                match_tok!(self, Comma); // dangling comma
                init
            }
            _ => Ok(Initializer::Assign(self.assignment()?)),
        }
    }

    #[instrument(skip_all)]
    fn initializer_list(&mut self) -> PR<Initializer> {
        let mut init_list = Vec::new();
        init_list.push(self.initializer()?);

        while check_tok!(self, Comma) {
            init_list.push(self.initializer()?);
        }

        Ok(Initializer::List(init_list))
    }
}
