//! This file implements ANSI C language parser.
//! For more information about grammar, see
//! <https://www.lysator.liu.se/c/ANSI-C-grammar-y.html>
//! <https://github.com/antlr/grammars-v3/blob/master/ANSI-C/C.g>

use super::{ParseError, ParseErrorTag, Parser, PR};
use crate::{check_tok, lookahead, match_tok, require_tok};
use cmex_ast::token::{Token, TokenTag::*};
use cmex_ast::*;
use cmex_span::{MaybeSpannable, Span, Spannable, Unspan};
use tracing::{debug, instrument};

macro_rules! curly_wrapped {
    ($self:expr, $expr:expr) => {{
        match_tok!($self, LeftCurly);
        let inner = $expr;
        require_tok!($self, RightCurly)?;
        inner
    }};
}

macro_rules! paren_wrapped {
    ($self:expr, $expr:expr) => {{
        match_tok!($self, LeftParen);
        let inner = $expr;
        require_tok!($self, RightParen)?;
        inner
    }};
}

impl Parser<'_> {
    pub fn parse(&mut self) -> Result<TranslationUnit, Vec<ParseError>> {
        self.translation_unit()
    }

    #[instrument(skip_all)]
    fn translation_unit(&mut self) -> Result<TranslationUnit, Vec<ParseError>> {
        let mut decls = Vec::new();
        let mut errors = Vec::new();

        while self.iter.peek().is_some() {
            match self.external_decl() {
                Ok(mut decl) => {
                    decls.append(&mut decl);
                }
                Err(err) => {
                    while !check_tok!(self, Comma | Semicolon | RightCurly) {
                        self.iter.next();
                    }

                    debug!("Synchronize parser");

                    errors.push(err);
                }
            }
        }

        if errors.is_empty() {
            Ok(TranslationUnit(decls))
        } else {
            Err(errors)
        }
    }

    /// External declaration is a top level declaration (e.g. functions)
    /// or a regular declaration
    #[instrument(skip_all)]
    pub(crate) fn external_decl(&mut self) -> PR<Vec<DeclTag>> {
        let mut decl_list = Vec::with_capacity(1);

        if let Some(Interpolated(nt)) = self.iter.peek().val() {
            match *nt {
                Nonterminal::Item(decl) => return Ok(decl),
                _ => {
                    return Err((
                        ParseErrorTag::InterpolationFailed(*nt),
                        self.iter.next().span().unwrap(),
                    ))
                }
            }
        }

        match self.iter.peek().val() {
            Some(MacroRules) => {
                decl_list.push(self.macro_rules_definition()?);
                return Ok(decl_list);
            }
            Some(Hash) => return Ok(vec![self.deprecated_macro()?]),
            _ => {}
        }

        if let Some(tok) = match_tok!(self, Typedef) {
            return self
                .maybe_decl_specifiers()?
                .map(|specs| {
                    if let Some(decl) = self.type_definition(specs.as_ref())? {
                        decl_list.push(decl);
                        Ok(decl_list)
                    } else {
                        Err((
                            ParseErrorTag::Expected("type definition".into()),
                            specs.span().unwrap_or(tok.1),
                        ))
                    }
                })
                .unwrap_or_else(|| {
                    Err((
                        ParseErrorTag::Expected(
                            "declaration specifiers".into(),
                        ),
                        tok.1,
                    ))
                });
        }

        let spec = self.maybe_decl_specifiers()?;

        // TODO: hotfix, refactor
        if check_tok!(self, Semicolon) {
            if let Some(ref spec) = spec {
                if let Some(decl) = self.type_definition(spec)? {
                    decl_list.push(decl);
                    return Ok(decl_list);
                }
            }
        }

        while !matches!(self.iter.peek().val(), Some(Semicolon) | None) {
            let decl = self.init_declarator()?;

            // Declator has an initializer e.g. `int foo = bar, ...`
            if decl.1.is_some() {
                let decl_spec = spec.clone().map(Ok).unwrap_or(Err((
                    ParseErrorTag::Expected("declaration specifiers".into()),
                    decl.span(),
                )))?;

                // Push this declaration just like it was seperate declaration
                // e.g. `int a = 5, b = 7;` would be `int a = 5; int b = 7;`
                decl_list.push(self.decl(decl_spec, decl.clone())?);
            }

            // A function definition
            if matches!(self.iter.peek().val(), Some(LeftCurly)) {
                // There was a declarations before. So you can't
                // write `int foo = 5, bar() { return 0 }` because
                // function is a top-level declaration in grammar
                if !decl_list.is_empty() {
                    return Err((
                        ParseErrorTag::Expected(
                            "`;` after top level declaration".into(),
                        ),
                        decl_list.span().unwrap(),
                    ));
                }

                decl_list.push(self.function_definition(spec, decl)?);
                return Ok(decl_list);
            }

            // Declarations listed `int foo = 5, bar = 7, ...`
            if !check_tok!(self, Comma) {
                break;
            }
        }

        require_tok!(self, Semicolon)?;
        Ok(decl_list)
    }

    #[instrument(skip_all)]
    fn type_definition(
        &mut self,
        spec: &[DeclSpecifier],
    ) -> PR<Option<DeclTag>> {
        if let Some(DeclSpecifier::TypeSpecifier(t)) = spec.last() {
            match t {
                TypeSpecifier::Enum(id, consts) => {
                    for decl in consts {
                        if let Identifier(name) = &decl.id.0 {
                            self.symbols
                                .define(name.clone(), decl.id.1)
                                .map_err(|_| {
                                    (
                                        ParseErrorTag::NameAlreadyDefined(
                                            name.clone(),
                                        ),
                                        decl.id.1,
                                    )
                                })?;
                        }
                    }

                    Ok(Some(DeclTag::Enum(id.clone(), consts.to_vec())))
                }
                TypeSpecifier::Record(id, r) => {
                    Ok(Some(DeclTag::Record(id.clone(), r.to_vec())))
                }
                _ => Ok(None),
            }
        } else {
            Ok(None)
        }
    }

    #[instrument(skip_all)]
    fn function_definition(
        &mut self,
        spec: Option<Vec<DeclSpecifier>>,
        decl: InitDeclarator,
    ) -> PR<DeclTag> {
        match decl.0.suffix {
            Some(DeclaratorSuffix::Func(_)) => Ok(DeclTag::Func {
                spec,
                decl: Box::new(decl.0.clone()),
                body: Box::new(self.compound_statement()?),
            }),
            // Function has a array suffix e.g. `int foo[100]() { ... }`
            Some(DeclaratorSuffix::Array(suffix)) => Err((
                ParseErrorTag::UnexpectedDeclarationSuffix,
                suffix.unwrap().span(),
            )),
            _ => Err((
                ParseErrorTag::Expected("parameter list".into()),
                self.iter.peek().span().unwrap(),
            )),
        }
    }

    #[instrument(skip_all)]
    pub fn statement(&mut self) -> PR<Stmt> {
        // Declarations after statements is a C99 feature, but anyway it's
        // supported here.
        if let Some(spec) = self.maybe_decl_specifiers()? {
            let init_decl = self.init_declarator()?;
            let decl = self.decl(spec, init_decl)?;
            require_tok!(self, Semicolon)?;

            return Ok(Stmt {
                tag: StmtTag::Decl(decl),
            });
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
                    Ok(Stmt {
                        tag: StmtTag::Expr(self.expression_statement()?),
                    })
                }
            }
            Some(Goto | Continue | Break | Return) => self.jump_statement(),
            _ => Ok(Stmt {
                tag: StmtTag::Expr(self.expression_statement()?),
            }),
        }
    }

    #[instrument(skip_all)]
    fn compound_statement(&mut self) -> PR<Stmt> {
        assert_eq!(self.iter.next().val(), Some(LeftCurly));
        self.symbols.enter();

        let mut stmts = Vec::new();

        while !check_tok!(self, RightCurly) {
            stmts.push(self.statement()?);
        }

        self.symbols.leave();

        Ok(Stmt {
            tag: StmtTag::Compound(stmts),
        })
    }

    #[instrument(skip_all)]
    pub fn block(&mut self) -> PR<(Vec<Stmt>, Span)> {
        let (_, open) = require_tok!(self, LeftCurly)?;

        let mut stmts = Vec::new();

        while !matches!(self.iter.peek().val(), Some(RightCurly)) {
            stmts.push(self.statement()?);
        }

        let (_, close) = self.iter.next().unwrap();

        Ok((stmts, Span::join(open, close)))
    }

    #[instrument(skip_all)]
    fn expression_statement(&mut self) -> PR<Option<Expr>> {
        let expr = if !check_tok!(self, Semicolon) {
            Some(self.expression().inspect_err(|_| {
                while !check_tok!(self, Semicolon) {
                    self.iter.next();
                }
            })?)
        } else {
            None
        };

        require_tok!(self, Semicolon)?;
        Ok(expr)
    }

    #[instrument(skip_all)]
    fn iteration_statement(&mut self) -> PR<Stmt> {
        Ok(match self.iter.peek().val() {
            Some(While) => {
                self.iter.next();

                Stmt {
                    tag: StmtTag::While {
                        cond: paren_wrapped!(self, { self.expression()? }),
                        stmt: Box::new(self.statement()?),
                    },
                }
            }
            Some(Do) => {
                self.iter.next();
                let stmt = Box::new(self.statement()?);
                require_tok!(self, While)?;
                let cond = paren_wrapped!(self, { self.expression()? });
                require_tok!(self, Semicolon)?;

                Stmt {
                    tag: StmtTag::Do { cond, stmt },
                }
            }
            Some(For) => {
                self.iter.next();
                let header = paren_wrapped!(self, {
                    (
                        self.expression_statement()?,
                        self.expression_statement()?,
                        if !check_tok!(self, RightParen) {
                            Some(self.expression()?)
                        } else {
                            None
                        },
                    )
                });

                Stmt {
                    tag: StmtTag::For(
                        header.0,
                        header.1,
                        header.2,
                        Box::new(self.statement()?),
                    ),
                }
            }
            _ => unreachable!(),
        })
    }

    #[instrument(skip_all)]
    fn selection_statement(&mut self) -> PR<Stmt> {
        match self.iter.peek().val() {
            Some(If) => {
                self.iter.next();
                let cond = paren_wrapped!(self, { self.expression()? });
                let if_stmt = Box::new(self.statement()?);
                let else_stmt = if check_tok!(self, Else) {
                    Some(Box::new(self.statement()?))
                } else {
                    None
                };

                Ok(Stmt {
                    tag: StmtTag::If(cond, if_stmt, else_stmt),
                })
            }
            Some(Switch) => {
                self.iter.next();
                Ok(Stmt {
                    tag: StmtTag::Switch(
                        paren_wrapped!(self, { self.expression()? }),
                        Box::new(self.statement()?),
                    ),
                })
            }
            _ => unreachable!(),
        }
    }

    #[instrument(skip_all)]
    fn labeled_statement(&mut self) -> PR<Stmt> {
        Ok(match self.iter.peek().val() {
            Some(Identifier(_)) => {
                let id = self.iter.next().unwrap();
                require_tok!(self, Colon)?;
                Stmt {
                    tag: StmtTag::Label(id, Box::new(self.statement()?)),
                }
            }
            Some(Case) => {
                self.iter.next();
                let expr = self.constant_expression()?;
                require_tok!(self, Colon)?;
                Stmt {
                    tag: StmtTag::Case(expr, Box::new(self.statement()?)),
                }
            }
            Some(Default) => {
                self.iter.next();
                require_tok!(self, Colon)?;
                Stmt {
                    tag: StmtTag::Default(Box::new(self.statement()?)),
                }
            }
            _ => unreachable!(),
        })
    }

    #[instrument(skip_all)]
    fn jump_statement(&mut self) -> PR<Stmt> {
        match self.iter.peek().val() {
            Some(Goto) => {
                self.iter.next();
                let id = require_tok!(self, Identifier(_))?;
                require_tok!(self, Semicolon)?;
                Ok(Stmt {
                    tag: StmtTag::Goto(id),
                })
            }
            Some(Continue) => {
                self.iter.next();
                require_tok!(self, Semicolon)?;
                Ok(Stmt {
                    tag: StmtTag::Continue,
                })
            }
            Some(Break) => {
                self.iter.next();
                require_tok!(self, Semicolon)?;
                Ok(Stmt {
                    tag: StmtTag::Break,
                })
            }
            Some(Return) => {
                let token = self.iter.next().unwrap();

                let expr = if !check_tok!(self, Semicolon) {
                    Some(self.expression()?)
                } else {
                    None
                };

                require_tok!(self, Semicolon)?;
                Ok(Stmt {
                    tag: StmtTag::Return(token, expr),
                })
            }
            _ => unreachable!(),
        }
    }

    #[instrument(skip_all)]
    fn decl(
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

    fn maybe_decl_specifiers(&mut self) -> PR<Option<Vec<DeclSpecifier>>> {
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
        self.iter
            .peek()
            .is_some_and(|t| matches!(t.0, Extern | Static | Auto | Register))
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
            Some(Identifier(_)) => false, // TODO: check
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
    fn init_declarator(&mut self) -> PR<InitDeclarator> {
        Ok(InitDeclarator(
            self.declarator()?,
            if check_tok!(self, Assign) {
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
            curly_wrapped!(self, { self.struct_decl_list()? }),
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
            .iter()
            .map(|decl| FieldDecl {
                specs: specs.clone(),
                decl: decl.clone(),
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
            curly_wrapped!(self, { self.enumerator_list()? }),
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
            Some(LeftParen) => Ok(paren_wrapped!(self, {
                DirectDeclarator::Paren(self.declarator()?)
            })),
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

                paren_wrapped!(self, {
                    Ok(DeclaratorSuffix::Func(Some(
                        self.parameter_type_list()?,
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
