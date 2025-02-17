use cmex_ast::{
    token::TokenTag::*, DeclSpecifier, DeclTag, DeclaratorSuffix,
    InitDeclarator, Nonterminal, TypeSpecifier,
};
use cmex_span::{MaybeSpannable, Span, Spannable, Unspan};
use tracing::instrument;

use crate::{check_tok, lookahead, match_tok, require_tok, skip_until};
use crate::{ParseErrorTag, Parser, SymbolTag, PR};

impl Parser<'_> {
    /// External declaration is a top level declaration (e.g. functions,
    /// macros, typdefs) or a regular declarations
    #[instrument(skip_all)]
    pub(crate) fn external_decl(&mut self) -> PR<Vec<DeclTag>> {
        let mut decl_list = Vec::with_capacity(1);

        match self.iter.peek().val() {
            Some(Interpolated(nt)) => match *nt {
                Nonterminal::Item(decl) => return Ok(decl),
                _ => {
                    return Err((
                        ParseErrorTag::InterpolationFailed(*nt),
                        self.iter.next().span().unwrap(),
                    ))
                }
            },
            Some(MacroRules) => {
                decl_list.push(self.macro_rules_definition()?);
                return Ok(decl_list);
            }
            Some(Hash) => return Ok(vec![self.deprecated_macro()?]),
            Some(Typedef) => {
                decl_list.push(self.typedef()?);
                return Ok(decl_list);
            }
            Some(Identifier(tok)) if lookahead!(self, 1, Identifier(_)) => {
                let span = self.iter.next().span().unwrap();

                self.external_decl_tail(None, decl_list)
                    .inspect_err(|err| self.errors.emit(err))
                    .ok();

                return Err((ParseErrorTag::UnknownTypeName(tok), span));
            }
            _ => {
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

                self.external_decl_tail(spec, decl_list)
            }
        }
    }

    pub(crate) fn external_decl_tail(
        &mut self,
        spec: Option<Vec<DeclSpecifier>>,
        mut decl_list: Vec<DeclTag>,
    ) -> PR<Vec<DeclTag>> {
        while !matches!(self.iter.peek().val(), Some(Semicolon) | None) {
            let decl = self.init_declarator()?;

            // Declator has an initializer e.g. `int foo = bar, ...`
            if decl.1.is_some() {
                let decl_spec = spec.clone().unwrap_or_else(|| {
                    self.errors.emit(&(
                        ParseErrorTag::Expected(
                            "declaration specifiers".into(),
                        ),
                        decl.span(),
                    ));
                    // Push an `int` specifier as a dummy, I believe this will
                    // help error recovery later
                    vec![DeclSpecifier::TypeQualifier((Int, decl.span()))]
                });

                // Push this declaration just like it was seperate declaration
                // e.g. `int a = 5, b = 7;` would be `int a = 5; int b = 7;`
                decl_list.push(self.decl(decl_spec, decl.clone())?);
            }

            // A function definition
            if matches!(self.iter.peek().val(), Some(LeftCurly)) {
                // There was a declarations before. So you can't
                // write `int foo = 5, bar() { return 0; }` because
                // function is a top-level declaration in grammar
                if !decl_list.is_empty() {
                    return Err((
                        ParseErrorTag::Expected(
                            "`;` after top level declarator".into(),
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

        require_tok!(self, Semicolon).inspect_err(|_| {
            self.iter.next();
        })?;
        Ok(decl_list)
    }

    fn typedef(&mut self) -> PR<DeclTag> {
        let keyword = require_tok!(self, Typedef)?;
        let specs = self.maybe_decl_specifiers()?.ok_or_else(|| {
            skip_until!(self, Semicolon);

            (
                ParseErrorTag::Expected("declaration specifiers".into()),
                keyword.1,
            )
        })?;

        if let Some(decl) = self.type_definition(specs.as_ref())? {
            let name = require_tok!(self, Identifier(_))?;
            self.symbols
                .define(
                    name.0.to_string(),
                    (SymbolTag::Type, Span::join(keyword.1, name.1)),
                )
                .map_err(|_| {
                    (
                        ParseErrorTag::NameAlreadyDefined(name.0.to_string()),
                        name.1,
                    )
                })?;
            require_tok!(self, Semicolon)?;
            Ok(decl)
        } else {
            Err((
                ParseErrorTag::Expected("type definition".into()),
                specs.span().unwrap_or(keyword.1),
            ))
        }
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
                                .define(
                                    name.clone(),
                                    (SymbolTag::Name, decl.id.1),
                                )
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
                TypeSpecifier::TypeName(_) => Ok(None),
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
                decl: Box::new(decl.0),
                body: Box::new(self.compound_statement()?),
            }),
            // Function has a array suffix e.g. `int foo[100]() { ... }`
            Some(DeclaratorSuffix::Array(ref suffix)) => {
                self.errors.emit(&(
                    ParseErrorTag::UnexpectedDeclarationSuffix,
                    suffix.clone().unwrap().span(),
                ));

                Ok(DeclTag::Func {
                    spec,
                    decl: Box::new(decl.0),
                    body: Box::new(self.compound_statement()?),
                })
            }
            _ => {
                let span = self.iter.peek().span().unwrap();

                if !check_tok!(self, Semicolon) {
                    self.compound_statement()
                        .inspect_err(|err| self.errors.emit(err))
                        .ok();
                }

                Err((ParseErrorTag::Expected("parameter list".into()), span))
            }
        }
    }
}
