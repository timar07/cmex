use crate::tree_builder::TreeBuilder;
use crate::*;

/// Implements AST dumping in clang compiler style
/// style i.e. `clang -Xclang -ast-dump <...>`
pub struct AstDumper {
    pub tree_builder: TreeBuilder,
}

impl AstDumper {
    pub fn new(entry: &dyn AstNodeDump) -> Self {
        let mut tree_builder = TreeBuilder::new();
        entry.dump(&mut tree_builder);

        Self { tree_builder }
    }
}

impl std::fmt::Display for AstDumper {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.tree_builder.build().unwrap())
    }
}

/// Helper trait that defines interface for AST node
/// dumping.
pub trait AstNodeDump {
    fn dump(&self, tb: &mut TreeBuilder);
}

impl AstNodeDump for TranslationUnit {
    fn dump(&self, tb: &mut TreeBuilder) {
        tb.open("TranslationUnit".into());

        for decl in &self.0 {
            decl.dump(tb);
        }

        tb.close();
    }
}

impl AstNodeDump for Item {
    fn dump(&self, tb: &mut TreeBuilder) {
        match self {
            Item::Invocation(_) => {
                tb.append_leaf("Invocation".into());
            }
            Item::Decl(decl_tag) => {
                decl_tag.dump(tb);
            }
        }
    }
}

impl AstNodeDump for StmtTag {
    fn dump(&self, tb: &mut TreeBuilder) {
        match self {
            StmtTag::Expr(expr) => {
                tb.open("ExprStmt".into());

                if let Some(expr) = expr {
                    expr.dump(tb);
                }

                tb.close();
            }
            StmtTag::Compound(stmts) => {
                tb.open("CompoundStmt".into());
                stmts.iter().for_each(|stmt| stmt.tag.dump(tb));
                tb.close();
            }
            StmtTag::Decl(_) => {
                tb.append_leaf("DeclStmt".into());
            }
            StmtTag::While { cond, stmt } => {
                tb.open("WhileStmt".into());
                cond.dump(tb);
                stmt.tag.dump(tb);
                tb.close();
            }
            StmtTag::Do { cond, stmt } => {
                tb.open("DoStmt".into());
                cond.dump(tb);
                stmt.tag.dump(tb);
                tb.close();
            }
            StmtTag::For(expr0, expr1, expr2, stmt) => {
                tb.open("ForStmt".into());

                for expr in [expr0, expr1, expr2].into_iter().flatten() {
                    expr.dump(tb);
                }

                stmt.tag.dump(tb);
                tb.close();
            }
            StmtTag::If(expr, if_true, otherwise) => {
                tb.open("IfStmt".into());
                expr.dump(tb);
                if_true.tag.dump(tb);

                if let Some(stmt) = otherwise {
                    stmt.tag.dump(tb);
                }

                tb.close();
            }
            StmtTag::Switch(expr, stmt) => {
                tb.open("SwitchStmt".into());
                expr.dump(tb);
                stmt.tag.dump(tb);
                tb.close();
            }
            StmtTag::Case(expr, stmt) => {
                tb.open("CaseStmt".into());
                expr.dump(tb);
                stmt.tag.dump(tb);
                tb.close();
            }
            StmtTag::Label(_, stmt) => {
                tb.open("LabelStmt".into());
                stmt.tag.dump(tb);
                tb.close();
            }
            StmtTag::Default(stmt) => {
                tb.open("DefaultStmt".into());
                stmt.tag.dump(tb);
                tb.close();
            }
            StmtTag::Break(_) => {
                tb.append_leaf("BreakStmt".into());
            }
            StmtTag::Continue(_) => {
                tb.append_leaf("ContinueStmt".into());
            }
            StmtTag::Return(_, expr) => {
                tb.open("ReturnStmt".into());
                if let Some(expr) = expr {
                    expr.dump(tb);
                }
                tb.close();
            }
            StmtTag::Goto(_) => {
                tb.append_leaf("Goto".into());
            }
        };
    }
}

impl AstNodeDump for DeclTag {
    fn dump(&self, tb: &mut TreeBuilder) {
        match self {
            DeclTag::Include { path, .. } => {
                tb.append_leaf(format!("Include `{path}`"));
            }
            DeclTag::Record(_, decls) => {
                tb.open("RecordDecl".into());
                decls.iter().for_each(|decl| decl.dump(tb));
                tb.close();
            }
            DeclTag::Enum(_, decls) => {
                tb.open("EnumDecl".into());
                decls.iter().for_each(|d| d.dump(tb));
                tb.close();
            }
            DeclTag::Func { spec: _, body, .. } => {
                tb.open("FuncDecl".into());

                body.tag.dump(tb);
                tb.close();
            }
            DeclTag::Var { spec: _, decl_list } => {
                decl_list.iter().for_each(|decl| {
                    tb.open("VarDecl".into());
                    decl.dump(tb);
                    tb.close();
                });
            }
            DeclTag::Macro { id, .. } => {
                tb.append_leaf(format!("Macro `{}`", id.0));
            }
            DeclTag::Typedef { .. } => {
                tb.append_leaf("Typedef".to_string());
            }
        }
    }
}

impl AstNodeDump for Initializer {
    fn dump(&self, tb: &mut TreeBuilder) {
        match self {
            Self::Assign(expr) => expr.dump(tb),
            Self::List(init_list) => {
                init_list.iter().for_each(|init| init.dump(tb));
            }
        }
    }
}

impl AstNodeDump for InitDeclarator {
    fn dump(&self, tb: &mut TreeBuilder) {
        if let Some(init) = &self.1 {
            init.dump(tb);
        }
    }
}

impl AstNodeDump for FieldDecl {
    fn dump(&self, tb: &mut TreeBuilder) {
        tb.append_leaf("FieldDecl".into());
    }
}

impl AstNodeDump for EnumConstantDecl {
    fn dump(&self, tb: &mut TreeBuilder) {
        tb.open("EnumConstantDecl".into());

        if let Some(expr) = &self.cexpr {
            expr.dump(tb);
        }

        tb.close();
    }
}

impl AstNodeDump for ParamList {
    fn dump(&self, tb: &mut TreeBuilder) {
        match self {
            Self::Identifier(ids) => ids.iter().for_each(|id| {
                tb.append_leaf(format!("ParamDecl `{}`", id.0));
            }),
            Self::Type(decls) => {
                decls.iter().for_each(|decl| decl.dump(tb));
            }
        }
    }
}

impl AstNodeDump for ParamDecl {
    fn dump(&self, tb: &mut TreeBuilder) {
        tb.append_leaf("ParamDecl".into());
    }
}

impl AstNodeDump for Expr {
    fn dump(&self, tb: &mut TreeBuilder) {
        match self {
            Expr::Paren(expr) => {
                expr.dump(tb);
            }
            Expr::Primary(tok) => {
                tb.append_leaf(format!("PrimaryExpr `{}`", tok.0));
            }
            Expr::BinExpr { op, lhs, rhs } => {
                tb.open(format!("BinaryOperator `{}`", op.0));
                lhs.dump(tb);
                rhs.dump(tb);
                tb.close();
            }
            Expr::UnExpr { op, rhs } => {
                tb.open(format!("UnaryOperator `{}`", op.0));
                rhs.dump(tb);
                tb.close();
            }
            Expr::Call { calle: _, args } => {
                tb.open("CallExpr".into());
                args.iter().for_each(|arg| arg.dump(tb));
                tb.close();
            }
            Expr::MemberAccess { expr: _, member: _ } => {
                tb.append_leaf("MemberExpr".into());
            }
            Expr::SizeofType { r#type: _ } => {
                tb.append_leaf("SizeofType".into());
            }
            Expr::SizeofExpr { expr } => {
                tb.open("SizeofExpr".into());
                expr.dump(tb);
                tb.close();
            }
            Expr::CastExpr { r#type: _, expr } => {
                tb.open("CastExpr".into());
                expr.dump(tb);
                tb.close();
            }
            Expr::Conditional {
                cond,
                then,
                otherwise,
            } => {
                tb.open("ConditionalOperator".into());
                cond.dump(tb);
                then.dump(tb);
                otherwise.dump(tb);
                tb.close();
            }
            Expr::Invocation(_) => {
                tb.append_leaf("Invocation".into());
            }
            Expr::StmtExpr(vec, _) => {
                tb.open("StmtExpr".into());
                vec.iter().for_each(|stmt| stmt.tag.dump(tb));
                tb.close();
            }
        }
    }
}
