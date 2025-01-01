use crate::tree_builder::TreeBuilder;

/// Implements AST dumping in clang compiler style
/// style i.e. `clang -Xclang -ast-dump <...>`
pub struct AstDumper {
    pub tree_builder: TreeBuilder
}

impl AstDumper {
    pub fn new(entry: &dyn AstNodeDump) -> Self {
        let mut tree_builder = TreeBuilder::new();
        entry.dump(&mut tree_builder);

        Self {
            tree_builder
        }
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
    fn dump(&self, tb: &mut TreeBuilder) ;
}

