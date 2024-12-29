///! Implements AST dumping in clang compiler style
///! style i.e. `clang -Xclang -ast-dump <...>`
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

///! Helper trait that defines interface for AST node
///! dumping.
pub trait AstNodeDump {
    fn dump(&self, tb: &mut TreeBuilder) -> ();
}

pub struct TreeBuilder {
    last: Option<TreeItem>,
    level: usize
}

impl TreeBuilder {
    pub fn new() -> Self {
        Self {
            last: None,
            level: 0,
        }
    }

    pub fn build(&self) -> Option<TreeItem> {
        Some(self.last.clone()?)
    }

    pub fn append_leaf(&mut self, header: String) -> &mut Self {
        self.open(header).close()
    }

    pub fn open(&mut self, header: String) -> &mut Self {
        if let Some(last) = &mut self.last {
            TreeBuilder::append_child(
                last,
                TreeItem::new(header),
                self.level
            );
        } else {
            self.last = Some(TreeItem::new(header))
        }

        self.level += 1;
        self
    }

    pub fn close(&mut self) -> &mut Self {
        self.level -= 1;
        self
    }

    fn append_child(parent: &mut TreeItem, entry: TreeItem, level: usize) {
        if level == 1 {
            parent.children.push(entry);
        } else {
            TreeBuilder::append_child(
                parent.children.last_mut().unwrap(),
                entry,
                level - 1
            );
        }
    }
}

#[derive(Clone, Debug)]
pub struct TreeItem {
    pub header: String,
    pub children: Vec<TreeItem>
}

impl TreeItem {
    pub fn new(header: String) -> Self {
        Self {
            header,
            children: Vec::new()
        }
    }

    pub fn is_empty(&self) -> bool {
        self.children.len() == 1
    }
}

impl std::fmt::Display for TreeItem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", TreePrinter::print(self))
    }
}

struct TreePrinter;

impl TreePrinter {
    pub fn print(root: &TreeItem) -> String {
        Self::print_entry(root, "".into(), "")
    }

    fn print_entry(entry: &TreeItem, indent: String, branch: &str) -> String {
        let shift_width = indent.len() + branch.len();

        format!(
            "{indent}{branch}{}\n{}",
            entry.header,
            entry.children
                .iter()
                .enumerate()
                .map(|(n, c)| {
                    if n == entry.children.len() - 1 {
                        Self::print_entry(
                            &c.clone(),
                            format!(
                                "{indent: <shift_width$}",
                            ),
                            "`-"
                        )
                    } else {
                        Self::print_entry(
                            &c.clone(),
                            format!(
                                "{indent: <shift_width$}|",
                            ),
                            "-"
                        )
                    }
                })
                .collect::<String>(),
        )
    }
}
