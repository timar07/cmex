use std::collections::HashMap;
use crate::lexer::Span;

type Idx = usize;

pub struct SymTable {
    index: Idx,
    scopes: Vec<Scope>
}

impl SymTable {
    pub fn new() -> Self {
        Self {
            index: 0,
            scopes: Vec::from([Scope::new(0, None)])
        }
    }

    pub fn define(&mut self, name: String, span: Span) {
        self.current_mut().define(name, span);
    }

    pub fn lookup(&self, name: &String) -> Option<Span> {
        self.lookup_from(self.index, name)
    }

    fn lookup_from(&self, index: Idx, name: &String) -> Option<Span> {
        if let Some(span) = self.current().get(&name) {
            return Some(span);
        }

        self.lookup_from(self.scopes[index].parent?, name)
    }

    pub fn enter(&mut self) {
        self.scopes.push(
            Scope::new(self.scopes.len(), Some(self.index))
        );
    }

    pub fn leave(&mut self) {
        self.index = self.current()
            .parent
            .unwrap_or(0)
    }

    fn current_mut(&mut self) -> &mut Scope {
        &mut self.scopes[self.index]
    }

    fn current(&self) -> &Scope {
        &self.scopes[self.index]
    }
}

pub struct Scope {
    index: Idx,
    inner: HashMap<String, Span>,
    parent: Option<Idx>
}

impl Scope {
    pub fn new(index: Idx, parent: Option<Idx>) -> Self {
        Self {
            index,
            parent,
            inner: HashMap::default()
        }
    }

    pub fn define(&mut self, name: String, span: Span) {
        self.inner.insert(name, span);
    }

    pub fn get(&self, name: &String) -> Option<Span> {
        self.inner.get(name).copied()
    }
}

enum SymbolError {
    AlreadyDefined
}
