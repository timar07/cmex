use std::{collections::HashMap, hash::Hash};

type Idx = usize;

pub struct SymTable<T, V> {
    index: Idx,
    scopes: Vec<Scope<T, V>>
}

impl<T, V> Default for SymTable<T, V> {
    fn default() -> Self {
        Self {
            index: Default::default(),
            scopes: Default::default()
        }
    }
}

impl<T: Hash + Eq, V: Clone> SymTable<T, V> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn define(&mut self, name: T, span: V) -> Result<(), SymbolError> {
        self.current_mut().define(name, span)
    }

    pub fn lookup(&self, name: &T) -> Option<V> {
        self.lookup_from(self.index, name)
    }

    fn lookup_from(&self, index: Idx, name: &T) -> Option<V> {
        if let Some(span) = self.current().get(name) {
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
            .unwrap_or_default()
    }

    fn current_mut(&mut self) -> &mut Scope<T, V> {
        &mut self.scopes[self.index]
    }

    fn current(&self) -> &Scope<T, V> {
        &self.scopes[self.index]
    }
}

#[derive(Default)]
struct Scope<T, V> {
    #[allow(unused)]
    index: Idx,
    inner: HashMap<T, V>,
    parent: Option<Idx>
}

impl<T: Hash + Eq, V: Clone> Scope<T, V> {
    pub fn new(index: Idx, parent: Option<Idx>) -> Self {
        Self {
            index,
            parent,
            inner: HashMap::default()
        }
    }

    pub fn define(&mut self, name: T, span: V) -> Result<(), SymbolError> {
        self.inner.insert(name, span)
            .map_or(Ok(()), |_| Err(SymbolError::AlreadyDefined))
    }

    pub fn get(&self, name: &T) -> Option<V> {
        self.inner.get(name).cloned()
    }
}

pub enum SymbolError {
    AlreadyDefined
}
