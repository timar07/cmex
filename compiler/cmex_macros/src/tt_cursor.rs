use cmex_ast::token::Token;
use cmex_ast::TokenTree;

pub struct TtCursor<'a> {
    iter: TtIter<'a>,
    peeked: Option<Option<Token>>,
}

impl<'a> TtCursor<'a> {
    pub fn new(tree: &'a Vec<TokenTree>) -> Self {
        Self {
            iter: TtIter::new(tree),
            peeked: None,
        }
    }

    #[inline]
    pub fn next_tree(&mut self) -> Option<TokenTree> {
        self.iter.next_tree()
    }

    #[inline]
    pub fn peek_tree(&mut self) -> Option<TokenTree> {
        self.iter.peek_tree().cloned()
    }

    #[inline]
    #[allow(dead_code)]
    pub fn peek(&mut self) -> Option<Token> {
        let iter = &mut self.iter;
        self.peeked.get_or_insert_with(|| iter.next()).clone()
    }
}

impl Iterator for TtCursor<'_> {
    type Item = Token;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        match self.peeked.take() {
            Some(v) => v,
            None => self.iter.next(),
        }
    }
}

/// A helper structure to traverse [`TokenTree`] as a flat token stream.
struct TtIter<'a> {
    stack: Vec<TtIter<'a>>,
    tt: &'a Vec<TokenTree>,
    curr: usize,
}

impl<'a> TtIter<'a> {
    pub fn new(tree: &'a Vec<TokenTree>) -> Self {
        TtIter {
            stack: Vec::new(),
            tt: tree,
            curr: 0,
        }
    }

    #[inline]
    pub fn next_tree(&mut self) -> Option<TokenTree> {
        self.stack
            .pop()
            .and_then(|iter| iter.peek_tree().cloned())
            .or_else(|| {
                self.peek_tt().cloned().inspect(|_| {
                    self.curr += 1;
                })
            })
    }

    #[inline]
    pub fn peek_tree(&self) -> Option<&TokenTree> {
        self.stack
            .last()
            .map(|iter| iter.peek_tree())
            .unwrap_or_else(|| self.peek_tt())
    }

    #[inline]
    fn peek_tt(&self) -> Option<&TokenTree> {
        self.tt.get(self.curr)
    }
}

impl Iterator for TtIter<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(cursor) = self.stack.last_mut() {
            cursor.next().or_else(|| {
                self.stack.pop();
                self.next()
            })
        } else {
            match &self.tt.get(self.curr) {
                Some(TokenTree::Token(tok)) => {
                    self.curr += 1;
                    Some(tok.clone())
                }
                Some(TokenTree::Delim(_, vec, _)) => {
                    self.stack.push(Self::new(vec));
                    self.curr += 1;
                    self.stack.last_mut()?.next()
                }
                None => None,
            }
        }
    }
}
