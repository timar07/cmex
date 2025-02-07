use std::collections::VecDeque;
use std::iter::Fuse;

/// Since we parse a LL(k) grammar, we would need more than one peekable token
#[derive(Clone, Debug)]
pub struct Lookahead<I: Iterator> {
    iter: Fuse<I>,
    queue: VecDeque<I::Item>,
}

impl<T: Iterator> From<T> for Lookahead<T> {
    fn from(iter: T) -> Self {
        Self {
            iter: iter.fuse(),
            queue: VecDeque::new(),
        }
    }
}

impl<T: Iterator> Lookahead<T>
where
    T::Item: Clone,
{
    pub fn peek(&mut self) -> Option<T::Item> {
        self.lookahead(0)
    }

    pub fn lookahead(&mut self, n: usize) -> Option<T::Item> {
        let rem = self.queue.len();
        if n >= rem {
            self.queue.extend(self.iter.by_ref().take(n - rem + 1));
        }
        self.queue.get(n).cloned()
    }
}

impl<T> Iterator for Lookahead<T>
where
    T: Iterator,
{
    type Item = T::Item;

    fn next(&mut self) -> Option<Self::Item> {
        self.queue.pop_front().or_else(|| self.iter.next())
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let rem = self.queue.len();
        let (lo, hi) = self.iter.size_hint();
        (lo + rem, hi.map(|n| n + rem))
    }
}

impl<I> ExactSizeIterator for Lookahead<I> where I: ExactSizeIterator {}
