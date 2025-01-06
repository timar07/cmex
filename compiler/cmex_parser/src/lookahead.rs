use std::collections::VecDeque;

/// Since we parse a LL(k) grammar,
/// we would need more than one peekable token
#[derive(Clone)]
pub struct Lookahead<T: Iterator> {
    iter: T,
    peeked: Option<T::Item>,
    queue: VecDeque<T::Item>,
}

impl<T: Iterator> From<T> for Lookahead<T> {
    fn from(iter: T) -> Self {
        Self {
            iter,
            peeked: None,
            queue: VecDeque::new(),
        }
    }
}

impl<T: Iterator> Iterator for Lookahead<T>
where
    T::Item: Clone,
{
    type Item = T::Item;

    fn next(&mut self) -> Option<Self::Item> {
        self.peeked
            .clone()
            .inspect(|_| {
                self.peeked = None;
            })
            .or_else(|| self.dequeue())
    }
}

impl<T: Iterator> Lookahead<T>
where
    T::Item: Clone,
{
    pub fn lookahead(&mut self, k: usize) -> Option<T::Item> {
        if k == 0 {
            return self.peek();
        }

        for _ in 0..k - 1 {
            self.enqueue();
        }

        self.enqueue()
    }

    pub fn peek(&mut self) -> Option<T::Item> {
        self.peeked.clone().or_else(|| self.next())
    }

    fn dequeue(&mut self) -> Option<T::Item> {
        self.queue
            .pop_back()
            .or_else(|| self.iter.next())
            .inspect(|item| {
                self.peeked = Some(item.clone());
            })
    }

    fn enqueue(&mut self) -> Option<T::Item> {
        self.queue.push_front(self.iter.next()?);
        self.queue.front().cloned()
    }
}

#[cfg(test)]
mod tests {
    use super::Lookahead;

    #[test]
    fn general_usage_test() {
        let nums = vec![1, 2, 3, 4, 5];
        let mut iter = Lookahead::from(nums.iter());
        assert_eq!(iter.lookahead(2), Some(&2));
        assert_eq!(iter.peek(), Some(&1));
        assert_eq!(iter.next(), Some(&1));
        assert_eq!(iter.next(), Some(&2));
        assert_eq!(iter.lookahead(3), Some(&5))
    }
}
