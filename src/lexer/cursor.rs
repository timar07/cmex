use std::str::{CharIndices, Chars};

/// Iterator over the string used due the lexing phase.
pub struct Cursor<'src> {
    src: CharIndices<'src>,
    iter: Chars<'src>,
    current: Option<char>,
    pub pos: usize
}

impl<'a> Cursor<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            src: src.char_indices(),
            iter: src.chars(),
            current: None,
            pos: 0
        }
    }

    pub fn next(&mut self) -> Option<char> {
        let c = self.current;
        self.current = self.iter.next();

        if c.is_some() {
            self.pos += 1;
        }

        c.or(self.current)
    }

    pub fn slice(&self, start: usize, end: usize) -> String {
        dbg!(self.src
            .clone()
            .skip(start)
            .take(end - start)
            .map(|(_, c)| c)
            .collect::<String>())
    }

    pub fn match_ch(&mut self, ch: char) -> bool {
        if self.peek() == Some(ch) {
            self.next();
            true
        } else {
            false
        }
    }

    pub fn peek(&mut self) -> Option<char> {
        self.current.or_else(|| self.next())
    }

    pub fn lookahead(&mut self, n: usize) -> Option<char> {
        self.iter.clone().skip(n - 1).next()
    }

    pub fn take_while<P>(&mut self, mut predicate: P) -> String
    where
        P: FnMut(char) -> bool
    {
        let mut lexeme = String::new();

        while let Some(c) = self.peek(){
            if !predicate(c) {
                break;
            }

            if let Some(c) = self.next() {
                lexeme.push(c)
            }
        }

        lexeme
    }
}
