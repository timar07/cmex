use std::str::Chars;

/// Iterator over the string used due the lexing phase.
pub struct Cursor<'src> {
    src: Chars<'src>,
    current: Option<char>,
    pos: usize
}

impl<'a> Cursor<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            src: src.chars(),
            current: None,
            pos: 0
        }
    }

    pub fn next(&mut self) -> Option<char> {
        let c = self.current;
        self.current = self.src.next();
        self.pos += 1;
        c.or_else(|| self.current)
    }

    pub fn match_ch(&mut self, ch: char) -> bool {
        if self.peek() == Some(ch) {
            self.src.next();
            true
        } else {
            false
        }
    }

    pub fn peek(&mut self) -> Option<char> {
        self.current.or_else(|| self.next())
    }

    pub fn lookahead(&mut self, n: usize) -> Option<char> {
        self.src.clone().skip(n - 1).next()
    }

    pub fn take_while<P>(&mut self, mut predicate: P) -> String
    where
        P: FnMut(char) -> bool
    {
        let mut lexeme = String::new();

        while self.peek().is_some() && predicate(self.peek().unwrap()) {
            if let Some(c) = self.next() {
                lexeme.push(c)
            }
        }

        lexeme
    }
}
