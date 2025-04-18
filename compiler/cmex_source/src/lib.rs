use std::cmp::Ordering;

#[derive(Clone)]
pub struct Source<'src> {
    src: &'src str,
    lines: Vec<Line>,
}

impl<'a> From<&'a str> for Source<'a> {
    fn from(src: &'a str) -> Self {
        Self {
            lines: Self::lines_matrix(src),
            src,
        }
    }
}

impl<'a> Source<'a> {
    pub fn get_line_contents(&self, index: usize) -> Option<String> {
        let line = &self.lines[index];
        let start = line.byte_offset;
        let end = line.byte_offset + line.byte_width;

        String::from_utf8(self.src.as_bytes()[start..end].to_vec()).ok()
    }

    /// Convert unicode character index to its line and column numbers
    pub fn get_line_col(&self, index: usize) -> Option<(usize, usize)> {
        self.get_line_containing_index(index)
            .map(|(idx, line)| (idx + 1, line.offset - index))
    }

    /// Get line and its index by its character index in O(log n).
    /// Note that `index` is *not* a byte offset
    pub fn get_line_containing_index(
        &self,
        index: usize,
    ) -> Option<(usize, Line)> {
        self.lines
            .binary_search_by(|line| {
                if line.contains(index) {
                    Ordering::Equal
                } else if index <= line.offset {
                    Ordering::Greater
                } else {
                    Ordering::Less
                }
            })
            .ok()
            .map(|idx| (idx, self.lines[idx].clone()))
    }

    /// TODO: Handle all types of end-of-line sequences
    fn lines_matrix(src: &'a str) -> Vec<Line> {
        let mut lines_matrix = Vec::new();
        let mut offset = 0;
        let mut byte_offset = 0;

        let lines = src.lines();

        for line in lines {
            let width = line.chars().count();
            let byte_width = line.len();

            lines_matrix.push(Line {
                width,
                offset,
                byte_width,
                byte_offset,
            });

            offset += width + 1;
            byte_offset += byte_width + 1;
        }

        lines_matrix
    }
}

#[derive(Debug, Clone)]
pub struct Line {
    pub width: usize,
    pub offset: usize,
    byte_width: usize,
    byte_offset: usize,
}

impl Line {
    /// Check whether a line contains an index
    fn contains(&self, index: usize) -> bool {
        index >= self.offset && index <= self.offset + self.width
    }
}

#[cfg(test)]
mod tests {
    use crate::Source;

    #[test]
    fn basic_lines_peeking() {
        let source = Source::from("hello\nworld\ntest");

        assert_eq!(
            source
                .get_line_contents(
                    source.get_line_containing_index(6).unwrap().0
                )
                .unwrap(),
            "world".to_owned()
        );

        assert_eq!(
            source
                .get_line_contents(
                    source.get_line_containing_index(15).unwrap().0
                )
                .unwrap(),
            "test".to_owned()
        );
    }
}
