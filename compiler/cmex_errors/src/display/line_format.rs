use colored::Colorize;

pub struct LineFormat<'a> {
    number: usize,
    line: &'a String,
    extra: Option<String>,
}

impl<'a> LineFormat<'a> {
    pub fn new(number: usize, line: &'a String, extra: Option<String>) -> Self {
        Self {
            number,
            line,
            extra,
        }
    }
}

impl std::fmt::Display for LineFormat<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let snippet_prefix = format!("{:>6} {} ", self.number, "|");

        write!(
            f,
            "{}{}{}",
            snippet_prefix.dimmed(),
            self.line,
            self.extra
                .as_ref()
                .map(|extra| format!(
                    "\n{}{}",
                    " ".repeat(snippet_prefix.len()),
                    extra
                ))
                .unwrap_or("\n".into())
        )
    }
}
