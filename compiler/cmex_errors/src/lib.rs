use std::str::Chars;

use cmex_span::Span;

#[derive(Default)]
pub struct ErrorBuilder {
    tag: Option<&'static str>,
    info: Option<String>,
    context: Option<String>
}

impl ErrorBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn info(mut self, info: String) -> Self {
        self.info = Some(info);
        self
    }

    pub fn tag(mut self, tag: &'static str) -> Self {
        self.tag = Some(tag);
        self
    }

    pub fn context(mut self, src: Chars<'_>, span: Span) -> Self {
        self.context = Some(
            src
                .skip(span.0)
                .take(span.1 - span.0)
                .collect()
        );
        self
    }

    pub fn build(self) -> String {
        format!(
            "\x1b[0;31m{}:\x1b[0m {}\n{}",
            self.tag.unwrap_or("Error".into()),
            self.info.unwrap_or("".into()),
            self.context.unwrap_or("".into())
        )
    }
}
