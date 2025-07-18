mod display;
mod emitter;

use cmex_source::Source;
use cmex_span::Span;
use colored::Colorize;
use display::LineFormat;

pub use emitter::ErrorEmitter;

#[derive(Clone, Default)]
pub struct ErrorBuilder<'a> {
    fname: Option<&'a str>,
    tag: Option<&'static str>,
    info: Option<String>,
    context: Option<String>,
}

impl<'a> ErrorBuilder<'a> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn filename(mut self, fname: &'a str) -> Self {
        self.fname = Some(fname);
        self
    }

    pub fn info(mut self, info: String) -> Self {
        self.info = Some(info);
        self
    }

    pub fn tag(mut self, tag: &'static str) -> Self {
        self.tag = Some(tag);
        self
    }

    pub fn context(mut self, span: Span, src: &Source<'_>) -> Self {
        let start = src
            .get_line_containing_index(span.0)
            .unwrap_or_else(|| panic!("unexisting source index {}", span.0));
        let end = src
            .get_line_containing_index(span.1)
            .unwrap_or_else(|| start.clone());

        let snippet = (start.0..=end.0)
            .map(|index| {
                LineFormat::new(
                    index + 1,
                    &src.get_line_contents(index).unwrap(),
                    None,
                )
                .to_string()
            })
            .collect();

        self.context = Some(snippet);
        self
    }

    pub fn build(self) -> String {
        format!(
            "{} {} {}\n{}\n",
            self.fname.unwrap_or_default(),
            format!("{}:", self.tag.unwrap_or("Error")).red().bold(),
            self.info.unwrap_or_default(),
            self.context.unwrap_or_default()
        )
    }
}
