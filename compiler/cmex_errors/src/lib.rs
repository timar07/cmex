mod display;
mod emitter;

use cmex_source::Source;
use cmex_span::Span;
use colored::Colorize;
use display::LineFormat;

pub use emitter::ErrorEmitter;

#[derive(Clone)]
pub struct ErrorBuilder<'a> {
    fname: Option<String>,
    tag: Option<&'static str>,
    info: Option<String>,
    context: Option<String>,
    src: Source<'a>,
}

impl<'a> ErrorBuilder<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            src: Source::from(src),
            fname: None,
            tag: None,
            info: None,
            context: None,
        }
    }

    pub fn filename(mut self, fname: String) -> Self {
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

    pub fn context(mut self, span: Span) -> Self {
        let start = self
            .src
            .get_line_containing_index(span.0)
            .unwrap_or_else(|| panic!("unexisting source index {}", span.0));
        let end = self
            .src
            .get_line_containing_index(span.1)
            .unwrap_or_else(|| start.clone());

        let snippet = (start.0..=end.0)
            .map(|index| {
                LineFormat::new(
                    index + 1,
                    &self.src.get_line_contents(index).unwrap(),
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
