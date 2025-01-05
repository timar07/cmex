mod display;

use cmex_source::Source;
use cmex_span::Span;
use colored::Colorize;
use display::LineFormat;

#[derive(Default)]
pub struct ErrorBuilder {
    fname: Option<String>,
    tag: Option<&'static str>,
    info: Option<String>,
    context: Option<String>
}

impl ErrorBuilder {
    pub fn new() -> Self {
        Self::default()
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

    pub fn context(mut self, src: &str, span: Span) -> Self {
        let source = Source::from(src);
        let start = source
            .get_line_containing_index(span.0)
            .unwrap_or_else(|| {
                panic!("unexisting source index {}", span.0)
            });
        let end = source
            .get_line_containing_index(span.1)
            .unwrap_or(start);

        let snippet = (start..=end)
            .map(|index| {
                LineFormat::new(
                    index + 1,
                    &source
                        .get_line_contents(index)
                        .unwrap(),
                    None
                ).to_string()
            })
            .collect();

        self.context = Some(snippet);
        self
    }

    pub fn build(self) -> String {
        format!(
            "{} {} {}\n{}\n",
            self.fname.unwrap_or_default(),
            format!("{}:", self.tag.unwrap_or("Error"))
                .red()
                .bold(),
            self.info.unwrap_or_default(),
            self.context.unwrap_or_default()
        )
    }
}

