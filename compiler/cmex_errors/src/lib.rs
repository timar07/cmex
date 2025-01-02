use cmex_source::Source;
use cmex_span::Span;

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
        let line_index = source
            .get_line_containing_index(span.0)
            .unwrap_or_else(|| {
                panic!("unexisting source index {}", span.0)
            });

        self.context = Some(format!(
            "{}",
            source
                .get_line_contents(line_index)
                .unwrap()
        ));
        self
    }

    pub fn build(self) -> String {
        format!(
            "{} \x1b[0;31m{}:\x1b[0m {}\n{}\n",
            self.fname.unwrap_or_default(),
            self.tag.unwrap_or("Error".into()),
            self.info.unwrap_or_default(),
            self.context.unwrap_or_default()
        )
    }
}
