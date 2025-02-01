use std::fmt::Display;

use cmex_source::Source;
use cmex_span::Spannable;

use crate::ErrorBuilder;

pub struct ErrorEmitter<'a> {
    builder: ErrorBuilder,
    src: Source<'a>,
}

impl<'a> ErrorEmitter<'a> {
    pub fn new(src: &'a str, builder: ErrorBuilder) -> Self {
        Self {
            builder,
            src: Source::from(src),
        }
    }

    pub fn emit<T: Display + Spannable>(&self, error: &T) {
        eprintln!(
            "{}",
            self.builder
                .clone()
                .info(error.to_string())
                .context(error.span(), &self.src)
                .build()
        );
    }
}
