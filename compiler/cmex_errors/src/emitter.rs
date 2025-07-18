use std::fmt::Display;

use cmex_source::Source;
use cmex_span::{Spannable, Spanned};

use crate::ErrorBuilder;

pub struct ErrorEmitter<'a> {
    builder: ErrorBuilder<'a>,
    src: Source<'a>,
}

impl<'a> ErrorEmitter<'a> {
    pub fn new(src: &'a str, builder: ErrorBuilder<'a>) -> Self {
        Self {
            builder,
            src: Source::from(src),
        }
    }

    pub fn emit<T>(&self, error: &Spanned<T>)
    where
        T: Display,
    {
        eprintln!(
            "{}",
            self.builder
                .clone()
                .context(error.span(), &self.src)
                .info(error.0.to_string())
                .build()
        );
    }
}
