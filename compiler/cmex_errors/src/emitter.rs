use std::fmt::Display;

use cmex_span::Spannable;

use crate::ErrorBuilder;

pub struct ErrorEmitter<'a> {
    builder: ErrorBuilder<'a>,
}

impl<'a> ErrorEmitter<'a> {
    pub fn new(builder: ErrorBuilder<'a>) -> Self {
        Self { builder }
    }

    pub fn emit<T: Display + Spannable>(&self, error: &T) {
        eprintln!(
            "{}",
            self.builder
                .clone()
                .info(error.to_string())
                .context(error.span())
                .build()
        );
    }
}
