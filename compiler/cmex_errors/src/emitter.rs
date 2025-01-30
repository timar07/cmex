use std::fmt::Display;

use cmex_span::Spannable;

use crate::ErrorBuilder;

pub struct ErrorEmitter {
    builder: ErrorBuilder,
}

impl ErrorEmitter {
    pub fn new(builder: ErrorBuilder) -> Self {
        Self { builder }
    }

    pub fn emit<T: Display + Spannable>(&self, error: &T) {
        eprintln!("{}", self.builder.clone().info(error.to_string()).build());
    }
}
