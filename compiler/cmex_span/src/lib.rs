#[derive(Debug, Clone, Copy)]
pub struct Span(pub usize, pub usize);

impl Span {
    pub fn join(a: Self, b: Self) -> Self {
        Self(a.0, b.1).normalize()
    }

    fn normalize(self) -> Self {
        if self.0 > self.1 {
            Self(self.1, self.0)
        } else {
            self
        }
    }
}

pub trait Spannable {
    fn span(&self) -> Span;
}

pub trait MaybeSpannable {
    fn span(&self) -> Option<Span>;
}

pub trait Unspan<T> {
    fn val(self) -> Option<T>;
}

pub type SpannedOption<I> = Option<(I, Span)>;

impl<T> Unspan<T> for SpannedOption<T> {
    /// A handy method to convert `Option<(T, Span)>` to `Option<T>`
    fn val(self) -> Option<T> {
        self.map(|(val, _)| val)
    }
}

impl From<usize> for Span {
    fn from(pos: usize) -> Self {
        Self(pos, pos)
    }
}

impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}:{}", self.0, self.1))
    }
}
