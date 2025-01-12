#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

impl<T: Spannable> MaybeSpannable for Vec<T> {
    fn span(&self) -> Option<Span> {
        self.iter().map(|item| item.span()).reduce(Span::join)
    }
}

pub trait Unspan<T> {
    fn val(self) -> Option<T>;
}

impl<T> Unspan<T> for Option<(T, Span)> {
    fn val(self) -> Option<T> {
        self.map(|(val, _)| val)
    }
}

impl<T: Clone> Unspan<T> for Option<&(T, Span)> {
    fn val(self) -> Option<T> {
        self.map(|(val, _)| val).cloned()
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
