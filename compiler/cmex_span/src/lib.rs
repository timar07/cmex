#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct Span(pub usize, pub usize);

impl Span {
    pub fn placeholder() -> Self {
        Self::default()
    }

    pub fn lo(self) -> Self {
        Self(self.0, self.0)
    }

    pub fn hi(self) -> Self {
        Self(self.1, self.1)
    }

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

impl<T: Spannable> From<&Vec<T>> for Span {
    fn from(vec: &Vec<T>) -> Self {
        vec.iter()
            .map(|i| i.span())
            .reduce(Span::join)
            .unwrap_or_default()
    }
}

pub trait Spannable {
    fn span(&self) -> Span;
}

pub trait MaybeSpannable {
    fn span(&self) -> Option<Span>;
}

impl<T: Clone> MaybeSpannable for Option<(T, Span)> {
    fn span(&self) -> Option<Span> {
        self.clone().map(|val| val.1)
    }
}

impl<T: Spannable + Clone> MaybeSpannable for Option<T> {
    fn span(&self) -> Option<Span> {
        self.clone().map(|val| val.span())
    }
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
