use super::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StringRef(pub SpanRef);

impl std::convert::TryFrom<Atom> for StringRef {
    type Error = Atom;

    fn try_from(value: Atom) -> Result<Self, Self::Error> {
        match value {
            Atom::Str(n) => Ok(Self(n)),
            _ => Err(value),
        }
    }
}

impl From<StringRef> for SpanRef {
    fn from(st: StringRef) -> Self {
        st.0
    }
}
