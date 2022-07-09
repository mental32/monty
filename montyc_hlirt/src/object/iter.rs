use crate::eval::ctx::EvalGlue;
use crate::exception::PyResult;
use crate::ObjectId;

enum IterKind {
    Empty,
    Direct(std::vec::IntoIter<ObjectId>),
}

pub struct PyIter {
    kind: IterKind,
}

impl<I> From<I> for PyIter
where
    I: Iterator<Item = ObjectId>,
{
    fn from(it: I) -> Self {
        Self {
            kind: IterKind::Direct(it.collect::<Vec<_>>().into_iter()),
        }
    }
}

impl PyIter {
    pub fn empty() -> Self {
        Self {
            kind: IterKind::Empty,
        }
    }

    pub fn next(&mut self, _ecx: &mut dyn EvalGlue) -> Option<PyResult<ObjectId>> {
        match &mut self.kind {
            IterKind::Empty => None,
            IterKind::Direct(it) => it.next().map(Ok),
        }
    }
}
