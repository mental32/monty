use montyc_core::TypeId;

#[derive(Debug, Clone, Copy)]
pub enum ValueKind<V> {
    Imm(V),
    ThinRef(V),
    FatRef(V, V),
}

#[derive(Debug, Clone, Copy)]
pub struct TValue<V> {
    inner: ValueKind<V>,
    pub type_id: TypeId,
}

impl<V> TValue<V> {
    pub fn imm(val: V, ty: TypeId) -> Self {
        Self {
            inner: ValueKind::Imm(val),
            type_id: ty,
        }
    }

    pub fn reference(val: V, ty: TypeId) -> Self {
        Self {
            inner: ValueKind::ThinRef(val),
            type_id: ty,
        }
    }

    pub fn fat(left: V, right: V, ty: TypeId) -> Self {
        Self {
            inner: ValueKind::FatRef(left, right),
            type_id: ty,
        }
    }

    pub fn as_value(&self) -> &V {
        match &self.inner {
            ValueKind::Imm(a) | ValueKind::ThinRef(a) | ValueKind::FatRef(a, _) => a,
        }
    }
}
