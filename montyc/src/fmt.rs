use std::{fmt, ops::Deref};

use crate::{prelude::*, typing::{BuiltinTypeId, ClassType, Generic, TypeDescriptor}};

pub struct Formattable<'a, T> {
    pub(crate) gctx: &'a GlobalContext,
    pub(crate) inner: T,
}

impl<T> Deref for Formattable<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}


impl fmt::Display for Formattable<'_, &FunctionType> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let args = self
            .args
            .iter()
            .map(|arg| {
                if let Some(inner) = self.gctx.resolver.type_map.get(*arg) {
                    format!("{}", Formattable { inner: inner.value(), gctx: self.gctx })
                } else {
                    format!("{}", BuiltinTypeId::Unknown)
                }
            })
            .collect::<Vec<_>>()
            .join(", ");

        let source = self.gctx.resolver.sources.get(&self.module_ref).unwrap();
        let name = self.gctx
            .resolver
            .span_ref
            .borrow()
            .resolve_ref(self.name, source.value())
            .unwrap();

        let ret = if let Some(inner) = self.gctx.resolver.type_map.get(self.ret) {
            format!("{}", Formattable { inner: inner.value(), gctx: self.gctx })
        } else {
            format!("{}", BuiltinTypeId::Unknown)
        };

        write!(f, "<function {}({}) -> {}>", name, args, ret)
    }
}


impl fmt::Display for Formattable<'_, &ClassType> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "<type object @ name={:?}, module={:?}>",
            self.gctx.resolver.resolve(self.mref.clone(), self.name),
            self.mref,
        )
    }
}


impl fmt::Display for Formattable<'_, &TypeDescriptor> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.inner {
            TypeDescriptor::Simple(s) => write!(f, "{}", s),
            TypeDescriptor::Function(ft) => write!(f, "{}", Formattable { inner: ft, gctx: self.gctx }),
            TypeDescriptor::Class(k) => write!(f, "{}", Formattable { inner: k, gctx: self.gctx }),
            TypeDescriptor::Generic(g) => write!(f, "{}", g),
        }
    }
}


impl fmt::Display for Generic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Generic::Pointer { inner } => write!(f, "Pointer({:?})", inner),
            Generic::Union { inner } => write!(f, "Union[{}]", inner.iter().map(|l| format!("{:?}", l)).collect::<Vec<_>>().join(", "))
        }
    }
}

impl fmt::Display for Formattable<'_, LocalTypeId> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", Formattable { gctx: self.gctx, inner: self.gctx.type_map.get(self.inner).unwrap().value() })
    }
}
