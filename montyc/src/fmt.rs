use std::{fmt, ops::Deref};

use crate::{
    prelude::*,
    typing::{BuiltinTypeId, ClassType, Generic, TypeDescriptor},
};

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
                if let Some(inner) = self.gctx.type_map.get(*arg) {
                    format!(
                        "{}",
                        Formattable {
                            inner: inner.value(),
                            gctx: self.gctx
                        }
                    )
                } else {
                    format!("{}", BuiltinTypeId::Unknown)
                }
            })
            .collect::<Vec<_>>()
            .join(", ");

        let source = self.gctx.resolver.sources.get(&self.module_ref).unwrap();

        let name = self.gctx.span_ref.borrow().get(self.name).unwrap();
        let name = source.value().get(name).unwrap();

        let ret = if let Some(inner) = self.gctx.type_map.get(self.ret) {
            format!(
                "{}",
                Formattable {
                    inner: inner.value(),
                    gctx: self.gctx
                }
            )
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
            "<class {:?}>",
            self.gctx.resolver.resolve_ident(self.name).unwrap(),
        )
    }
}

impl fmt::Display for Formattable<'_, &TypeDescriptor> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.inner {
            TypeDescriptor::Simple(s) => write!(f, "{}", s),
            TypeDescriptor::Function(ft) => write!(
                f,
                "{}",
                Formattable {
                    inner: ft,
                    gctx: self.gctx
                }
            ),
            TypeDescriptor::Class(k) => write!(
                f,
                "{}",
                Formattable {
                    inner: k,
                    gctx: self.gctx
                }
            ),
            TypeDescriptor::Generic(g) => write!(
                f,
                "{}",
                Formattable {
                    inner: g,
                    gctx: self.gctx
                }
            ),
        }
    }
}

impl fmt::Display for Formattable<'_, &Generic> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.inner {
            Generic::Pointer { inner } => write!(
                f,
                "Pointer({})",
                Formattable {
                    gctx: self.gctx,
                    inner: *inner
                }
            ),

            Generic::Struct { inner } => {
                write!(
                    f,
                    "Tuple[{}]",
                    inner
                        .iter()
                        .map(|l| format!(
                            "{}",
                            Formattable {
                                gctx: self.gctx,
                                inner: *l
                            }
                        ))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }

            Generic::Union { inner } => write!(
                f,
                "Union[{}]",
                inner
                    .iter()
                    .map(|l| format!(
                        "{}",
                        Formattable {
                            gctx: self.gctx,
                            inner: *l
                        }
                    ))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}

impl fmt::Display for Formattable<'_, LocalTypeId> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            Formattable {
                gctx: self.gctx,
                inner: self.gctx.type_map.get(self.inner).unwrap().value()
            }
        )
    }
}
