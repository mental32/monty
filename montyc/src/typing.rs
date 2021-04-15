use std::{
    any::{Any, TypeId},
    collections::HashMap,
    intrinsics::transmute,
    mem::transmute_copy,
    num::NonZeroUsize,
    rc::Rc,
};

pub type NodeId = Option<NonZeroUsize>;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, derive_more::From)]
#[repr(transparent)]
pub struct LocalTypeId(usize);

#[derive(Debug)]
pub struct TaggedType<T> {
    pub type_id: LocalTypeId,
    pub inner: T,
}

#[derive(Debug, Clone)]
pub struct FunctionType {
    pub name: SpanEntry,
    pub args: Vec<LocalTypeId>,
    pub ret: LocalTypeId,
    pub decl: Option<Rc<dyn AstObject>>,

    pub resolver: Rc<InternalResolver>,
    pub module_ref: ModuleRef,
}

impl std::fmt::Display for FunctionType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let args = self
            .args
            .iter()
            .map(|arg| {
                if let Some(inner) = self.resolver.type_map.borrow().get(*arg) {
                    format!("{}", inner)
                } else {
                    format!("{}", BuiltinTypeId::Unknown)
                }
            })
            .collect::<Vec<_>>()
            .join(", ");

        let s_map = self.resolver.sources.borrow();;
        let source = s_map.get(&self.module_ref).unwrap();
        let name = self.resolver.span_ref.borrow().resolve_ref(self.name, source).unwrap();

        let ret = if let Some(inner) = self.resolver.type_map.borrow().get(self.ret) {
            format!("{}", inner)
        } else {
            format!("{}", BuiltinTypeId::Unknown)
        };

        write!(
            f,
            "<function {}({}) -> {}>",
            name, args, ret
        )
    }
}

#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
#[repr(u8)]
pub enum BuiltinTypeId {
    Invalid = 0,
    Int = 1,
    Float = 2,
    Str = 3,
    Bool = 4,
    None = 5,
    Ellipsis = 6,
    Module = 7,
    Unknown = 8,
    Never = 255,
}

impl std::fmt::Display for BuiltinTypeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BuiltinTypeId::Invalid => write!(f, "{{error: invalid type}}"),
            BuiltinTypeId::Int => write!(f, "{{integer}}"),
            BuiltinTypeId::Float => write!(f, "{{float}}"),
            BuiltinTypeId::Str => write!(f, "{{str}}"),
            BuiltinTypeId::Bool => write!(f, "{{bool}}"),
            BuiltinTypeId::None => write!(f, "{{None}}"),
            BuiltinTypeId::Ellipsis => write!(f, "... (ellipsis)"),
            BuiltinTypeId::Module => write!(f, "{{module}}"),
            BuiltinTypeId::Unknown => write!(f, "{{unknown}}"),
            BuiltinTypeId::Never => write!(f, "{{never}}"),
        }
    }
}

#[derive(Debug, Clone, derive_more::From)]
pub enum TypeDescriptor {
    Simple(BuiltinTypeId),
    Function(FunctionType),
}

impl std::fmt::Display for TypeDescriptor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeDescriptor::Simple(s) => write!(f, "{}", s),
            TypeDescriptor::Function(ft) => write!(f, "{}", ft),
        }
    }
}

impl TypeDescriptor {
    fn inner_type_id(&self) -> TypeId {
        match self {
            Self::Simple(_) => TypeId::of::<BuiltinTypeId>(),
            Self::Function(_) => TypeId::of::<FunctionType>(),
        }
    }

    fn inner_ptr(&self) -> *const () {
        match self {
            Self::Simple(s) => s as *const _ as *const (),
            Self::Function(f) => f as *const _ as *const (),
        }
    }
}

use crate::{ast::AstObject, context::{InternalResolver, LocalContext, ModuleRef}, parser::SpanEntry};

pub trait TypedObject {
    fn infer_type<'a>(&self, ctx: &LocalContext<'a>) -> Option<LocalTypeId>;

    fn typecheck<'a>(&self, ctx: LocalContext<'a>);
}

#[derive(Debug)]
pub struct TypeMap(HashMap<LocalTypeId, TypeDescriptor>);

impl TypeMap {
    pub const INTEGER: LocalTypeId = LocalTypeId(1);
    pub const FLOAT: LocalTypeId = LocalTypeId(2);
    pub const STRING: LocalTypeId = LocalTypeId(3);
    pub const BOOL: LocalTypeId = LocalTypeId(4);
    pub const NONE_TYPE: LocalTypeId = LocalTypeId(5);
    pub const ELLIPSIS: LocalTypeId = LocalTypeId(6);
    pub const MODULE: LocalTypeId = LocalTypeId(7);
    pub const UNKNOWN: LocalTypeId = LocalTypeId(8);
    pub const NEVER: LocalTypeId = LocalTypeId(255);

    #[inline]
    pub fn new() -> Self {
        let mut mapping = HashMap::with_capacity(std::mem::variant_count::<BuiltinTypeId>());

        mapping.insert(Self::INTEGER, TypeDescriptor::Simple(BuiltinTypeId::Int));
        mapping.insert(Self::FLOAT, TypeDescriptor::Simple(BuiltinTypeId::Float));
        mapping.insert(Self::STRING, TypeDescriptor::Simple(BuiltinTypeId::Str));
        mapping.insert(Self::BOOL, TypeDescriptor::Simple(BuiltinTypeId::Bool));
        mapping.insert(Self::NONE_TYPE, TypeDescriptor::Simple(BuiltinTypeId::None));
        mapping.insert(
            Self::ELLIPSIS,
            TypeDescriptor::Simple(BuiltinTypeId::Ellipsis),
        );
        mapping.insert(Self::MODULE, TypeDescriptor::Simple(BuiltinTypeId::Module));
        mapping.insert(
            Self::UNKNOWN,
            TypeDescriptor::Simple(BuiltinTypeId::Unknown),
        );
        mapping.insert(Self::NEVER, TypeDescriptor::Simple(BuiltinTypeId::Never));

        Self(mapping)
    }

    #[inline]
    pub fn insert<T>(&mut self, t: T) -> LocalTypeId
    where
        T: Into<TypeDescriptor>,
    {
        let idx = self
            .0
            .keys()
            .max()
            .cloned()
            .map(|n| LocalTypeId(n.0 + 1))
            .unwrap_or(LocalTypeId(255));
        self.0.insert(idx, t.into());
        idx
    }

    #[inline]
    pub fn insert_tagged<T>(&mut self, t: T) -> TaggedType<T>
    where
        T: Into<TypeDescriptor> + Clone,
    {
        let type_id = self.insert(t.clone().into());

        TaggedType { type_id, inner: t }
    }

    #[inline]
    pub fn get(&self, type_id: LocalTypeId) -> Option<&TypeDescriptor> {
        self.0.get(&type_id)
    }

    #[inline]
    pub fn get_tagged<T: 'static + Clone>(
        &self,
        type_id: LocalTypeId,
    ) -> Option<Result<TaggedType<T>, TypeDescriptor>> {
        let descriptor = self.get(type_id)?;

        let ptr = if descriptor.inner_type_id() == TypeId::of::<T>() {
            descriptor.inner_ptr() as *const T
        } else {
            return Some(Err(descriptor.clone()));
        };

        // SAFETY: The TypeId of the inner type of the descriptor
        //         matches that of the type provided so we can ptr
        //         cast the variant payload reference correctly.
        let inner = unsafe { &*ptr }.clone();

        let result = TaggedType { type_id, inner };

        Some(Ok(result))
    }
}
