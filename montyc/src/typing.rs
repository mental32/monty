use std::{
    any::{Any, TypeId},
    cell::Cell,
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
    pub reciever: Option<LocalTypeId>,
    pub name: SpanEntry,
    pub args: Vec<LocalTypeId>,
    pub ret: LocalTypeId,
    pub decl: Option<Rc<FunctionDef>>,
    // non type related stuff
    pub resolver: Rc<InternalResolver>,
    pub module_ref: ModuleRef,
}

impl std::fmt::Display for FunctionType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let args = self
            .args
            .iter()
            .map(|arg| {
                if let Some(inner) = self.resolver.type_map.get(*arg) {
                    format!("{}", inner.value())
                } else {
                    format!("{}", BuiltinTypeId::Unknown)
                }
            })
            .collect::<Vec<_>>()
            .join(", ");

        let source = self.resolver.sources.get(&self.module_ref).unwrap();
        let name = self
            .resolver
            .span_ref
            .borrow()
            .resolve_ref(self.name, source.value())
            .unwrap();

        let ret = if let Some(inner) = self.resolver.type_map.get(self.ret) {
            format!("{}", inner.value())
        } else {
            format!("{}", BuiltinTypeId::Unknown)
        };

        write!(f, "<function {}({}) -> {}>", name, args, ret)
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

#[derive(Debug, Clone)]
pub struct ClassType {
    pub name: SpanEntry,
    pub mref: ModuleRef,
    pub resolver: Rc<InternalResolver>,
}

impl std::fmt::Display for ClassType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "<type object @ name={:?}, module={:?}>",
            self.resolver.resolve(self.mref.clone(), self.name),
            self.mref,
        )
    }
}

#[derive(Debug, Clone, derive_more::From)]
pub enum TypeDescriptor {
    Simple(BuiltinTypeId),
    Function(FunctionType),
    Class(ClassType),
}

impl std::fmt::Display for TypeDescriptor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeDescriptor::Simple(s) => write!(f, "{}", s),
            TypeDescriptor::Function(ft) => write!(f, "{}", ft),
            TypeDescriptor::Class(k) => write!(f, "{}", k),
        }
    }
}

impl TypeDescriptor {
    fn inner_type_id(&self) -> TypeId {
        match self {
            Self::Simple(_) => TypeId::of::<BuiltinTypeId>(),
            Self::Function(_) => TypeId::of::<FunctionType>(),
            Self::Class(_) => TypeId::of::<ClassType>(),
        }
    }

    fn inner_ptr(&self) -> *const () {
        match self {
            Self::Simple(s) => s as *const _ as *const (),
            Self::Function(f) => f as *const _ as *const (),
            Self::Class(c) => c as *const _ as *const (),
        }
    }
}

use dashmap::DashMap;

use crate::{
    ast::{funcdef::FunctionDef, AstObject},
    context::{resolver::InternalResolver, LocalContext, ModuleRef},
    parser::SpanEntry,
    MontyError,
};

pub trait TypedObject {
    fn infer_type<'a>(&self, ctx: &LocalContext<'a>) -> crate::Result<LocalTypeId>;

    fn typecheck<'a>(&self, ctx: &LocalContext<'a>) -> crate::Result<()>;
}

#[derive(Debug)]
pub struct TypeMap {
    last_id: Cell<usize>,
    inner: DashMap<LocalTypeId, TypeDescriptor>,
}

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
        let mut mapping = DashMap::with_capacity(std::mem::variant_count::<BuiltinTypeId>());

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

        Self {
            inner: mapping,
            last_id: Cell::new(255),
        }
    }

    #[inline]
    pub fn insert<T>(&self, t: T) -> LocalTypeId
    where
        T: Into<TypeDescriptor>,
    {
        let idx = self.last_id.replace(self.last_id.get() + 1);
        let idx = LocalTypeId(idx);

        self.inner.insert(idx, t.into());

        idx
    }

    #[inline]
    pub fn unify_call<'a>(
        &self,
        func_t: LocalTypeId,
        callsite: impl Iterator<Item = &'a LocalTypeId>,
    ) -> Result<LocalTypeId, (LocalTypeId, LocalTypeId, usize)> {
        let func = self.get_tagged::<FunctionType>(func_t).unwrap().unwrap();

        for (idx, (actual, expected)) in callsite.cloned().zip(func.inner.args).enumerate() {
            if actual == Self::UNKNOWN {
                continue;
            } else if expected != actual {
                return Err((expected, actual, idx));
            }
        }

        Ok(func_t)
    }

    #[inline]
    pub fn unify_func(&self, func_t: LocalTypeId, mold: &FunctionType) -> bool {
        let func = self.get_tagged::<FunctionType>(func_t).unwrap().unwrap();

        if (func.inner.name != mold.name)
            || (func.inner.args.len() != mold.args.len())
            || (mold.ret != Self::UNKNOWN && mold.ret != func.inner.ret)
        {
            return false;
        }

        func.inner
            .args
            .iter()
            .zip(mold.args.iter())
            .all(|(l, r)| (*l == Self::UNKNOWN) || (l == r))
    }

    #[inline]
    pub fn insert_tagged<T>(&self, t: T) -> TaggedType<T>
    where
        T: Into<TypeDescriptor> + Clone,
    {
        let type_id = self.insert(t.clone().into());

        TaggedType { type_id, inner: t }
    }

    #[inline]
    pub fn get(&self, type_id: LocalTypeId) -> Option<dashmap::mapref::one::Ref<'_, LocalTypeId, TypeDescriptor>> {
        self.inner.get(&type_id)
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

pub trait CompilerError {
    type Success;

    fn unwrap_or_compiler_error<'a>(self, ctx: &LocalContext<'a>) -> Self::Success;
}

impl CompilerError for Option<LocalTypeId> {
    type Success = LocalTypeId;

    fn unwrap_or_compiler_error<'a>(self, ctx: &LocalContext<'a>) -> Self::Success {
        match self {
            Some(t) => t,
            None => {
                todo!();
            }
        }
    }
}

impl<T> CompilerError for Result<T, MontyError> {
    type Success = T;

    fn unwrap_or_compiler_error<'b>(self, ctx: &LocalContext<'b>) -> Self::Success {
        match self {
            Ok(t) => t,
            Err(err) => ctx.error(err),
        }
    }
}
