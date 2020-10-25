from abc import ABC, abstractmethod
import ast
import builtins
from pathlib import Path
import sys
from sys import modules, path
import typing
from contextlib import suppress
from ast import dump, walk
from collections import deque
from dataclasses import InitVar, dataclass, field
from functools import cached_property, lru_cache, partial, singledispatch, singledispatchmethod
from typing import (
    Any,
    Callable,
    Dict,
    Iterator,
    List,
    NamedTuple,
    Optional,
    Sequence,
    Set,
    Tuple,
    Type,
    TypeVar,
    Union,
)

ASTNode = ast.AST
STDLIB_PATH = Path(__file__).parent.joinpath("std").absolute()
CAN_HAVE_SCOPE = (ast.Module, ast.ClassDef, ast.FunctionDef, ast.AsyncFunctionDef)
LEGAL_NODE_TYPES = {
    ast.ClassDef,
    ast.Module,
    ast.FunctionDef,
    ast.AnnAssign,
    ast.Assign,
    ast.Name,
    ast.Load,
    ast.Store,
    ast.Constant,
    ast.arguments,
    ast.Return,
    ast.arg,
    ast.Call,
    ast.Compare,
    ast.Add,
    ast.Mult,
    ast.Sub,
    ast.AugAssign,
    ast.While,
    ast.BinOp,
    ast.If,
    ast.Expr,
    ast.Import,
    ast.alias,
    ast.ImportFrom,
    ast.Attribute,
    ast.Pass,
}


class TypeInfo(ABC):
    """Base class for all types."""

    @abstractmethod
    def size(self) -> int:
        """Get the size of this type."""


@dataclass
class Pointer(TypeInfo):
    inner: TypeInfo

    def size(self) -> int:
        """Get the size of this type."""
        return 64  # TODO: This is fixed for 64-bit architectures.

    def __hash__(self) -> int:
        return hash(self.inner)

    def __str__(self) -> str:
        return f"Pointer[{self.inner!s}]"


class _StringType(Pointer):
    def __str__(self) -> str:
        return "{str}"


@dataclass
class Function(TypeInfo):
    name: str
    args: List[TypeInfo]
    ret: TypeInfo
    impl: Optional[Callable[[ASTNode], None]] = field(default=None)

    def size(self) -> int:
        """Get the size of this type."""
        return Pointer(inner=NoneType).size()

    def __hash__(self) -> int:
        return hash((self.name, tuple(map(hash, self.args)), hash(self.ret)))

    def __eq__(self, o: object) -> bool:
        f = typing.cast("Function", o)

        return (
            type(o) == type(self)
            and f.name == self.name
            and f.args == self.args
            and f.ret == self.ret
        )

    def __str__(self) -> str:
        args = ", ".join(map(str, self.args))
        return f"<function {self.name!s}({args}) -> {self.ret!s}>"


@dataclass
class TypeRef(TypeInfo):
    other: "Item"

    def size(self) -> int:
        """Get the size of this type."""
        raise TypeError(
            "TypeRef's don't have a size known at compile time, resolve it first!"
        )

    def __hash__(self) -> int:
        return hash(self.other.kind)

    def __eq__(self, o: object) -> bool:
        r = typing.cast("TypeRef", o)
        return type(o) == type(self) and r.other == self.other


@dataclass
class PrimitiveBase(TypeInfo):
    name: str = field(default_factory=str)
    kind: InitVar[Optional[type]] = field(default=None)

    def size(self) -> int:
        """Get the size of this type."""
        return 0  # Unsized

    def __post_init__(self, kind: Optional[type]):
        if self.name or kind is None:
            return

        assert not self.name and kind is not None
        self.name = {
            type(None): "NoneType",
            ast.Module: "ModuleType",
            int: "int",
            float: "FloatType",
            bool: "BoolType",
        }.get(kind, "UnknownType")

    def __hash__(self) -> int:
        return hash(self.name)

    def __eq__(self, other: object) -> bool:
        b = typing.cast("PrimitiveBase", other)
        return type(other) is PrimitiveBase and b.name == self.name

    def __repr__(self) -> str:
        return f"Primitive({self.name!r})"

    def __str__(self) -> str:
        return {
            "NoneType": "{None}",
            "UnknownType": "{unknown}",
            "ModuleType": "{module}",
            "int": "{int}",
            "FloatType": "{float}",
            "BoolType": "{bool}",
        }[self.name]


@dataclass
class KlassType(TypeInfo):
    """Represents a `class` definition."""

    node: ast.ClassDef
    name: str
    # bases: dict
    # namepsace: dict

    def __hash__(self) -> int:
        return hash(self.name)

    def size(self) -> int:
        raise NotImplementedError()


NoneType = PrimitiveBase(kind=type(None))
UnknownType = PrimitiveBase(kind=object)
ModuleType = PrimitiveBase(kind=ast.Module)
IntegerType = PrimitiveBase(kind=int)
FloatType = PrimitiveBase(kind=float)
BoolType = PrimitiveBase(kind=bool)

StringType = _StringType(inner=IntegerType)

# PRIMITIVE_TYPES: Dict[Any, Union[Function, PrimitiveBase]]
PRIMITIVE_TYPES = {
    str: Function(name="builtins.str", args=[], ret=StringType),
    int: Function(name="builtins.int", args=[], ret=IntegerType),
    float: Function(name="builtins.float", args=[], ret=FloatType),
    bool: Function(name="builtins.bool", args=[], ret=BoolType),
    type(None): NoneType,
}


def ofwhichinstance(this: Any, *those: type) -> Optional[type]:
    """Like `isinstance` but returns the type that was matched on."""
    for that in those:
        if isinstance(this, that):
            return that
    else:
        return None


def panic(msg: str = "..."):
    raise AssertionError(msg)


WalkStream = Iterator[Tuple[int, Optional[ASTNode], ASTNode]]


def postorder_walk(
    parent: Optional[ASTNode],
    node: ASTNode,
    *,
    _stack_depth: int = 0,
    pred: Callable[[Optional[ASTNode], ASTNode], bool],
) -> WalkStream:
    for child in ast.iter_child_nodes(node):
        depth = _stack_depth + pred(node, child) + pred(parent, node)
        yield from postorder_walk(node, child, _stack_depth=depth, pred=pred)
    else:
        yield (_stack_depth, parent, node)


class Builtin:
    """Base class used as a market type for all compiler built-in stuff."""


# import x.y as z
#
# qualname = ("x", "y")
# access_path = "z"
#
# scope.lookup("z") == ImportDecl(qualname = ("x", "y"))
@dataclass
class ImportDecl:
    node: ast.alias = field(repr=False)
    parent: Union[ast.Import, ast.ImportFrom] = field(repr=False)

    def __repr__(self) -> str:
        return f"ImportDec({ast.dump(self.node)=!r})"

    @cached_property
    def qualname(self) -> List[str]:
        return self.node.name.split(".")

    @cached_property
    def realname(self) -> str:
        return self.node.asname or self.node.name

    def __hash__(self) -> int:
        return hash(self.node)

    def __str__(self) -> str:
        return self.realname


class SpanInfo(NamedTuple):
    module_name: str = ""
    file_name: str = ""
    source_ref: str = field(repr=False, default="")
    lineno: int = sys.maxsize
    col_offset: int = sys.maxsize
    end_lineno: int = sys.maxsize
    end_col_offset: int = sys.maxsize

    def display(self, *, lines: int = -1) -> str:
        sameline = self.lineno == self.end_lineno
        splat = self.source_ref.split("\n")

        if not sameline:
            return "\n".join(splat[self.lineno - 1:self.end_lineno][:lines])
        else:
            return splat[self.lineno - 1][
            self.col_offset : self.end_col_offset
        ]

    def fmt(self, st: str) -> str:
        if self.source_ref:
            prefix = "| "
            display = "\n".join(f"\n| {part!r}" for part in self.display().split("\n"))
        else:
            display = prefix = ""

        return (
            f"\n{prefix}[{self.file_name!s} @ {self.lineno}:{self.col_offset}]: {st!s}"
            + display
        )


@dataclass
class Item:
    span: SpanInfo = field(repr=False, default_factory=SpanInfo)
    node: ASTNode = field(default_factory=(lambda: ast.Pass()))
    parent: Optional[ASTNode] = field(default=None, repr=False)
    kind: TypeInfo = field(default=UnknownType)
    scope: "Scope" = field(
        repr=False, default_factory=(lambda: Scope(parent_node=None))
    )

    def __hash__(self) -> int:
        return hash(self.node)

    @classmethod
    def from_node(cls, node: ASTNode, *, module_name: str, source_ref: str) -> "Item":
        span_info = SpanInfo(
            source_ref=source_ref,
            module_name=module_name,
            file_name=module_name,
            lineno=getattr(node, "lineno", sys.maxsize),
            end_lineno=getattr(node, "end_lineno", sys.maxsize),
            col_offset=getattr(node, "col_offset", sys.maxsize),
            end_col_offset=getattr(node, "end_col_offset", sys.maxsize),
        )

        return cls(node=node, span=span_info)

    @classmethod
    def from_module(
        cls,
        root: ast.Module,
        *,
        module_name: str,
        source_ref: str,
        file_name: Optional[str] = None,
    ) -> Tuple["Item", Dict[ASTNode, "Item"]]:
        """Produce a realized entry for a module root."""
        assert isinstance(root, ast.Module)

        def check_depth(pred, succ) -> bool:
            """A depth change has occured if the parent abd child node own a scope."""
            return isinstance(pred, CAN_HAVE_SCOPE) and isinstance(succ, CAN_HAVE_SCOPE)

        # Generate a post-order "walk" (this will attempt to yield AST nodes from the leaves up.)
        traversal = postorder_walk(parent=None, node=root, pred=check_depth)

        # The following code will fold all non-owning scope nodes into their scope-owning parents.
        # from the leaves up into the module root.

        stream = deque(traversal)
        depth, parent, node = stream.popleft()
        local_entry = cls.from_node(
            node=node, module_name=module_name, source_ref=source_ref
        )

        if parent is None:
            return (local_entry, {})

        local_nodes: Dict[ASTNode, Item] = {node: local_entry}
        dangling: Dict[ASTNode, List[Item]] = {}
        bucket: Dict[int, List[Item]] = {depth: [local_entry]}

        def flush(*, bucket, depth, local_entry, dangling, node, p) -> int:
            for item in dangling.pop(node, []) + bucket[depth]:
                local_entry.scope.elements.append(item)

                if not isinstance(item.node, CAN_HAVE_SCOPE):
                    item.scope = local_entry.scope

                item.parent = local_entry.node
                item.scope.parent = local_entry.node

                # print(f"\t{item}")

            # if isinstance(local_entry.node, ast.FunctionDef):
            #     assert ast.BinOp in map(
            #         type, [_.node for _ in local_entry.scope.elements]
            #     ), breakpoint()  # type: ignore

            if p is not None:
                if n in bucket:
                    bucket[n].append(local_entry)
                else:
                    bucket[n] = [local_entry]
            else:
                bucket.clear()

            return n

        while stream:
            n, p, node = stream.popleft()

            local_nodes[node] = local_entry = cls.from_node(
                node=node,
                module_name=module_name,
                source_ref=source_ref,
            )

            peek_depth = stream[0][0] if stream else 0

            if (
                not isinstance(node, CAN_HAVE_SCOPE)
                and isinstance(p, CAN_HAVE_SCOPE)
                and (max(peek_depth, n) - min(peek_depth, n) > 1)
            ):
                if p in dangling:
                    dangling[p] += bucket[depth]
                else:
                    dangling[p] = bucket[depth][:]

                dangling[p].append(local_entry)

                bucket[depth] = []
                depth = n

            elif n == (depth - 1) or p is None:
                # print(f"[{depth=!r} {n=!r}] Flush! {node}")
                assert local_entry.scope is not None
                depth = flush(
                    bucket=bucket,
                    depth=depth,
                    local_entry=local_entry,
                    dangling=dangling,
                    node=node,
                    p=p,
                )

            elif n == depth:
                # print(f"[{depth=!r} {n=!r}] Same depth {node}")
                bucket[n].append(local_entry)

            else:
                # print(f"[{depth=!r} {n=!r}] LARGE depth diff {node}")

                if remaining := bucket[depth]:
                    depth = flush(
                        bucket={depth: remaining},
                        depth=depth,
                        local_entry=local_entry,
                        dangling=dangling,
                        node=node,
                        p=p,
                    )
                    # # breakpoint()

                bucket[n] = [local_entry]
                depth = n
        else:
            assert not dangling, repr(dangling)
            assert not bucket, repr(bucket)

        root_entry = local_nodes[root]

        # breakpoint()

        return (root_entry, local_nodes)

    def get_direct_parent_node(self, ctx: "Context") -> Optional[ASTNode]:
        if (cached := getattr(self, "_direct_parent_node", None)) is not None:
            return cached

        if isinstance(self.node, ast.Module):
            return self.node

        parent = ctx.modules[self.span.module_name].root if self.parent is None else ctx.ast_nodes[self.parent]

        assert parent.scope is not None

        @singledispatch
        def predicate(node: ASTNode, cur: Item) -> bool:
            raise NotImplementedError(
                f"I dont know how to get the direct parent node of this type of node {ASTNode}"
            )

        @predicate.register
        def _pred_assign(node: ast.Name, cur: Item) -> bool:
            return isinstance(node.ctx, ast.Store) and (
                isinstance(asn := cur.node, ast.Assign)
                and len(asn.targets) == 1
                and asn.targets[0] is self.node
            ) or (
                isinstance(ann := cur.node, ast.AnnAssign)
                and ann.target is self.node
            )

        @predicate.register(ast.Add)
        @predicate.register(ast.Sub)
        @predicate.register(ast.Mult)
        def _pred_binop(node, cur: Item) -> bool:
            return (
                    isinstance(binop := cur.node, ast.BinOp) and binop.op is self.node
                )

        @predicate.register
        def _pred_import(node: ast.alias, cur: Item) -> bool:
            return (
                isinstance(imp := cur.node, (ast.ImportFrom, ast.Import))
                and self.node in imp.names
            )

        @predicate.register
        def _pred_funcdef(node: ast.Return, cur: Item) -> bool:
            return isinstance(
                    cur.node, (ast.FunctionDef)
                ) and self.node in ast.walk(cur.node)

        @predicate.register(ast.FunctionDef)
        @predicate.register(ast.ClassDef)
        def _pred_scopeable(node, cur: Item) -> bool:
            return isinstance(cur.node, CAN_HAVE_SCOPE) and self in cur.scope

        pred = partial(predicate.dispatch(self.node.__class__), self.node)

        # breakpoint()

        for parent in filter(pred, [parent, *parent.scope.elements]):
            setattr(self, "_direct_parent_node", parent.node)
            return parent.node
        else:
            return None

def collapse_attribute(n: Union[ast.Attribute, ast.Name]) -> str:
    if isinstance(n, ast.Attribute):
        assert isinstance(n.value, (ast.Attribute, ast.Name))
        return f"{collapse_attribute(n.value)!s}.{n.attr}"

    if isinstance(n, ast.Name):
        return n.id

    assert False


@dataclass
class Scope:
    parent_node: Optional[ASTNode] = None
    elements: List[Item] = field(default_factory=list)

    def __iter__(self) -> Iterator[Item]:
        return iter(self.elements)

    def lookup(self, target: str, *, ctx: "Context") -> Optional[Item]:
        for item in self:
            node = item.node

            if (
                (
                    isinstance(node, (ast.AnnAssign, ast.AugAssign))
                    and node.target == target
                )
                or (
                    isinstance(node, ast.Name)
                    and isinstance(node.ctx, ast.Store)
                    and node.id == target
                )
                or (isinstance(node, ast.arg) and node.arg == target)
                or (isinstance(node, ast.FunctionDef) and node.name == target)
                or (isinstance(node, ast.alias) and node.name == target)
                or (
                    isinstance(node, ast.Attribute)
                    and collapse_attribute(node).startswith(target)
                )
                or (isinstance(node, ast.ClassDef) and node.name == target)
            ):
                return item

            if type(node) in LEGAL_NODE_TYPES:
                continue

            raise NotImplementedError((item, ast.dump(item.node)))

        if (parent := self.parent_node) is not None:
            scope = ctx.ast_nodes[parent].scope
            assert scope is not None
            result = scope.lookup(target, ctx=ctx)
        else:
            result = None

        return result


@dataclass
class Module:
    # "__main__"
    name: str

    # Item(node=ast.Module, parent=None, ...)
    root: Item

    # Big ass map of all nodes to entries.
    local_nodes: Dict[ASTNode, Item] = field(default_factory=dict)

    # The filepath to the module (if any)
    path: Optional[Path] = field(default=None)

    @lru_cache
    def functions(self, *, driver) -> Set[Item]:
        return {
            driver.node_entry[node]
            for node in ast.walk(self.root.node)
            if isinstance(node, ast.FunctionDef)
        }


def is_visible(n: ASTNode) -> bool:
    return not isinstance(
        n, (ast.Load, ast.Store, ast.Add, ast.Sub, ast.Module, ast.arguments)
    )


@dataclass
class Context:
    """Big blob of compilation state."""
    modules: Dict[str, Module] = field(default_factory=dict)
    functions: Set[Function] = field(default_factory=set)
    klass_types: Set[KlassType] = field(default_factory=set)

    ast_nodes: Dict[ASTNode, Item] = field(default_factory=dict)
    source_map: Dict[ast.Module, str] = field(default_factory=dict)
    builtin_types: Dict[TypeInfo, ASTNode] = field(default_factory=dict)

    def __post_init__(self):
        self.autoimpls = {
            IntegerType: [
                Function(name="__add__", args=[IntegerType, IntegerType], ret=IntegerType)
            ]
        }

    def node_entry(self, node: ASTNode) -> Item:
        return self.ast_nodes[node]

    def type_eq(self, lhs: TypeInfo, rhs: TypeInfo) -> bool:
        return self.resolve_type(lhs) == self.resolve_type(rhs)

    def get_qualname(self, item: Item) -> str:
        assert isinstance(item.node, (ast.FunctionDef, ast.Module, ast.ClassDef))

        def name_of(node: ASTNode) -> str:
            if isinstance(node, ast.ClassDef):
                stem = node.name

            elif isinstance(node, ast.Module):
                stem = cursor.span.module_name

            elif isinstance(node, ast.FunctionDef):
                return self._ast_nodes[node].kind.name  # type: ignore

            else:
                raise NotImplementedError(parent)

            return stem

        qualname = f"{name_of(item.node)!s}"

        cursor = item
        parent = None

        while cursor is not parent:
            parent = cursor.get_direct_parent_node(ctx=self)
            assert parent is not None

            qualname = f"{name_of(parent)}.{qualname!s}"

            if isinstance(parent, ast.Module):
                break
            else:
                cursor = self.ast_nodes[parent]

        return qualname

    def resolve_type(self, ty: TypeInfo) -> TypeInfo:
        """Return a fully qualified type i.e. not a type-ref."""
        assert isinstance(ty, TypeInfo), repr(ty)

        while isinstance(ty, TypeRef):
            ty = self.ast_nodes[ty.other.node].kind

        return ty

    def check_builtins(self, target: str) -> Optional[Function]:
        for func in self.functions:
            if func.name == f"builtins.{target!s}":
                return func
        else:
            return None

    def fmt(self, node: ASTNode) -> str:
        entry = self.ast_nodes[node]
        assert entry.kind is not None

        kind = self.resolve_type(entry.kind)
        st = (
            kind.__str__()
            if not isinstance(kind, KlassType)
            else self.get_qualname(self.ast_nodes[kind.node])
        )

        return f"{entry.span.display()!r}" f" has type {st!r}"

class Typing:
    # Helpers

    @staticmethod
    def _validate_argument(argument: ast.arg, item: Item) -> ast.arg:
        assert isinstance(argument, ast.arg)

        if getattr(argument, "annotation", None) is None:
            span = item.span
            assert (
                False
            ), f"[{span.lineno}:{span.col_offset}]: All non-self parameters must be type annotated."

        return argument

    # Typechecking

    def typecheck(self, entry: Item, ctx: Context):
        """Recursively typecheck some node entry."""

        assert entry.scope is not None

        for item in entry.scope:
            item_node: ASTNode = item.node

            if item_node is entry.node:
                continue

            self._typecheck_node(item.node, item=item, ctx=ctx)
            continue

    @singledispatchmethod
    def _typecheck_node(self, node: ASTNode, item: Item, ctx: Context):
        SUPPORTED_NODES = {
            ast.While,
            ast.Store,
            ast.Load,
            ast.Name,
            ast.Constant,
            ast.arguments,
            ast.arg,
            ast.Expr,
            ast.alias,
            ast.Import,
            ast.ImportFrom,
            ast.Attribute,
            ast.Pass,
            ast.BinOp,
        }

        if type(node) not in SUPPORTED_NODES:
            raise NotImplementedError(item)

    @_typecheck_node.register
    def _typecheck_classdef(self, node: ast.ClassDef, item: Item, ctx: Context):
        self.typecheck(entry=item, ctx=ctx)

    @_typecheck_node.register
    def _typecheck_funcdef(self, node: ast.FunctionDef, item: Item, ctx: Context):
        assert isinstance(item.kind, Function)

        self.typecheck(entry=item, ctx=ctx)

        if not any(isinstance(it.node, ast.Return) for it in item.scope) and item.kind.ret is not NoneType:
            span_info = item.span

            raise TypeError(
                f"\n| [{span_info.lineno}:{span_info.col_offset}]: expected type {item.kind.ret}, found {NoneType}."
                f"\n|\t{span_info.display(lines=1)!r}"
                 "\n|"
                f"\n| implicitly returns None as its body has no `return` statement."
            )

    @_typecheck_node.register
    def _typecheck_attribute(self, node: ast.Attribute, item: Item, ctx: Context):
        assert item.kind is not None

        if ctx.type_eq(item.kind, UnknownType):
            kinds = "\n".join(
                f"|\t{ctx.fmt(node)!s}"
                for node in ast.walk(item.node)
                if is_visible(node) and node is not item.node
            )

            raise TypeError(
                f"\n| [{item.span.lineno}:{item.span.col_offset}]: Unable to infer type for attribute access..."
                f"\n|\t{item.span.display()!r}"
                f"\n| where:\n{kinds!s}"
            )

    @_typecheck_node.register
    def _typecheck_ifstmt(self, node: ast.If, item: Item, ctx: Context):
        t = ctx.ast_nodes[node.test]
        t_k = t.kind
        assert t_k is not None
        t_t = ctx.resolve_type(t_k)
        assert t_t is BoolType, ast.dump(t.node)

    @_typecheck_node.register
    def _typecheck_return(self, node: ast.Return, item: Item, ctx: Context):
        assert item.kind is not None

        parent_node = item.get_direct_parent_node(ctx=ctx)
        assert parent_node is not None
        assert isinstance(parent_node, ast.FunctionDef), parent_node

        parent_item = ctx.ast_nodes[parent_node]
        assert isinstance(parent_item.kind, Function)

        parent_item_kind = (
            parent_item.kind.ret
            if isinstance(parent_item.kind, Function)
            else parent_item.kind
        )

        expected = ctx.resolve_type(parent_item_kind)
        actual = ctx.resolve_type(item.kind)

        if not ctx.type_eq(expected, actual):
            span_info = ctx.ast_nodes[item.node].span

            kinds = "\n".join(
                f"|\t{ctx.fmt(node)!s}"
                for node in ast.walk(node)
                if is_visible(node) and node is not node
            )

            raise TypeError(
                f"\n| [{span_info.lineno}:{span_info.col_offset}]: Attempted to return with type {actual}, expected type {expected}"
                f"\n|\t{span_info.display()!r}"
                + (f"\n| where:\n{kinds!s}" if kinds else "")
            )

    @_typecheck_node.register
    def _typecheck_augassign(self, node: ast.AugAssign, item: Item, ctx: Context):
        raise NotImplementedError()

    @_typecheck_node.register
    def _typecheck_annassign(self, node: ast.AnnAssign, item: Item, ctx: Context):
        annotation, value = node.annotation, node.value

        assert value is not None

        expected_kind = ctx.ast_nodes[annotation].kind
        assert expected_kind is not None
        expected = ctx.resolve_type(expected_kind)

        actual_kind = ctx.ast_nodes[value].kind
        assert actual_kind is not None
        actual = ctx.resolve_type(actual_kind)

        if not ctx.type_eq(expected, actual):
            span_info = ctx.ast_nodes[item.node].span
            raise TypeError(
                f"[{span_info.lineno}:{span_info.col_offset}]: Expected type {expected.__str__()!r} instead got {actual.__str__()!r}"
            )

    @_typecheck_node.register(ast.Compare)
    @_typecheck_node.register(ast.Gt)
    @_typecheck_node.register(ast.Add)
    @_typecheck_node.register(ast.Sub)
    @_typecheck_node.register(ast.Mult)
    def _typecheck_binop(self, node, item: Item, ctx: Context):
        if item.kind is UnknownType:
            parent = item.get_direct_parent_node(ctx=ctx)
            assert parent is not None
            parent_entry = ctx.ast_nodes[parent]
            item_span = parent_entry.span

            kinds = "\n".join(
                f"|\t{ctx.fmt(node)!s}"
                for node in ast.iter_child_nodes(parent)
                if is_visible(node) and node is not item.node
            )

            raise TypeError(
                f"\n| [{item_span.lineno}:{item_span.col_offset}]: Unable to infer the result type of this expression..."
                f"\n|\t{item_span.display()!r}"
                f"\n| where:\n{kinds!s}"
            )

    @_typecheck_node.register
    def _typecheck_assign(self, node: ast.Assign, item: Item, ctx: Context):
        # elif isinstance(item_node, ast.Assign):
        assert len(node.targets) == 1

    @_typecheck_node.register
    def _typecheck_call(self, node: ast.Call, item: Item, ctx: Context):
        assert isinstance(item.node, ast.Call)
        f_entry = ctx.ast_nodes[item.node.func]
        span_info = ctx.ast_nodes[item.node].span

        if not isinstance(func_kind := f_entry.kind, Function):
            assert func_kind is not None
            actual = func_kind

            raise TypeError(
                f"[{span_info.lineno}:{span_info.col_offset}]: Type {actual} is not callable."
            )

        args: List[TypeInfo] = [
            self.infer(ctx.ast_nodes[arg_], ctx=ctx) or panic()
            for arg_ in item.node.args
        ]

        assert all(arg not in {UnknownType, None} for arg in args)

        callsite_signature = Function(name=func_kind.name, args=args, ret=func_kind.ret)

        if callsite_signature != func_kind:
            if callsite_signature.args != func_kind.args:
                n_expected_args = len(func_kind.args)
                n_actual_args = len(callsite_signature.args)
                assert n_expected_args != n_actual_args
                plural = lambda n: "s" if n > 1 else ""
                reason = (
                    f"this function takes {n_expected_args}"
                    f" argument{plural(n_expected_args)} but"
                    f" {n_actual_args} arguments were supplied."
                )
            else:
                assert False

            raise TypeError(span_info.fmt(reason))

    # Inference

    def infer(self, item: Item, ctx: Context) -> Optional[TypeInfo]:
        """Attempt to infer the type of some item."""
        if item.kind not in {None, UnknownType}:
            # Happy path!
            return item.kind

        return self._infer_node(item.node, item=item, ctx=ctx)

    @singledispatchmethod
    def _infer_node(self, node: ASTNode, item: Item, ctx: Context) -> Optional[TypeInfo]:
        item_node_type = type(item.node)

        if item_node_type not in LEGAL_NODE_TYPES:
            raise NotImplementedError(
                f"{item_node_type!r} is not supported at the moment."
            )

        if item_node_type is ast.arguments:
            return UnknownType

        if item_node_type in (
            ast.Store,
            ast.Load,
            ast.AnnAssign,
            ast.Assign,
            ast.AugAssign,
            ast.While,
            ast.ImportFrom,
            ast.Import,
            ast.If,
            ast.Pass,
        ):
            return NoneType

        elif item_node_type is ast.Module:
            return ModuleType

        return None

    @_infer_node.register
    def _infer_classdef(
        self, node: ast.ClassDef, item: Item, ctx: Context
    ) -> Optional[TypeInfo]:
        return KlassType(node=node, name=node.name)

    @_infer_node.register
    def _infer_const(
        self, node: ast.Constant, item: Item, ctx: Context
    ) -> Optional[TypeInfo]:
        kind = PRIMITIVE_TYPES[type(node.value)]
        assert isinstance(kind, TypeInfo)
        return getattr(kind, "ret", kind)

    @_infer_node.register
    def _infer_name(
        self, node: ast.Name, item: Item, ctx: Context
    ) -> Optional[TypeInfo]:
        assert isinstance(item.node, ast.Name)

        if (
            isinstance(item.parent, ast.Assign)
            or isinstance(item.parent, ast.AnnAssign)
            and item.parent.annotation is not item.node
        ):
            # Type of the name depends on the type of the expression or the annotation.
            if isinstance(item.parent, ast.Assign):
                oof = typing.cast("ASTNode", item.parent.value)
            else:
                assert isinstance(item.parent, ast.AnnAssign)
                oof = typing.cast("ASTNode", item.parent.annotation)

            assert oof is not None

            dependent_entry = ctx.ast_nodes[oof]

            return TypeRef(other=dependent_entry)

        elif isinstance(item.node.ctx, ast.Load):
            assert item.scope is not None
            result = item.scope.lookup(target=item.node.id, ctx=ctx)

            if result is None:
                if (
                    fn_type := ctx.check_builtins(item.node.id)
                ) is not None:
                    result_type = fn_type.ret
                else:
                    return None

            elif result.kind is None:
                return TypeRef(other=result)

            else:
                assert result.kind is not None
                result_type = result.kind if result is not None else UnknownType

            return result_type

        else:
            assert isinstance(item.node.ctx, ast.Store)
            parent_node = item.get_direct_parent_node(ctx=ctx)
            assert parent_node is not None
            assert isinstance(parent_node, (ast.Assign, ast.AnnAssign))
            assert parent_node.value is not None
            value = ctx.ast_nodes[parent_node.value]
            return TypeRef(other=value)

    @_infer_node.register
    def _infer_funcdef(
        self, node: ast.FunctionDef, item: Item, ctx: Context
    ) -> Optional[TypeInfo]:
        assert isinstance(item.node, ast.FunctionDef)
        arguments: List[TypeInfo] = []

        if any(ast.iter_fields(item.node.args)):
            f_args = item.node.args

            for group in [f_args.posonlyargs, f_args.args, f_args.kwonlyargs]:
                for argument in group:
                    if argument.arg != "self":
                        self._validate_argument(argument, item)

                    argument_entry = ctx.ast_nodes[argument]
                    argument_type = self.infer(argument_entry, ctx=ctx)
                    assert argument_type is not None
                    arguments.append(argument_type)

        return_annotation = item.node.returns
        ret: TypeInfo = NoneType

        if return_annotation is not None:
            node_ = self.infer(ctx.ast_nodes[return_annotation], ctx=ctx)
            assert node_ is not None
            ret = node_.ret if isinstance(node_, Function) else node_

        return Function(name=item.node.name, args=arguments, ret=ret)

    @_infer_node.register
    def _infer_attribute(
        self, node: ast.Attribute, item: Item, ctx: Context
    ) -> Optional[TypeInfo]:
        assert isinstance(item.node, ast.Attribute)
        assert item.scope is not None

        result = item.scope.lookup(st := collapse_attribute(item.node), ctx=ctx)

        assert result is not None

        if result.node is item.node:
            for (name, module) in ctx.modules.items():
                module_node = module.root.node
                assert isinstance(module_node, ast.Module)

                if name == st:
                    return ModuleType

                if st.startswith(name):
                    assert st[len(name)] == "."
                    me = ctx.ast_nodes[module_node]
                    assert me.scope is not None
                    result = me.scope.lookup(target=st[len(name) + 1 :], ctx=ctx)
                    assert result is not None
                    return result.kind
            else:
                return UnknownType
        else:
            return result.kind

    @_infer_node.register
    def _infer_alias(
        self, node: ast.alias, item: Item, ctx: Context
    ) -> Optional[TypeInfo]:
        assert isinstance(item.node, ast.alias)

        import_node = item.get_direct_parent_node(ctx=ctx)
        assert import_node is not None

        if not isinstance(import_node, ast.ImportFrom):
            return ModuleType

        module_name = import_node.module
        assert isinstance(module_name, str)

        module_ = ctx.modules[module_name].root
        assert module_.scope is not None

        result = module_.scope.lookup(target=item.node.name, ctx=ctx)
        assert result is not None
        return result.kind

    @_infer_node.register(ast.Import)
    @_infer_node.register(ast.ImportFrom)
    def _infer_import(
        self, node: Union[ast.Import, ast.ImportFrom], item: Item, ctx: Context
    ) -> Optional[TypeInfo]:
        assert isinstance(item.node, (ast.ImportFrom, ast.Import))

        for al in item.node.names:
            ty = self.infer(ctx.ast_nodes[al], ctx=ctx)
            assert ty is not None
            ctx.ast_nodes[al].kind = ty

        return NoneType

    @_infer_node.register
    def _infer_expr(
        self, node: ast.Expr, item: Item, ctx: Context
    ) -> Optional[TypeInfo]:
        assert isinstance(item.node, ast.Expr)
        return TypeRef(other=ctx.ast_nodes[item.node.value])

    @_infer_node.register
    def _infer_compare(
        self, node: ast.Compare, item: Item, ctx: Context
    ) -> Optional[TypeInfo]:
        if (len(node.ops), len(node.comparators)) != (1, 1):
            span = ctx.ast_nodes[item.node].span
            raise SyntaxError(
                f"[{span.lineno}:{span.col_offset}]: Comparisson is too complex to solve."
            )

        raise NotImplementedError()
        # Compare(left=Name(id='x', ctx=Load()), ops=[Gt()], comparators=[Constant(value=10)])
        return UnknownType

    @_infer_node.register(ast.Add)
    @_infer_node.register(ast.Sub)
    @_infer_node.register(ast.Mult)
    @_infer_node.register(ast.BinOp)
    def _infer_binop(self, node, item: Item, ctx: Context) -> Optional[TypeInfo]:
        if not isinstance(item.node, ast.BinOp):
            assert (_ := node) and isinstance(_, (ast.Add, ast.Sub, ast.Mult))
            parent = item.get_direct_parent_node(ctx=ctx)
            op_type = type(node)
        else:
            assert isinstance(node, ast.BinOp)
            parent = node
            op_type = type(parent.op)

        assert parent is not None
        assert isinstance(parent, ast.BinOp)

        parent_item = ctx.ast_nodes[parent]

        f = partial(self.infer, ctx=ctx)
        (lhs, rhs) = map(f, map(ctx.node_entry, [parent.left, parent.right]))

        assert lhs is not None
        assert rhs is not None

        def into_resolved_parts(ty: TypeInfo):
            ty = ctx.resolve_type(ty=ty)

            if isinstance(ty, PrimitiveBase):
                impl = ctx.builtin_types[ty]
                imit = ctx.ast_nodes[impl]
                name = ctx.get_qualname(imit)
                return name, ty

            raise NotImplementedError(ty)

        (lhs_name, lhs) = into_resolved_parts(ty=lhs)
        (rhs_name, rhs) = into_resolved_parts(ty=rhs)

        # breakpoint()

        _ast_binop_name: Dict[type, str]
        _ast_binop_name = {ast.Add: "add", ast.Sub: "sub", ast.Mult: "mul"}[
            op_type  # type: ignore
        ]

        assert isinstance(lhs, PrimitiveBase)
        assert isinstance(rhs, PrimitiveBase)

        ltr = Function(
            name=f"{lhs_name}.__{_ast_binop_name}__",
            args=[lhs, rhs],
            ret=UnknownType,
        )

        rtl = Function(
            name=f"{rhs_name}.__r{_ast_binop_name}__",
            args=[rhs, lhs],
            ret=UnknownType,
        )

        # breakpoint()

        def unify(f: Function) -> Optional[Function]:
            for other in ctx.functions:
                if (
                    other.name != f.name
                    or len(other.args) != len(f.args)
                    or f.ret is not UnknownType
                    and f.ret != other.ret
                ):
                    continue

                assert len(other.args) == len(f.args)

                def check(lr: Tuple[TypeInfo, TypeInfo]) -> bool:
                    (l, r) = lr
                    assert r is not UnknownType
                    return (l is UnknownType) or (l == r)

                if all(map(check, zip(f.args, other.args))):
                    return other
            else:
                return None

        # breakpoint()

        func = unify(ltr) or unify(rtl)

        # print(node, func)

        parent_item.kind = kind = (func and func.ret) or UnknownType
        return kind

    @_infer_node.register
    def _infer_call(
        self, node: ast.Call, item: Item, ctx: Context
    ) -> Optional[TypeInfo]:
        func_kind: Optional[TypeInfo]

        if isinstance(node.func, ast.Attribute):
            attr_ty = self.infer(ctx.ast_nodes[node.func], ctx)
            assert attr_ty is not None
            func_kind = attr_ty
        else:
            assert isinstance(node.func, ast.Name)
            assert item.scope is not None

            if (f := item.scope.lookup(target=node.func.id, ctx=ctx)) is not None:
                assert isinstance(f, Item)
                func_kind = f.kind
            else:
                func_kind = ctx.check_builtins(node.func.id)

        if not isinstance(func_kind, Function):
            return UnknownType
        else:
            return func_kind.ret

    @_infer_node.register
    def _infer_arg(self, node: ast.arg, item: Item, ctx: Context) -> Optional[TypeInfo]:
        assert item.scope is not None
        assert isinstance(item.node, ast.arg)

        arg_parent = item.get_direct_parent_node(ctx=ctx)

        assert isinstance(arg_parent, ast.FunctionDef)

        funcdef_item = ctx.ast_nodes[arg_parent]
        funcdef_parent = funcdef_item.get_direct_parent_node(ctx)
        assert funcdef_parent is not None
        in_classdef = isinstance(funcdef_parent, ast.ClassDef)
        is_self_arg = node.arg == "self"

        if is_self_arg:
            assert in_classdef

        if in_classdef and is_self_arg:
            klass_type = ctx.ast_nodes[funcdef_parent]
            return TypeRef(other=klass_type)

        arg = self._validate_argument(node, item)

        assert arg.annotation is not None

        assert isinstance(arg.annotation, (ast.Name, ast.Constant))

        if isinstance(arg.annotation, ast.Name):
            annotation_id = arg.annotation.id
        else:
            assert isinstance(arg.annotation, ast.Constant)
            annotation_id = arg.annotation.value

        result = item.scope.lookup(target=annotation_id, ctx=ctx)

        if result is None:
            return (
                f_.ret
                if (f_ := ctx.check_builtins(annotation_id)) is not None
                else None
            )
        else:
            return result.kind

    @_infer_node.register
    def _infer_return(
        self, node: ast.Return, item: Item, ctx: Context
    ) -> Optional[TypeInfo]:
        assert isinstance(item.node, ast.Return)

        if item.node.value is not None:
            value = ctx.ast_nodes[item.node.value]
            return TypeRef(other=value)
        else:
            return NoneType


@dataclass
class MontyDriver:
    ctx: Context = field(default_factory=Context)
    tcx: Typing = field(default_factory=Typing)

    @cached_property
    def __monty(self) -> Union[Builtin, Module]:
        class _BuiltinModule(Module, Builtin):
            driver: MontyDriver

            def __init__(self, driver: "MontyDriver"):
                root = Item()
                root.span = SpanInfo(module_name = "__monty")
                super().__init__(name="__monty", root=root)
                self.driver = driver

        return _BuiltinModule(driver=self)

    def __post_init__(self):
        self.ctx.modules["__monty"] = phantom_module = self.__monty
        self.ctx.ast_nodes[phantom_module.root] = phantom_module.root

        def bind_lang_impl(node: Union[ast.ClassDef, ast.FunctionDef]):
            assert isinstance(node, (ast.ClassDef, ast.FunctionDef))
            assert node.decorator_list, breakpoint() # type: ignore

            top = node.decorator_list.pop()

            assert isinstance(top, ast.Call)
            assert isinstance(top.func, ast.Attribute)
            assert collapse_attribute(top.func) == "__monty.lang"

            assert len(top.args) == 1
            assert isinstance(top.args[0], ast.Constant)

            name = top.args[0].value
            assert isinstance(name, str)

            kind = {
                "int": IntegerType,
            }[name]

            # breakpoint()

            self.ctx.builtin_types[kind] = node

        bind_lang = Function(name="__monty.lang", args=[StringType], ret=NoneType, impl=bind_lang_impl)

        self.ctx.functions.add(bind_lang)
        self.ctx.functions.update(PRIMITIVE_TYPES.values())

    def _comptime(self, module: ast.Module, source_ref: str) -> ast.Module:
        """Given a module node, perform obvious comptime behaviour.

        Here "obvious" compile time behaviour mainly concerns itself with
        code found in the module, or global, scope. Since, in monty, the
        module scope is intended for static, const, and comptime behaviour.

        This function will mutate the given root tree in-place as it
        decides which nodes to keep or discard, such as branches which are
        proveably false (this is useful if the user wants to variably
        include code depending on the Python version.)

        Stuff that happens:

         * branches get evaluated and the succeeding block will replace the entire `if` node.
         * `assert` nodes turn into the equivelent of a `static_assert` in cxx.

        """
        assert isinstance(module, ast.Module), f"{ast.dump(module)=!r}"

        T = TypeVar("T")

        def search(l: List[T], p: Callable[[T], bool]) -> Optional[int]:
            for (idx, obj) in enumerate(l):
                if p(obj):
                    return idx
            else:
                return None

        def comptime_lookup(
            node: Union[str, ast.Name], seq: Sequence[ASTNode]
        ) -> Optional[ASTNode]:
            if isinstance(node, ast.Name):
                assert isinstance(node.ctx, ast.Load)
                target = node.id
            else:
                assert isinstance(node, str)
                target = node

            # comptime-name-lookup requires a sequence of AST nodes that
            # that came "before" this one. This sequence forms the raw
            # body of what will be used to resolve the name.
            if __debug__:
                _ = f"No history sequence provided in order to search the name: {target!r}"
                assert seq, _
                assert isinstance(seq, list), repr(seq)
                _ = f"Refusing to comptime evaluate a history with branches {seq!r}"
                assert ast.If not in map(type, seq), _

            ALLOWED_TYPES = (ast.Assign, ast.AnnAssign, ast.FunctionDef, ast.Import)
            for rev in reversed(seq):
                if type(rev) not in ALLOWED_TYPES:
                    continue

                if (
                    (
                        isinstance(rev, ast.Import)
                        and any([alias.name == target for alias in rev.names])
                    )
                    or (
                        isinstance(rev, (ast.FunctionDef, ast.AnnAssign))
                        and hasattr(rev, "name")
                        and rev.name == target  # type: ignore
                    )
                    or (
                        isinstance(asn := rev, ast.Assign)
                        and len(asn.targets) == 1
                        and isinstance(lhs := asn.targets[0], ast.Name)
                        and lhs.id == target
                    )
                ):
                    return rev
            else:
                raise NotImplementedError(f"{list(map(ast.dump, seq))}")

        def comptime_bool(n: ASTNode, seq: Optional[Sequence[ASTNode]] = None) -> bool:
            assert isinstance(n, (ast.Constant, ast.Name)), ast.dump(n)

            if isinstance(n, ast.Constant):
                return bool(n.value)

            if isinstance(n, ast.Name) and isinstance(n.ctx, ast.Load):
                assert seq is not None
                result = comptime_lookup(node=n, seq=seq)

                assert result is not None

                if isinstance(result, ast.FunctionDef):
                    return True

                if isinstance(result, ast.AnnAssign):
                    if result.value is not None:
                        return comptime_bool(result.value, seq=seq)

                    # value-less annotated assignments exist:
                    #     x: int
                    #     x = 1
                    return comptime_bool(n, seq=seq[: seq.index(result)])

                if isinstance(result, ast.Assign):
                    return comptime_bool(result.value, seq=seq)

            assert False, "unreachable"

        def yield_all_of(*, ty: Type[T], seq: List[T]) -> Iterator[Tuple[int, T]]:
            while ty in set(map(type, seq)):
                if (idx := search(seq, (lambda n: isinstance(n, ty)))) is not None:
                    yield (idx, seq[idx])
                else:
                    assert False, f"Wait, that's illegal. {seq!r}"

        def fold_branches(m: ast.Module):
            body = m.body

            for (idx, next_if) in yield_all_of(ty=ast.If, seq=body):
                head = next_if
                while True:
                    assert isinstance(head, ast.If)

                    if comptime_bool(head.test):
                        left = body[:idx]
                        right = body[idx + 1 :] if idx + 1 in range(len(body)) else []

                        body = m.body = left + head.body + right
                        break

                    elif head.orelse:
                        alt = head.orelse

                        if alt and isinstance(top := alt[0], ast.If):
                            head = top
                            continue

                        assert alt is not None

                        left = body[:idx]
                        right = body[idx + 1 :] if idx + 1 in range(len(body)) else []

                        body = m.body = left + alt + right
                        break

                    else:
                        alt = head.orelse
                        if len(alt) == 1:
                            head = alt[0]
                            continue
                        else:
                            del body[idx]
                            break

                break

        def eval_assert(m: ast.Module):
            for (idx, asrt) in yield_all_of(ty=ast.Assert, seq=m.body):
                assert isinstance(asrt, ast.Assert)

                if not comptime_bool(asrt.test, seq=m.body[:idx]):
                    assert False, Item.from_node(
                        node=asrt,
                        module_name="...",
                        source_ref=source_ref,
                    ).span.fmt("Failed static assert.")

                del m.body[idx]

        def invoke_decorators(m: ast.Module):
            nonlocal self, comptime_lookup
            for node in m.body:
                if isinstance(node, (ast.ClassDef, ast.FunctionDef)) and node.decorator_list:
                    assert len(node.decorator_list) == 1, "only one decorator allowed."

                    [top] = node.decorator_list

                    assert isinstance(top, ast.Call)
                    assert isinstance(top.func, ast.Attribute)

                    f_name = collapse_attribute(top.func)

                    assert f_name.startswith("__monty.")

                    for kind in {_ for _ in self.ctx.functions if _.name == f_name}:
                        assert kind.impl is not None
                        kind.impl(node)
                        break
                    else:
                        assert False, f"No implementation for function {f_name=!r} found!"

        # TODO:
        #   Need to think of a safer way to confirm that no modifications to
        #   the tree has happened in a single comptime pass loop, currently we
        #   just "hash" the tree with `ast.dump` and compare the previous and
        #   latest "digests"

        def apply_exhaustively(module: ast.Module, func: Callable[[ast.Module], None]):
            """Apply a transforming function to the supplied module until there is no difference."""
            previous_digest = ""
            while True:
                current_digest = ast.dump(module)
                if current_digest == previous_digest:
                    break

                func(module)

                previous_digest = ast.dump(module)

            return module

        apply_exhaustively(module, fold_branches)
        apply_exhaustively(module, eval_assert)
        apply_exhaustively(module, invoke_decorators)

        return module

    def import_module(self, decl: ImportDecl) -> Optional[Module]:
        # if (idx := self.data.fetch_by_origin(origin=decl)) is not None:
        #     return self.data[idx]

        paths_to_inspect = [Path("."), STDLIB_PATH]

        assert decl.qualname

        if isinstance(decl.parent, ast.ImportFrom):
            if decl.parent.module is not None:
                prefix = decl.parent.module.split(".")
            else:
                prefix = []

            qualname = prefix + decl.qualname
        else:
            qualname = decl.qualname

        assert qualname, f"{decl=!r}"

        fullname = ".".join(qualname)
        if (module := self.ctx.modules.get(fullname, None)) is not None:
            return module

        def search(curdir: Path, expected: str) -> Optional[Path]:
            for path in curdir.iterdir():
                is_py_file = path.is_file() and path.name.endswith(".py")

                # We cant have "." in module names.
                if not is_py_file and "." in path.name:
                    continue

                name = path.name

                if is_py_file:
                    name = path.name[:-3]

                if name == expected:
                    return path
            else:
                return None

        def compile_module(final_path: Path, qualname: List[str]):
            with open(final_path) as inf:
                return self.compile(
                    st=inf.read(), module_name=".".join(qualname), path=final_path
                )

        final_path: Optional[Path] = None
        final_qualname: List[str]

        for target in paths_to_inspect:
            qualname_iter = enumerate(iter(qualname))
            final_qualname = []
            final_path = None

            # "x.y" <- ("x", "y")
            # "./x/y.py"
            while (_ := next(qualname_iter, None)) is not None:
                if final_path is not None and not final_path.is_dir():
                    assert final_path.is_file
                    assert final_qualname
                    return compile_module(final_path, qualname=final_qualname)
                else:
                    (idx, part) = _
                    is_last = idx == (len(qualname) - 1)

                if (final_path := search(target, part)) is None:
                    break

                final_qualname.append(part)

                assert final_path is not None

                target = final_path

                if final_path.is_dir():
                    # special case?
                    # "./x/y/__init__.py"
                    #
                    # from x.y import z
                    #
                    # or
                    #
                    # from x import y

                    contains_init = (final_path / "__init__.py").exists()

                    if contains_init:
                        if is_last:
                            assert is_last
                            return compile_module(
                                final_path / "__init__.py",
                                qualname=final_qualname + [part],
                            )

                        if not final_path.is_dir():
                            return compile_module(final_path, qualname=final_qualname)

                        peek = qualname[idx + 1]

                        if search(final_path, peek) is not None:
                            continue

                    if is_last and not contains_init:
                        assert False, f"Missing __init__ file {final_path!r}"

        return (
            compile_module(final_path, qualname=qualname)
            if final_path is not None
            else None
        )

    def compile(
        self,
        st: str,
        *,
        module_name: str = "__main__",
        path: Optional[Path] = None,
    ) -> Module:
        root_tree = self._comptime(ast.parse(st), source_ref=st)

        (root_entry, local_nodes) = Item.from_module(
            root=root_tree,
            module_name=module_name,
            source_ref=st,
        )

        module_obj = Module(name=module_name, root=root_entry, path=path)
        module_obj.local_nodes = local_nodes

        self.ctx.modules[module_name] = module_obj
        self.ctx.ast_nodes[root_tree] = root_entry

        assert (
            ast.alias in kinds
            if (kinds := set(map(type, local_nodes))) & {ast.Import, ast.ImportFrom}
            else True
        )

        self.ctx.ast_nodes.update(local_nodes)

        if (newly_defined := set(self.ctx.autoimpls) & set(self.ctx.builtin_types)):
            for primitive in newly_defined:
                prototype = self.ctx.autoimpls.pop(primitive)

                impl_node = self.ctx.builtin_types[primitive]
                impl_item = self.ctx.ast_nodes[impl_node]
                impl_qualname = self.ctx.get_qualname(impl_item)

                for func in prototype:
                    self.ctx.functions.add(Function(name=f"{impl_qualname}.{func.name}", args=func.args[:], ret=func.ret))
            else:
                del newly_defined

        for (node, entry) in local_nodes.items():
            if not isinstance(node, (ast.Import, ast.ImportFrom)):
                continue

            for alias in node.names:
                decl = ImportDecl(node=alias, parent=node)

                if self.import_module(decl) is None:
                    span = entry.span.fmt(
                        f"Failed to import {decl.realname!r}",
                    )

                    raise ImportError(span)

        for (node, item) in local_nodes.items():
            if isinstance(node, (ast.FunctionDef, ast.ClassDef)):
                assert not node.decorator_list, f"decorators are not supported"

            if (inferred := self.tcx.infer(item, self.ctx)) is None:
                assert False, f"Failed to infer type for {ast.dump(item.node)=!r}"

            if isinstance(inferred, Function):
                item.kind = inferred

                if isinstance(item.get_direct_parent_node(ctx=self.ctx), ast.ClassDef):
                    func = Function(
                        name=self.ctx.get_qualname(item),
                        args=inferred.args[:],
                        ret=inferred.ret,
                    )
                else:
                    func = inferred

                self.ctx.functions.add(func)

            elif isinstance(inferred, KlassType):
                self.ctx.klass_types.add(inferred)

            item.kind = inferred
            continue

        self.tcx.typecheck(entry=root_entry, ctx=self.ctx)

        return module_obj


test_input = """
from std.builtins import int

def _() -> int:
    return 1 + 1

""".strip()


def compile(source: str) -> MontyDriver:
    driver = MontyDriver()
    module = driver.compile(source)
    assert module.name in driver.ctx.modules
    return driver


_ = compile(test_input)
