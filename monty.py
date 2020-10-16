import ast
import builtins
import sys
from sys import modules
import typing
from ast import dump, walk
from collections import deque
from dataclasses import dataclass, field
from functools import lru_cache
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
)

CAN_HAVE_SCOPE = (ast.Module, ast.ClassDef, ast.FunctionDef, ast.AsyncFunctionDef)


class TypeInfo:
    """Base class for all types."""


@dataclass
class Function(TypeInfo):
    name: str
    args: List[TypeInfo]
    ret: TypeInfo

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


@dataclass
class TypeRef(TypeInfo):
    other: "Item"

    def __hash__(self) -> int:
        return hash(self.other.kind)

    def __eq__(self, o: object) -> bool:
        r = typing.cast("TypeRef", o)
        return type(o) == type(self) and r.other == self.other


@dataclass
class PrimitiveBase(TypeInfo):
    name: str

    def __hash__(self) -> int:
        return hash(self.name)

    def __eq__(self, other: object) -> bool:
        b = typing.cast("PrimitiveBase", other)
        return type(other) is PrimitiveBase and b.name == self.name

    def __repr__(self) -> str:
        return f"Primitive({self.name!r})"

    def __str__(self) -> str:
        return {
            "NoneType": "None",
            "UnknownType": "{unknown}",
            "ModuleType": "{module}",
            "IntegerType": "{int}",
            "FloatType": "{float}",
        }[self.name]


NoneType = PrimitiveBase(name="NoneType")
UnknownType = PrimitiveBase(name="UnknownType")
ModuleType = PrimitiveBase(name="ModuleType")
IntegerType = PrimitiveBase(name="IntegerType")
FloatType = PrimitiveBase(name="FloatType")
BoolType = PrimitiveBase(name="BoolType")


@dataclass
class Pointer(TypeInfo):
    inner: TypeInfo

    def __hash__(self) -> int:
        return hash(self.inner)


# PRIMITIVE_TYPES: Dict[Any, Union[Function, PrimitiveBase]]
PRIMITIVE_TYPES = {
    str: Function(name="str", args=[], ret=Pointer(IntegerType)),
    int: Function(name="int", args=[], ret=IntegerType),
    float: Function(name="float", args=[], ret=FloatType),
    bool: Function(name="bool", args=[], ret=BoolType),
    # type(None): NoneType,
}

ASTNode = ast.AST


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


@dataclass
class Item:
    node: ASTNode
    parent: Optional[ASTNode] = field(default=None, repr=False)
    kind: Optional[TypeInfo] = field(default=None)
    scope: Optional["Scope"] = field(repr=False, default=None)

    def __hash__(self) -> int:
        return hash(self.node)


@dataclass
class Scope:
    item: Item
    elements: List[Item] = field(default_factory=list)

    def __iter__(self) -> Iterator[Item]:
        return iter(self.elements)

    def lookup(self, target: str, *, driver: "MontyDriver") -> Optional[Item]:
        for item in self:
            node = item.node

            assert isinstance(node, ASTNode)

            if any(
                [
                    isinstance(node, (ast.AnnAssign, ast.AugAssign))
                    and node.target == target,
                    isinstance(node, ast.Name)
                    and isinstance(node.ctx, ast.Store)
                    and node.id == target,
                    isinstance(node, ast.arg) and node.arg == target,
                    isinstance(node, ast.FunctionDef) and node.name == target,
                ]
            ):
                return item

            if type(node) in {
                ast.Gt,
                ast.Compare,
                ast.Add,
                ast.AugAssign,
                ast.While,
                ast.FunctionDef,
                ast.AnnAssign,
                ast.arg,
                ast.Name,
                ast.Assign,
                ast.Constant,
                ast.Call,
                ast.Return,
                ast.arguments,
                ast.Store,
                ast.Load,
                ast.BinOp,
                ast.If,
                ast.Expr,
            }:
                continue

            raise NotImplementedError(item)

        if (parent := self.item.parent) is not None:
            scope = driver._ast_nodes[parent].item.scope
            assert scope is not None
            result = scope.lookup(target, driver=driver)
        else:
            result = None

        return result


class SpanInfo(NamedTuple):
    file_name: str
    lineno: int
    col_offset: int
    end_lineno: int
    end_col_offset: int

    def display(self, origin: str) -> str:
        return origin.split("\n")[self.lineno - 1][
            self.col_offset : self.end_col_offset
        ]

    def fmt(self, st: str, *, origin: Optional[str] = None) -> str:
        if origin is not None:
            assert isinstance(origin, str)
            prefix = "| "
            display = "\n".join(
                f"\n| {part}" for part in self.display(origin=origin).split("\n")
            )
        else:
            display = prefix = ""

        return (
            f"\n{prefix}[{self.file_name!s} @ {self.lineno}:{self.col_offset}]: {st!s}"
            + display
        )


class NodeEntry(NamedTuple):
    item: Item
    span: SpanInfo

    @property
    def node(self) -> ASTNode:
        return self.item.node

    @classmethod
    def from_module(
        cls, root: ast.Module, *, module_name: str
    ) -> Tuple["NodeEntry", Dict[ASTNode, "NodeEntry"]]:
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
        depth, _, node = stream.popleft()
        local_entry = cls.from_node(node=node, module_name=module_name)

        local_nodes: Dict[ASTNode, NodeEntry] = {}
        dangling: Dict[ASTNode, List[Item]] = {}
        bucket: Dict[int, List[Item]] = {depth: [local_entry.item]}

        while stream:
            n, p, node = stream.popleft()

            local_nodes[node] = local_entry = NodeEntry.from_node(
                node=node, module_name=module_name
            )

            # print(f"\n[{n}] {p}\t{node}")

            peek_depth = stream[0][0] if stream else 0

            if (
                not isinstance(node, CAN_HAVE_SCOPE)
                and isinstance(p, CAN_HAVE_SCOPE)
                and (max(peek_depth, n) - min(peek_depth, n) > 1)
            ):
                # print(
                # f"[{depth} -> {stream[0][0]}] Bailout! {local_entry.item.node!r} will be added to {p}"
                # )

                if p in dangling:
                    dangling[p] += bucket[depth]
                else:
                    dangling[p] = bucket[depth][:]

                dangling[p].append(local_entry.item)

                bucket[depth] = []
                depth = n

            elif n == (depth - 1) or p is None:
                # print(f"[{n}] Flushing depth bucket! {local_entry.item!r}")

                assert local_entry.item.scope is not None

                for item in dangling.pop(node, []) + bucket[depth]:
                    local_entry.item.scope.elements.append(item)

                    if not isinstance(item.node, CAN_HAVE_SCOPE):
                        item.scope = local_entry.item.scope

                    item.parent = local_entry.item.node

                if p is not None:
                    if n in bucket:
                        bucket[n].append(local_entry.item)
                    else:
                        bucket[n] = [local_entry.item]
                else:
                    bucket = {}

                depth = n

                # print(
                # f"[{n}] {local_entry.item!r} now contains {len(local_entry.item.scope.elements)} element(s)"
                # )
                # for elem in local_entry.item.scope:
                # print(f"\t{elem}")

            elif n == depth:
                # print(f"[{n}] Adding node to depth {local_entry.item!r}")
                bucket[n].append(local_entry.item)

            else:
                bucket[n] = [local_entry.item]
                depth = n
        else:
            assert not dangling, repr(dangling)
            assert not bucket, repr(bucket)

        root_entry = local_nodes[root]

        return (root_entry, local_nodes)

    @classmethod
    def from_node(cls, node: ASTNode, *, module_name: str) -> "NodeEntry":
        span_info = SpanInfo(
            file_name=module_name,
            lineno=getattr(node, "lineno", None),
            end_lineno=getattr(node, "end_lineno", None),
            col_offset=getattr(node, "col_offset", None),
            end_col_offset=getattr(node, "end_col_offset", None),
        )

        item = Item(node=node)

        if isinstance(node, CAN_HAVE_SCOPE):
            item.scope = Scope(item=item)

        return cls(
            item=item,
            span=span_info,
        )


@dataclass
class Module:
    name: str
    root: Item

    @lru_cache
    def functions(self, *, driver) -> Set[Item]:
        return {
            driver.node_entry[node]
            for node in ast.walk(self.root.node)
            if isinstance(node, ast.FunctionDef)
        }


@dataclass
class MontyDriver:
    modules: Dict[str, ASTNode] = field(default_factory=dict)
    functions: Set[Function] = field(default_factory=set)

    _ast_nodes: Dict[ASTNode, NodeEntry] = field(default_factory=dict)
    _source_map: Dict[ast.Module, str] = field(default_factory=dict)

    def __post_init__(self):
        self.functions.update(PRIMITIVE_TYPES.values())
        self.functions.update(
            {
                Function(
                    name="IntegerType.__add__",
                    args=[IntegerType, IntegerType],
                    ret=IntegerType,
                ),
                Function(
                    name="IntegerType.__radd__",
                    args=[IntegerType, IntegerType],
                    ret=IntegerType,
                ),
                Function(
                    name="builtins.print",
                    args=[Pointer(inner=IntegerType)],
                    ret=NoneType,
                ),
            }
        )

    def node_entry(self, node: ASTNode) -> NodeEntry:
        return self._ast_nodes[node]

    def _resolve_type(self, ty: TypeInfo) -> TypeInfo:
        """Return a fully qualified type i.e. not a type-ref."""
        assert isinstance(ty, TypeInfo), repr(ty)

        while isinstance(ty, TypeRef):
            ty = typing.cast("TypeInfo", self._ast_nodes[ty.other.node].item.kind)

        return ty

    def _get_direct_parent_node(self, item: Item) -> Optional[ASTNode]:
        assert item.parent is not None, item

        parent_scope = self._ast_nodes[item.parent].item.scope

        assert parent_scope is not None

        if isinstance(item.node, ast.Name) and isinstance(item.node.ctx, ast.Store):

            def predicate(cur: Item) -> bool:
                return (
                    isinstance(asn := cur.node, ast.Assign)
                    and len(asn.targets) == 1
                    and asn.targets[0] is item.node
                ) or (
                    isinstance(ann := cur.node, ast.AnnAssign)
                    and ann.target is item.node
                )

        elif isinstance(item.node, (ast.Add, ast.Sub, ast.Mult)):

            def predicate(cur: Item) -> bool:
                return (
                    isinstance(binop := cur.node, ast.BinOp) and binop.op is item.node
                )

        else:
            raise NotImplementedError(
                f"I dont know how to get the direct parent node of this type of node {item}"
            )

        for parent in filter(predicate, parent_scope.elements):
            return parent.node
        else:
            return None

    def typecheck(self, entry: NodeEntry):
        """Recursively typecheck some node entry."""

        def type_eq(lhs: TypeInfo, rhs: TypeInfo) -> bool:
            return self._resolve_type(lhs) == self._resolve_type(rhs)

        assert entry.item.scope is not None

        for item in entry.item.scope:
            item_node: ASTNode = item.node

            if item_node is entry.item.node:
                continue

            if isinstance(item_node, ast.FunctionDef):
                self.typecheck(entry=self._ast_nodes[item.node])
                continue

            if isinstance(item_node, ast.If):
                t = self._ast_nodes[item_node.test]
                t_k = t.item.kind
                assert t_k is not None
                t_t = self._resolve_type(t_k)
                assert t_t is BoolType, ast.dump(t.item.node)

            elif isinstance(item_node, ast.Return):
                assert item.kind is not None
                assert isinstance(entry.item.node, ast.FunctionDef)
                assert isinstance(entry.item.kind, Function)

                expected = self._resolve_type(entry.item.kind.ret)
                actual = self._resolve_type(item.kind)

                if not type_eq(expected, actual):
                    span_info = self._ast_nodes[item.node].span

                    def is_visible(n: ASTNode) -> bool:
                        return not isinstance(
                            n, (ast.Load, ast.Store, ast.Add, ast.Sub, ast.Module)
                        )

                    def fmt(n: ASTNode) -> str:
                        entry = self._ast_nodes[n]
                        module = self.modules[entry.span.file_name]
                        assert isinstance(module, ast.Module)
                        source_span = self._source_map[module]
                        assert entry.item.kind is not None
                        return (
                            f"{entry.span.display(origin=source_span)!r}"
                            f" has type {self._resolve_type(entry.item.kind).__str__()!r}"
                        )

                    kinds = "\n".join(
                        f"|\t{fmt(node)!s}"
                        for node in ast.walk(item.node)
                        if is_visible(node) and node is not item.node
                    )

                    m = self.modules[span_info.file_name]
                    assert isinstance(m, ast.Module)

                    raise TypeError(
                        f"\n| [{span_info.lineno}:{span_info.col_offset}]: Attempted to return with type {actual}, expected type {expected}"
                        f"\n|\t{span_info.display(origin=self._source_map[m])!r}"
                        f"\n| where:\n{kinds!s}"
                    )

            elif isinstance(item_node, ast.AugAssign):
                raise NotImplementedError()

            elif isinstance(item_node, ast.AnnAssign):
                node = typing.cast("ast.AnnAssign", item_node)
                annotation, value = node.annotation, node.value

                assert value is not None

                expected_kind = self._ast_nodes[annotation].item.kind
                assert expected_kind is not None
                expected = self._resolve_type(expected_kind)

                actual_kind = self._ast_nodes[value].item.kind
                assert actual_kind is not None
                actual = self._resolve_type(actual_kind)

                if not type_eq(expected, actual):
                    span_info = self._ast_nodes[item.node].span
                    raise TypeError(
                        f"[{span_info.lineno}:{span_info.col_offset}]: Expected type {expected} instead got {actual}"
                    )

            elif isinstance(item_node, (ast.Compare, ast.Gt, ast.Add, ast.Sub)):
                if item.kind is UnknownType:
                    parent = self._get_direct_parent_node(item=item)
                    assert parent is not None
                    parent_entry = self._ast_nodes[parent]

                    m = self.modules[parent_entry.span.file_name]
                    assert isinstance(m, ast.Module)
                    origin = self._source_map[m]

                    raise TypeError(
                        parent_entry.span.fmt(
                            st="Unable to infer the type of this expression.",
                            origin=origin,
                        )
                    )

            elif isinstance(item_node, ast.Assign):
                assert len(item_node.targets) == 1

            elif type(item_node) in {
                ast.While,
                ast.Call,
                ast.Store,
                ast.Load,
                ast.Name,
                ast.Constant,
                ast.arguments,
                ast.arg,
                ast.BinOp,
                ast.Expr,
            }:
                continue

            else:
                raise NotImplementedError(item)

        # raise NotImplementedError()

    def infer(self, entry: NodeEntry) -> Optional[TypeInfo]:
        """Attempt to infer the type of some item."""
        item = entry.item

        if item.kind is not None or item.kind is UnknownType:
            # Happy path!
            return item.kind

        item_node_type = type(item.node)

        def check_builtins(target: str) -> Optional[Function]:
            builtins_: Dict[str, Function]
            builtins_ = {
                f.name[len("builtins.") :]: f
                for f in self.functions
                if f.name.startswith("builtins.")
            }

            return builtins_.get(target, None)

        # fmt: off
        LEGAL_NODE_TYPES = frozenset({
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
            ast.Gt,
            ast.Compare,
            ast.Add,
            ast.AugAssign,
            ast.While,
            ast.BinOp,
            ast.If,
            ast.Expr,
        })
        # fmt: on

        def _validate_argument(argument: ast.arg) -> ast.arg:
            assert isinstance(argument, ast.arg)
            span = self._ast_nodes[argument].span
            assert (
                getattr(argument, "annotation", None) is not None
            ), f"[{span.lineno}:{span.col_offset}]: All non-self parameters must be type annotated."
            return argument

        if item_node_type not in LEGAL_NODE_TYPES:
            raise NotImplementedError(
                f"{item_node_type!r} is not supported at the moment."
            )

        if item_node_type in {ast.Store, ast.Load, ast.arguments, ast.Gt, ast.If}:
            return UnknownType

        elif item_node_type is ast.Expr:
            assert isinstance(item.node, ast.Expr)
            return TypeRef(other=self._ast_nodes[item.node.value].item)

        elif item_node_type is ast.Compare:
            node: ast.Compare = typing.cast("ast.Compare", item.node)

            if (len(node.ops), len(node.comparators)) != (1, 1):
                span = self._ast_nodes[item.node].span
                raise SyntaxError(
                    f"[{span.lineno}:{span.col_offset}]: Comparisson is too complex to solve."
                )

            raise NotImplementedError()
            breakpoint()
            # Compare(left=Name(id='x', ctx=Load()), ops=[Gt()], comparators=[Constant(value=10)])
            return UnknownType

        elif item_node_type in {ast.Add, ast.Sub, ast.BinOp}:
            if not isinstance(item.node, ast.BinOp):
                parent = self._get_direct_parent_node(item=item)
                op_type = item_node_type
            else:
                parent = item.node
                assert isinstance(parent, ast.BinOp)
                op_type = type(parent.op)

            assert parent is not None
            assert isinstance(parent, ast.BinOp)

            (lhs, rhs) = map(
                self.infer, map(self.node_entry, [parent.left, parent.right])
            )

            assert lhs is not None
            assert rhs is not None

            lhs = self._resolve_type(ty=lhs)
            rhs = self._resolve_type(ty=rhs)

            name: Dict[type, str]
            name = {ast.Add: "add", ast.Sub: "sub", ast.Mult: "mul"}[
                op_type  # type: ignore
            ]

            assert isinstance(lhs, PrimitiveBase)
            assert isinstance(rhs, PrimitiveBase)

            ltr = Function(
                name=f"{lhs.name}.__{name}__", args=[lhs, rhs], ret=UnknownType
            )
            rtl = Function(
                name=f"{rhs.name}.__r{name}__", args=[rhs, lhs], ret=UnknownType
            )

            def unify(f: Function) -> Optional[Function]:
                for other in self.functions:
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

            if (func := (unify(ltr) or unify(rtl))) is not None:
                kind = func.ret
            else:
                kind = UnknownType

            self._ast_nodes[parent].item.kind = kind
            return kind

        elif item_node_type is ast.Call:
            assert isinstance(item.node, ast.Call)
            assert isinstance(item.node.func, ast.Name)
            assert item.scope is not None

            if (
                f := item.scope.lookup(target=item.node.func.id, driver=self)
            ) is not None:
                assert isinstance(f, Item)
                func_kind = f.kind
            else:
                func_kind = check_builtins(item.node.func.id)

            if not isinstance(func_kind, Function):
                span_info = self._ast_nodes[item.node].span
                actual = func_kind

                raise TypeError(
                    f"[{span_info.lineno}:{span_info.col_offset}]: Type {actual} is not callable."
                )

            args: List[TypeInfo] = [
                self.infer(self._ast_nodes[arg_]) or panic()
                for arg_ in item.node.args
            ]

            assert all(arg is not None for arg in args)

            _f = Function(name=func_kind.name, args=args, ret=func_kind.ret)

            assert func_kind == _f, (func_kind, _f)

            return func_kind.ret

        elif item_node_type is ast.arg:
            assert item.scope is not None
            assert isinstance(item.node, ast.arg)

            arg = _validate_argument(item.node)

            assert arg.annotation is not None
            assert isinstance(arg.annotation, ast.Name)

            thing = item.scope.lookup(target=arg.annotation.id, driver=self)

            return thing.kind if thing is not None else None

        elif item_node_type is ast.Return:
            assert isinstance(item.node, ast.Return)
            assert item.node.value is not None

            value = self._ast_nodes[item.node.value]
            return self.infer(entry=value)

        elif item_node_type is ast.FunctionDef:
            assert isinstance(item.node, ast.FunctionDef)
            arguments: List[TypeInfo] = []

            if any(ast.iter_fields(item.node.args)):
                f_args = item.node.args

                for group in [f_args.posonlyargs, f_args.args, f_args.kwonlyargs]:
                    for argument in group:
                        argument_entry = self._ast_nodes[_validate_argument(argument)]
                        argument_type = self.infer(argument_entry)
                        assert argument_type is not None
                        arguments.append(argument_type)

            return_annotation = item.node.returns
            ret: TypeInfo = NoneType

            if return_annotation is not None:
                node_ = self.infer(self._ast_nodes[return_annotation])
                assert node_ is not None
                ret = node_.ret if isinstance(node_, Function) else node_

            return Function(name=item.node.name, args=arguments, ret=ret)

        elif item_node_type is ast.Name:
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

                dependent_entry = self._ast_nodes[oof]

                return TypeRef(other=dependent_entry.item)

            elif isinstance(item.node.ctx, ast.Load):
                assert item.scope is not None
                result = item.scope.lookup(target=item.node.id, driver=self)

                if result is None:
                    if (fn_type := check_builtins(item.node.id)) is not None:
                        result_type = fn_type.ret
                    else:
                        result_type = UnknownType
                else:
                    assert result.kind is not None
                    result_type = result.kind if result is not None else UnknownType

                return result_type or UnknownType

            else:
                assert isinstance(item.node.ctx, ast.Store)
                parent_node = self._get_direct_parent_node(item)
                assert parent_node is not None
                assert isinstance(parent_node, (ast.Assign, ast.AnnAssign))
                assert parent_node.value is not None
                value = self._ast_nodes[parent_node.value]
                return TypeRef(other=value.item)

        elif item_node_type is ast.Constant:
            assert isinstance(item.node, ast.Constant)
            return PRIMITIVE_TYPES[type(item.node.value)].ret

        elif item_node_type in (ast.AnnAssign, ast.Assign, ast.AugAssign, ast.While):
            return NoneType

        elif item_node_type is ast.Module:
            return ModuleType

        breakpoint()

        return None

    def _comptime_map(self, module: ast.Module) -> ast.Module:
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
            for (
                idx,
                obj,
            ) in enumerate(l):
                if p(obj):
                    return idx
            else:
                return None

        def comptime_bool(n: ASTNode, seq: Optional[Sequence[ASTNode]] = None) -> bool:
            assert isinstance(n, (ast.Constant, ast.Name)), ast.dump(n)

            if isinstance(n, ast.Constant):
                return bool(n.value)

            if isinstance(n, ast.Name) and isinstance(n.ctx, ast.Load):
                # comptime-name-lookup requires a sequence of AST nodes that
                # that came "before" this one. This sequence forms the raw
                # body of what will be used to resolve the name.
                assert (
                    seq is not None
                ), f"No history sequence provided in order to search the name: {ast.dump(n)!r}"
                assert isinstance(seq, list), repr(seq)
                assert ast.If not in set(
                    map(type, seq)
                ), f"Refusing to comptime evaluate a history with branches {seq!r}"

                ALLOWED_TYPES = (ast.Assign, ast.AnnAssign, ast.FunctionDef)
                for node in reversed(seq):
                    if type(node) not in ALLOWED_TYPES:
                        continue
                    else:
                        ty = ofwhichinstance(node, *ALLOWED_TYPES)

                    if ty is ast.FunctionDef and node.name == n.id:
                        return True

                    if ty is ast.AnnAssign and node.name == n.id:
                        return comptime_bool(node.value)

                    if (
                        isinstance(asn := node, ast.Assign)
                        and len(asn.targets) == 1
                        and isinstance(lhs := asn.targets[0], ast.Name)
                        and lhs.id == n.id
                    ):
                        return comptime_bool(asn.value, seq=seq)
                else:
                    raise NotImplementedError(f"{list(map(ast.dump, seq))}")

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
                    assert False, NodeEntry.from_node(
                        node=asrt, module_name="..."
                    ).span.fmt("Failed static assert.", origin=self._source_map[m])

                del m.body[idx]

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

        return module

    def compile(self, st: str, *, module_name: str = "__main__"):
        root_tree = ast.parse(st)
        self._source_map[root_tree] = st
        root_tree = self._comptime_map(root_tree)

        dump_kwargs = {"indent": 4} if sys.version_info >= (3, 9) else {}
        print(ast.dump(root_tree, **dump_kwargs))  # type: ignore

        self.modules[module_name] = root_tree

        (root_entry, local_nodes) = NodeEntry.from_module(
            root=root_tree, module_name=module_name
        )

        self._ast_nodes[root_tree] = root_entry
        self._ast_nodes.update(local_nodes)

        for (node, entry) in local_nodes.items():
            if (inferred := self.infer(entry)) is not None:
                entry.item.kind = inferred
            else:
                assert False, f"Failed to infer type for {ast.dump(entry.item.node)=!r}"

            if isinstance(inferred, Function):
                self.functions.add(inferred)

            elif isinstance(node, (ast.Import, ast.ImportFrom)):
                raise NotImplementedError("Import logic is missing.")

        self.typecheck(entry=root_entry)


test_comptime = """
if False:
    x = 1
elif False:
    x = 2
elif True:
    def f(x: int) -> int:
        return x
else:
    x = 3

assert f
"""

test_input = """
def main():
    print()
""".strip()

driver = MontyDriver()
driver.compile(test_input)
