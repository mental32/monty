import ast
from ast import walk
import builtins
from collections import defaultdict, deque
from dataclasses import dataclass, field
from typing import (
    Callable,
    Dict,
    Iterator,
    List,
    NamedTuple,
    Optional,
    Set,
    Tuple,
)

CAN_HAVE_SCOPE = (ast.Module, ast.ClassDef, ast.FunctionDef, ast.AsyncFunctionDef)


class TypeInfo:
    pass


@dataclass
class Function(TypeInfo):
    name: str
    args: List[TypeInfo]
    ret: TypeInfo

    def __eq__(self, o: object) -> bool:
        return (
            type(o) == type(self)
            and o.name == self.name
            and o.args == self.args
            and o.ret == self.ret
        )

@dataclass
class TypeRef(TypeInfo):
    other: "Item"

    def __eq__(self, o: object) -> bool:
        return type(o) == type(self) and o.other == self.other


@dataclass
class PrimitiveBase(TypeInfo):
    name: str

    def __eq__(self, other: object) -> bool:
        return type(other) is PrimitiveBase and other.name == self.name

    def __repr__(self) -> str:
        return f"Primitive({self.name!r})"


NoneType = PrimitiveBase(name="NoneType")
UnknownType = PrimitiveBase(name="UnknownType")
ModuleType = PrimitiveBase(name="ModuleType")
IntegerType = PrimitiveBase(name="IntegerType")
FloatType = PrimitiveBase(name="FloatType")


@dataclass
class Pointer(TypeInfo):
    inner: TypeInfo


PRIMITIVE_TYPES = {
    str: Function(name="str", args=[], ret=Pointer(IntegerType)),
    int: Function(name="int", args=[], ret=IntegerType),
    float: Function(name="float", args=[], ret=FloatType),
    type(None): NoneType,
    None: NoneType,
}

ASTNode = ast.AST
ASTNode.dump = (lambda self: ast.dump(self))


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
            }:
                continue

            raise NotImplementedError(item)

        if (parent := self.item.parent) is not None:
            result = driver._ast_nodes[parent].item.scope.lookup(target, driver=driver)
        else:
            result = None

        if result is not None:
            return result

        # Fallback builtin lookup.
        builtin_type = getattr(builtins, target, null := object())

        if builtin_type is not null:
            return (
                Item(node=None, parent=None, kind=ty.ret, scope=self)
                if (ty := PRIMITIVE_TYPES.get(builtin_type, None)) is not None
                else None
            )


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


class NodeEntry(NamedTuple):
    item: Item
    span: SpanInfo

    @property
    def node(self) -> ASTNode:
        return self.item.node

    @classmethod
    def from_node(cls, node: ast.AST, *, module_name: str) -> "NodeEntry":
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
class MontyDriver:
    modules: Dict[str, ASTNode] = field(default_factory=dict)
    functions: Set[Function] = field(default_factory=set)

    _ast_nodes: Dict[ASTNode, NodeEntry] = field(default_factory=dict)

    def _resolve_type(self, ty: TypeInfo) -> TypeInfo:
        """Return a fully qualified type i.e. not a type-ref."""
        assert isinstance(ty, TypeInfo), repr(ty)

        while isinstance(ty, TypeRef):
            ty = self._ast_nodes[ty.other.node].item.kind

        return ty

    def typecheck(self, entry: NodeEntry):
        """Recursively typecheck some node entry."""

        def type_eq(lhs: TypeInfo, rhs: TypeInfo) -> bool:
            return self._resolve_type(lhs) == self._resolve_type(rhs)

        for item in entry.item.scope:
            item_node: ASTNode = item.node

            if item_node is entry.item.node:
                continue

            if isinstance(item_node, ast.FunctionDef):
                self.typecheck(entry=self._ast_nodes[item.node])
                continue

            if isinstance(item_node, ast.Return):
                assert isinstance(
                    entry.item.node, ast.FunctionDef
                ), "Found a return outside of a function!"
                expected = self._resolve_type(entry.item.kind.ret)
                actual = self._resolve_type(item.kind)

                if not type_eq(expected, actual):
                    span_info = self._ast_nodes[item.node].span
                    raise TypeError(
                        f"[{span_info.lineno}:{span_info.col_offset}]: Attempted to return with type {actual}, expected type {expected}"
                    )

            elif isinstance(item_node, ast.AugAssign):
                raise NotImplementedError()

            elif isinstance(item_node, ast.AnnAssign):
                expected = self._resolve_type(
                    self._ast_nodes[item.node.annotation].item.kind
                )
                actual = self._resolve_type(self._ast_nodes[item.node.value].item.kind)

                if not type_eq(expected, actual):
                    span_info = self._ast_nodes[item.node].span
                    raise TypeError(
                        f"[{span_info.lineno}:{span_info.col_offset}]: Expected type {expected} instead got {actual}"
                    )

            elif isinstance(item_node, (ast.Compare, ast.Gt, ast.Add, ast.Sub)):
                raise NotImplementedError()

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
            ast.While
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

        if item_node_type in {ast.Store, ast.Load, ast.arguments, ast.Gt}:
            return UnknownType

        elif item_node_type in {ast.Compare, ast.Add, ast.Sub}:
            if item_node_type is ast.Compare:
                if (len(item.node.ops), len(item.node.comparators)) != (1, 1):
                    span = self._ast_nodes[item.node]
                    raise SyntaxError(f"[{span.lineno}:{span.col_offset}]: Comparisson is too complex to solve.")

                op, comp = [*item.node.ops, *item.node.comparators]

                breakpoint()

            else:
                raise NotImplementedError()

            # * transform item into appropriate function call
            # * find matching function signature
            # * the type is then the output type of that signature.

            # Compare(left=Name(id='x', ctx=Load()), ops=[Gt()], comparators=[Constant(value=10)])
            return UnknownType

        elif item_node_type is ast.Call:
            func = item.scope.lookup(target=item.node.func.id, driver=self)

            if not isinstance(func.kind, Function):
                span_info = self._ast_nodes[item.node].span
                actual = func.kind

                raise TypeError(
                    f"[{span_info.lineno}:{span_info.col_offset}]: Type {actual} is not callable."
                )

            args = [
                self.infer(self._ast_nodes[item.node])
                for item in map(item.scope.lookup, item.node.args)
            ]

            assert all(arg is not None for arg in args)

            assert func.kind == Function(
                name=func.kind.name, args=args, ret=func.kind.ret
            )

            return func.kind.ret

        elif item_node_type is ast.arg:
            thing = item.scope.lookup(
                target=_validate_argument(item.node).annotation.id, driver=self
            )

            return thing.kind if thing is not None else None

        elif item_node_type is ast.Return:
            value = self._ast_nodes[item.node.value]
            return self.infer(entry=value)

        elif item_node_type is ast.FunctionDef:

            if not any(ast.iter_fields(item.node.args)):
                args = []
            else:
                f_args = item.node.args
                args = [
                    _validate_argument(arg)
                    for group in [f_args.posonlyargs, f_args.args, f_args.kwonlyargs]
                    for arg in group
                ]

            return_annotation = item.node.returns
            ret = NoneType

            if return_annotation is not None:
                node = self.infer(self._ast_nodes[return_annotation])
                ret = node.ret if isinstance(node, Function) else node

            return Function(name=item.node.name, args=args, ret=ret)

        elif item_node_type is ast.Name:
            if (
                (is_regular_assignment := isinstance(item.parent, ast.Assign))
                or isinstance(item.parent, ast.AnnAssign)
                and item.parent.annotation is not item.node
            ):
                # Type of the name depends on the type of the expression or the annotation.
                dependent_entry = self._ast_nodes[
                    item.parent.value
                    if is_regular_assignment
                    else item.parent.annotation
                ]

                return TypeRef(other=dependent_entry.item)

            elif isinstance(item.node.ctx, ast.Load):
                thing = item.scope.lookup(target=item.node.id, driver=self)
                return thing.kind if thing is not None else None

            else:
                assert isinstance(item.node.ctx, ast.Store)
                return UnknownType

        elif item_node_type is ast.Constant:
            return PRIMITIVE_TYPES[type(item.node.value)].ret

        elif item_node_type in (ast.AnnAssign, ast.Assign, ast.AugAssign, ast.While):
            return NoneType

        elif item_node_type is ast.Module:
            return ModuleType

        return None

    def compile(self, st: str, *, module_name: str = "__main__"):
        root_tree = ast.parse(st)
        assert isinstance(root_tree, ast.Module), f"{ast.dump(root_tree)=!r}"

        # TODO:
        #   const/comptime/macro logic should occur here so we can rewrite
        #   the tree, dropping any disqualified roots from further compilation
        #   in a "as if they never existed." style.

        self.modules[module_name] = root_tree
        nodes = self._ast_nodes

        WalkStream = Iterator[Tuple[int, ASTNode, ASTNode]]

        def postorder_walk(
            parent: Optional[ASTNode],
            node: ASTNode,
            *,
            _stack_depth: int = 0,
            pred: Callable[[ASTNode, ASTNode], bool],
        ) -> WalkStream:
            for child in ast.iter_child_nodes(node):
                depth = _stack_depth + pred(node, child) + pred(parent, node)
                yield from postorder_walk(node, child, _stack_depth=depth, pred=pred)
            else:
                yield (_stack_depth, parent, node)

        def check_depth(pred, succ) -> bool:
            return isinstance(pred, CAN_HAVE_SCOPE) and isinstance(succ, CAN_HAVE_SCOPE)

        def fold_leaves_into_root(root: ast.Module) -> NodeEntry:
            assert isinstance(root, ast.Module)

            traversal = postorder_walk(parent=None, node=root, pred=check_depth)

            stream = deque(traversal)
            depth, parent, node = stream.popleft()

            nodes[node] = entry = NodeEntry.from_node(
                node=node, module_name=module_name
            )

            bucket: Dict[int, List[Item]] = {depth: [entry.item]}

            while stream:
                n, parent, node = stream.popleft()

                nodes[node] = entry = NodeEntry.from_node(
                    node=node, module_name=module_name
                )

                if n == depth:
                    bucket[n].append(entry.item)

                elif n == (depth - 1):
                    entry.item.scope.elements.extend(bucket[depth])

                    for item in bucket[depth]:
                        if not isinstance(item.node, CAN_HAVE_SCOPE):
                            item.scope = entry.item.scope

                        item.parent = entry.item.node

                    if n in bucket:
                        bucket[n].append(entry.item)
                    else:
                        bucket[n] = [entry.item]

                    depth = n

                else:
                    assert n == (depth + 1), (n, depth)
                    bucket[n] = [entry.item]
                    depth = n

            return self._ast_nodes[self.modules[module_name]].item

        root_item = fold_leaves_into_root(root=root_tree)

        functions = {
            item.node.name: item
            for item in root_item.scope
            if isinstance(item.node, ast.FunctionDef)
        }

        for (node, entry) in nodes.items():
            if (inferred := self.infer(entry)) is not None:
                entry.item.kind = inferred
            else:
                assert False, f"Failed to infer type for {ast.dump(entry.item.node)=!r}"

            if isinstance(node, (ast.Import, ast.ImportFrom)):
                raise NotImplementedError("Import logic is missing.")

        self.typecheck(entry=nodes[root_tree])


test_input = """
def pi() -> float:
    return 3.14

def f(x: int) -> float:
    x = 0
    return pi()

def x():
    x: int = 0
""".strip()

driver = MontyDriver()
driver.compile(test_input)
