import ast
import sys
import typing
from collections import deque
from dataclasses import dataclass, field
from functools import cached_property, lru_cache, partial, singledispatch
from pathlib import Path
from typing import (
    TYPE_CHECKING,
    Callable,
    Dict,
    Iterator,
    List,
    NamedTuple,
    Optional,
    Set,
    Tuple,
    Union,
)

from .ty import TypeInfo, UnknownType
from .utils import (
    CAN_HAVE_SCOPE,
    LEGAL_NODE_TYPES,
    NULL_SCOPE,
    ASTNode,
    collapse_attribute,
)

if TYPE_CHECKING:
    from .context import Context


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
            return "\n".join(splat[self.lineno - 1 : self.end_lineno][:lines])
        else:
            return splat[self.lineno - 1][self.col_offset : self.end_col_offset]

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
    span: SpanInfo = field(repr=False, default_factory=SpanInfo)
    node: ASTNode = field(default_factory=(lambda: ast.Pass()))
    parent: Optional[ASTNode] = field(default=None, repr=False)
    kind: TypeInfo = field(default=UnknownType)
    scope: "Scope" = field(
        init=False, repr=False, default=typing.cast("Scope", NULL_SCOPE)
    )

    def __hash__(self) -> int:
        return hash(self.node)

    # Constructors

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

        scope = Scope(parent_node=None)
        item = cls(node=node, span=span_info)
        scope.item = item
        item.scope = scope
        return item

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

        local_entry.scope.item = local_entry

        if parent is None:
            return (local_entry, {})

        local_nodes: Dict[ASTNode, Item] = {node: local_entry}
        dangling: Dict[ASTNode, List[Item]] = {}
        bucket: Dict[int, List[Item]] = {depth: [local_entry]}

        def flush(*, bucket, depth, local_entry, dangling, node, p) -> int:
            for item in dangling.pop(node, []) + bucket[depth]:
                assert isinstance(item, Item)

                local_entry.scope.elements.append(item)

                if not isinstance(item.node, CAN_HAVE_SCOPE):
                    old_scope = item.scope
                    item.scope = local_entry.scope
                    del old_scope

                item.parent = local_entry.node
                item.scope.parent_node = local_entry.node

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
                bucket[n].append(local_entry)

            else:
                if remaining := bucket[depth]:
                    depth = flush(
                        bucket={depth: remaining},
                        depth=depth,
                        local_entry=local_entry,
                        dangling=dangling,
                        node=node,
                        p=p,
                    )

                bucket[n] = [local_entry]
                depth = n
        else:
            assert not dangling, repr(dangling)
            assert not bucket, repr(bucket)

        root_entry = local_nodes[root]

        return (root_entry, local_nodes)

    # Helpers

    def get_direct_parent_node(self, ctx: "Context") -> Optional[ASTNode]:
        if (cached := getattr(self, "_direct_parent_node", None)) is not None:
            return cached

        if isinstance(self.node, ast.Module):
            return self.node

        parent = (
            ctx.modules[self.span.module_name].root
            if self.parent is None
            else ctx.ast_nodes[self.parent]
        )

        assert parent.scope is not None

        @singledispatch
        def predicate(node: ASTNode, cur: Item) -> bool:
            raise NotImplementedError(
                f"I dont know how to get the direct parent node of this type of node {node}"
            )

        @predicate.register
        def _pred_arg(node: ast.arg, cur: Item) -> bool:
            return isinstance(cur.node, ast.FunctionDef) and node in ast.walk(
                cur.node.args
            )

        @predicate.register
        def _pred_assign(node: ast.Name, cur: Item) -> bool:
            return (
                isinstance(node.ctx, ast.Store)
                and (
                    isinstance(asn := cur.node, ast.Assign)
                    and len(asn.targets) == 1
                    and asn.targets[0] is self.node
                )
                or (
                    isinstance(ann := cur.node, ast.AnnAssign)
                    and ann.target is self.node
                )
            )

        @predicate.register(ast.Gt)
        @predicate.register(ast.Lt)
        @predicate.register(ast.NotEq)
        @predicate.register(ast.Eq)
        @predicate.register(ast.LtE)
        @predicate.register(ast.GtE)
        def _pred_logic(node, cur: Item) -> bool:
            return isinstance(comp := cur.node, ast.Compare) and node in ast.walk(comp)

        @predicate.register(ast.Add)
        @predicate.register(ast.Sub)
        @predicate.register(ast.Mult)
        def _pred_binop(node, cur: Item) -> bool:
            return isinstance(binop := cur.node, ast.BinOp) and binop.op is self.node

        @predicate.register
        def _pred_import(node: ast.alias, cur: Item) -> bool:
            return (
                isinstance(imp := cur.node, (ast.ImportFrom, ast.Import))
                and self.node in imp.names
            )

        @predicate.register
        def _pred_funcdef(node: ast.Return, cur: Item) -> bool:
            return isinstance(cur.node, (ast.FunctionDef)) and self.node in ast.walk(
                cur.node
            )

        @predicate.register(ast.FunctionDef)
        @predicate.register(ast.ClassDef)
        def _pred_scopeable(node, cur: Item) -> bool:
            return isinstance(cur.node, CAN_HAVE_SCOPE) and self in cur.scope

        pred = partial(predicate.dispatch(self.node.__class__), self.node)

        for parent in filter(pred, [parent, *parent.scope.elements]):
            setattr(self, "_direct_parent_node", parent.node)
            return parent.node
        else:
            return None


@dataclass
class Scope:
    item: Optional[Item] = field(default=None)
    parent_node: Optional[ASTNode] = field(default=None)
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

        result = None

        if (
            (parent := self.parent_node) is not None
            or self.item is not None
            and (parent := self.item.node)
        ):
            scope = ctx.ast_nodes[parent].scope
            if scope is not self:
                assert scope is not None
                result = scope.lookup(target, ctx=ctx)

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
