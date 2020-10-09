import ast
import builtins
from dataclasses import dataclass, field
from os import name
from pprint import pformat
from typing import Dict, Generator, Iterator, List, NamedTuple, Optional, Set, Tuple, Type
from functools import cached_property


class TypeInfo:
    pass


@dataclass
class KlassType(TypeInfo):
    name: str


@dataclass
class Function(TypeInfo):
    name: str
    args: List[TypeInfo]
    ret: TypeInfo


@dataclass
class TypeRef(TypeInfo):
    other: "Item"


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

@dataclass
class Pointer(TypeInfo):
    inner: TypeInfo


PRIMITIVE_TYPES = {
    str: Function(name="str", args=[], ret=Pointer(IntegerType)),
    int: Function(name="int", args=[], ret=IntegerType),
    float: KlassType(name="float"),
    type(None): NoneType,
}

ASTNode = ast.AST


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


class SpanInfo(NamedTuple):
    file_name: str
    lineno: int
    col_offset: int
    end_lineno: int
    end_col_offset: int


class NodeEntry(NamedTuple):
    item: Item
    span: SpanInfo

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

        if isinstance(
            node, (ast.Module, ast.ClassDef, ast.FunctionDef, ast.AsyncFunctionDef)
        ):
            item.scope = Scope(item=item)

        return cls(
            item=item,
            span=span_info,
        )


@dataclass
class MontyDriver:
    modules: Dict[str, Dict[ASTNode, NodeEntry]] = field(default_factory=dict)

    def infer(self, entry: NodeEntry) -> Optional[TypeInfo]:
        """Attempt to infer the type of some item."""
        item = entry.item

        if item.kind is not None or item.kind is UnknownType:
            # Happy path!
            return item.kind

        item_node_type = type(item.node)

        UNTYPED_NODE_TYPES = frozenset(
            {
                ast.Store,
                ast.Load,
                ast.arguments,
            }
        )

        if item_node_type in UNTYPED_NODE_TYPES:
            return UnknownType

        elif item_node_type is ast.FunctionDef:
            return_annotation = item.node.returns

            if return_annotation is None:
                ret = NoneType
            else:
                ret = self.infer(self.modules[entry.span.file_name][return_annotation])

            if isinstance(ret, Function):
                ret = ret.ret

            return Function(name=item.node.name, args=[], ret=ret)

        elif item_node_type is ast.Name:
            if isinstance(item.parent, ast.AnnAssign) and item.parent.annotation is not item.node:
                # Type of the name depends on the annotation.
                dependent_entry = self.modules[entry.span.file_name][item.parent.annotation]
                return TypeRef(other=dependent_entry.item)

            elif isinstance(item.parent, ast.Assign):
                # Type of the name depends on the type of the expression.
                dependent_entry = self.modules[entry.span.file_name][item.parent.value]
                return TypeRef(other=dependent_entry.item)

            else:
                return PRIMITIVE_TYPES[getattr(builtins, item.node.id)].ret

        elif item_node_type is ast.Constant:
            return PRIMITIVE_TYPES[type(item.node.value)]

        elif item_node_type in (ast.AnnAssign, ast.Assign):
            return NoneType

        elif item_node_type is ast.Module:
            return ModuleType

        return None

    def compile(self, st: str, *, module_name: str = "__main__"):
        root_tree = ast.parse(st)
        assert isinstance(root_tree, ast.Module), f"{ast.dump(root_tree)=!r}"

        self.modules[module_name] = nodes = {}

        def postorder_walk(
            parent: Optional[ASTNode], node: ASTNode
        ) -> Iterator[Tuple[ASTNode, ASTNode]]:
            for child in ast.iter_child_nodes(node):
                yield from postorder_walk(node, child)
            else:
                yield (parent, node)

        current_scope: Optional[Scope] = None
        leftover: Set[Item] = set()

        for (
            parent,
            node,
        ) in postorder_walk(None, root_tree):
            nodes[node] = entry = NodeEntry.from_node(
                node=node, module_name=module_name
            )

            entry.item.parent = parent

            # Scope resolution and assignment logic.
            #
            # Every AST node has an "Item" counterpart
            # An Item is a "fat" AST node in that it contains:
            #
            #  * The AST node itself
            #  * The type of the item/ast node (default to None when unprocessed)
            #  * The scope of the item/ast node.
            #
            if (scope := entry.item.scope) is not None:
                if current_scope is not None:
                    # Common case, multiple scopes.
                    scope.elements.append(current_scope.item)
                else:
                    # Rare case (should happen once per module.)
                    #
                    # Case of the first scope appearing, re-process the
                    # leftover items and bind the correct scope.
                    for item in leftover:
                        item.scope = scope

                    scope.elements.extend(leftover)
                    leftover = set()

                current_scope = scope

            elif current_scope is not None:
                # Common case, the item itself does not introduce a new scope.
                # just bind it to whatever the current scope is.
                entry.item.scope = current_scope

            else:
                # Unfortunately for the first scope all items are marked as
                # leftover until we hit a ceiling, at which point we re-process
                # them and assign the correct scope.
                leftover.add(entry.item)

        for node, entry, in nodes.items():
            if (inferred := self.infer(entry)) is not None:
                entry.item.kind = inferred
            else:
                assert False, f"Failed to infer type for {entry=!r}"

            print(f"{entry.item}\n{pformat(entry.item.scope.elements)!s}\n\n")

        self.typecheck(entry=nodes[root_tree])


test_input = """
def f():
    a: int = "foo"
"""

driver = MontyDriver()
driver.compile(test_input)
