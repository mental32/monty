import ast
from dataclasses import dataclass, field
from functools import singledispatchmethod
import sys
from sys import breakpointhook
from typing import Optional, Set, Dict, TYPE_CHECKING, Union

from .item import Item
from .utils import ASTNode, TypeInfo
from .ty import (
    FloatType,
    Function,
    KlassType,
    IntegerType,
    BoolType,
    TypeRef,
    UnknownType,
)

if TYPE_CHECKING:
    from monty import Module


@dataclass
class Context:
    """Big blob of compilation state."""

    modules: Dict[str, "Module"] = field(default_factory=dict)
    functions: Set[Function] = field(default_factory=set)
    klass_types: Set[KlassType] = field(default_factory=set)

    ast_nodes: Dict[ASTNode, Item] = field(default_factory=dict)
    source_map: Dict[ast.Module, str] = field(default_factory=dict)
    builtin_types: Dict[TypeInfo, ASTNode] = field(default_factory=dict)

    def __post_init__(self):
        self.autoimpls = autoimpls = {}

        autoimpls[BoolType] = []
        autoimpls[IntegerType] = [
            Function(name="__add__", args=[IntegerType, IntegerType], ret=IntegerType),
            Function(name="__mul__", args=[IntegerType, IntegerType], ret=IntegerType),
            Function(name="__sub__", args=[IntegerType, IntegerType], ret=IntegerType),
            Function(name="__eq__", args=[IntegerType, IntegerType], ret=BoolType),
            Function(name="__ne__", args=[IntegerType, IntegerType], ret=BoolType),
            Function(name="__gt__", args=[IntegerType, IntegerType], ret=BoolType),
            Function(name="__lt__", args=[IntegerType, IntegerType], ret=BoolType),
            Function(name="__ge__", args=[IntegerType, IntegerType], ret=BoolType),
            Function(name="__le__", args=[IntegerType, IntegerType], ret=BoolType),
        ]

        autoimpls[FloatType] = [
            Function(name="__add__", args=[FloatType, FloatType], ret=IntegerType),
            Function(name="__mul__", args=[FloatType, FloatType], ret=IntegerType),
            Function(name="__sub__", args=[FloatType, FloatType], ret=IntegerType),
            Function(name="__eq__", args=[FloatType, FloatType], ret=BoolType),
            Function(name="__ne__", args=[FloatType, FloatType], ret=BoolType),
            Function(name="__gt__", args=[FloatType, FloatType], ret=BoolType),
            Function(name="__lt__", args=[FloatType, FloatType], ret=BoolType),
            Function(name="__ge__", args=[FloatType, FloatType], ret=BoolType),
            Function(name="__le__", args=[FloatType, FloatType], ret=BoolType),
        ]

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

    def resolve_type(self, inty: Union[TypeInfo, ASTNode]) -> TypeInfo:
        """Return a fully qualified type i.e. not a type-ref."""

        assert isinstance(inty, (TypeInfo, ASTNode))

        if isinstance(inty, ASTNode):
            ty = self.ast_nodes[inty].kind
        else:
            assert isinstance(inty, TypeInfo)
            ty = inty

        assert isinstance(ty, TypeInfo), repr(ty)

        while isinstance(ty, TypeRef):
            ty = self.ast_nodes[ty.other.node].kind

        if isinstance(ty, KlassType):
            inverted_bultin_map = {
                node: prim for prim, node in self.builtin_types.items()
            }

            if ty.node in inverted_bultin_map:
                ty = inverted_bultin_map[ty.node]

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
