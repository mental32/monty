

import ast
from dataclasses import InitVar, dataclass, field
from functools import partial, singledispatchmethod
from typing import Any, Callable, List, Optional, TYPE_CHECKING, Tuple, Union
import typing

from ..utils import ASTNode, LEGAL_NODE_TYPES, TypeInfo, collapse_attribute, is_visible, panic

if TYPE_CHECKING:
    from ..context import Context
    from ..item import Item

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
        if self.name == "int":
            return 64 // 8

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

    def __repr__(self) -> str:
        return f"<class: {self.name!s}>"

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
