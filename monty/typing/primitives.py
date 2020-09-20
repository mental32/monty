from enum import IntEnum, auto
from typing import Optional

from . import TypeInfo

__all__ = ("Primitive",)


class Primitive(TypeInfo, IntEnum):
    """Primitive types that do not compound or have any special semantics (apart from `Unknown`)."""

    Unknown = 0  # NOTICE! Primitive.Unknown is a special cased type always slotted to 0

    Bool = auto()
    Number = auto()
    LValue = auto()
    Module = auto()
    Return = auto()
    String = auto()

    Nothing = auto()
    None_ = auto()

    Integer = auto()
    I64 = auto()
    I32 = auto()

    def reconstruct(self, tcx) -> str:
        return self.name

    def size(self) -> int:
        """Get the size of this type in bytes."""
        return _PRIMITIVE_SIZE_MAP.get(self, 0)

    @staticmethod
    def from_builtin_type(ty: type) -> Optional["Primitive"]:
        return _PRIMITIVE_BUILTIN_MAP.get(ty, None)


_PRIMITIVE_BUILTIN_MAP = {
    bool: Primitive.Bool,
    int: Primitive.Integer,
    str: Primitive.String,
    type(None): Primitive.None_,
}

_PRIMITIVE_SIZE_MAP = {
    Primitive.Bool: 1,
    Primitive.I64: 8,
    Primitive.I32: 4,
    Primitive.Integer: 4,
    Primitive.None_: 1,
    Primitive.Nothing: 0,
}
