from enum import IntEnum, auto
from typing import Optional

from . import TypeInfo

__all__ = ("Primitive",)

#     LValue = auto()
#     Return = auto()


class Primitive(TypeInfo):
    """Primitive types that do not compound or have any special semantics (apart from `Unknown`)."""

    def __repr__(self) -> str:
        return type(self).__name__.title()

    def as_str(self, _) -> str:
        return "primitive"

    def size(self) -> Optional[int]:
        return None

    def __eq__(self, other):
        return type(other) == type(self)

    @staticmethod
    def from_builtin_type(ty: type) -> Optional["Primitive"]:
        _PRIMITIVE_BUILTIN_MAP = {
            bool: Boolean,
            int: Integer,
            str: StrSlice,
            type(None): NoneType,
        }

        return _PRIMITIVE_BUILTIN_MAP.get(ty, None)()

class Unknown(Primitive):
    def as_str(self, _) -> str:
        return "Unknown"


class ImportType(Primitive):
    def as_str(self, _) -> str:
        return "import"

class ModuleType(Primitive):
    def as_str(self, _) -> str:
        return "ModuleType"

class StrSlice(Primitive):
    def as_str(self, _) -> str:
        return "StrSlice"

class NoneType(Primitive):
    def as_str(self, _) -> str:
        return "NoneType"

class Boolean(Primitive):
    def as_str(self, _) -> str:
        return "boolean"

    def size(self) -> Optional[int]:
        return 1

# Number types

class Number(Primitive):
    def as_str(self, _) -> str:
        return "number"

    def size(self) -> Optional[int]:
        return 0


class Integer(Number):
    def as_str(self, _) -> str:
        return "integer"

    def size(self) -> Optional[int]:
        return 0


class Int64(Number):
    def as_str(self, _) -> str:
        return "int64"

    def size(self) -> Optional[int]:
        return 8


class Int32(Number):
    def as_str(self, _) -> str:
        return "int32"

    def size(self) -> Optional[int]:
        return 4
