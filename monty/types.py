import typing
from dataclasses import dataclass
from enum import IntEnum, auto
from typing import Type, Tuple, Union, Any

__all__ = ("OpaqueType", "TypedTuple")


class OpaqueType(IntEnum):
    Integer = auto()
    String = auto()
    Tuple = auto()
    FnPtr = auto()
    Kind = auto()
    Unknown = auto()
    ForwardRef = auto()

    @staticmethod
    def from_object(obj: Any) -> "OpaqueType":
        if obj is type:
            return OpaqueType.Kind

        ty = type(obj)

        if ty is int:
            return OpaqueType.Integer

        if ty is str:
            return OpaqueType.String

        if ty is tuple:
            return OpaqueType.Tuple

        return OpaqueType.Unknown

@dataclass
class TypedTuple:
    form: Tuple[Union[Type, "monty.RawType"]]

    @classmethod
    def from_object(cls: Type["TypedTuple"], obj: Tuple) -> "TypedTuple":
        form = tuple(type(field) for field in obj)

        if any(field is type for field in form):
            raise TypeError("Cant include types in tuple!")

        return cls(form=form)

    def __hash__(self) -> int:
        return hash(self.form)
