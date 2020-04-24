from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from enum import IntEnum, auto

import monty
from monty.errors import TypeCheckError

__all__ = ("TypeId", "TypeInfo", "Primitive", "List", "Callable", "Ref")

TypeId = int


class TypeInfo:
    """Base class for all comprehensible types."""

    def reconstruct(self, tcx: "InferenceEngine") -> str:
        """Produce a locally constructed representation of the type."""
        raise NotImplementedError("Failed to implement reconstruct.")


class Primitive(TypeInfo, IntEnum):
    """Primitive types that do not compound or have any special semantics (apart from `Unknown`)."""

    Unknown = 0  # NOTICE! Primitive.Unknown is a special cased type always slotted to 0

    Bool = auto()
    Number = auto()
    LValue = auto()
    Module = auto()
    Return = auto()

    def reconstruct(self, tcx) -> str:
        return self.name


@dataclass
class List(TypeInfo):
    """List are monomorphic, but inference allows dynamic creation of sum types for the inner kind."""

    kind: TypeId

    def reconstruct(self, tcx: "InferenceEngine") -> str:
        return f"List[{tcx.reconstruct(self.kind)}]"


@dataclass
class Callable(TypeInfo):
    """Functions, lambda's, classes, etc...Anything implementing `__call__`."""

    parameters: TypeId = field(default=Primitive.Unknown)
    output: TypeId = field(default=Primitive.Unknown)

    def reconstruct(self, tcx: "InferenceEngine") -> str:
        return f"Callable[{tcx.reconstruct(self.parameters)}, {tcx.reconstruct(self.output)}]"


@dataclass
class Ref(TypeInfo):
    """A reference type is used as a form of indirection when unifying types in the inference engine."""

    target: TypeId

    def reconstruct(self, tcx):
        return tcx.reconstruct(self.target)
