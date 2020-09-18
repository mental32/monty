from dataclasses import dataclass, field
from typing import TYPE_CHECKING

from . import Primitive, TypeId, TypeInfo

if TYPE_CHECKING:
    from monty.typing import TypeContext


@dataclass
class List(TypeInfo):
    """List are monomorphic, but inference allows dynamic creation of sum types for the inner kind."""

    kind: TypeId

    def reconstruct(self, tcx: "TypeContext") -> str:
        return f"List[{tcx.reconstruct(self.kind)}]"


@dataclass
class Callable(TypeInfo):
    """Functions, lambda's, classes, etc...Anything implementing `__call__`."""

    parameters: TypeId = field(default=Primitive.Unknown)
    output: TypeId = field(default=Primitive.Unknown)

    def reconstruct(self, tcx: "TypeContext") -> str:
        return f"Callable[{tcx.reconstruct(self.parameters)}, {tcx.reconstruct(self.output)}]"


@dataclass
class Ref(TypeInfo):
    """A reference type is used as a form of indirection when unifying types in the inference engine."""

    target: TypeId

    def reconstruct(self, tcx: "TypeContext"):
        return tcx.reconstruct(self.target)
