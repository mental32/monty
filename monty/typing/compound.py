import typing
from dataclasses import dataclass, field
from typing import TYPE_CHECKING

from . import Primitive, TypeId, TypeInfo

if TYPE_CHECKING:
    from monty.typing import TypeContext

@dataclass
class Tuple(TypeInfo):
    """A tuple, which is essentially a struct with anonymous fields."""
    inner: typing.Optional[typing.List[TypeId]] = field(default=None)

    def as_str(self, tcx: "TypeContext") -> str:
        inner = ", ".join(map(tcx.reconstruct, self.inner))
        return f"Tuple[{inner}]"


@dataclass
class List(TypeInfo):
    """List are monomorphic, but inference allows dynamic creation of sum types for the inner kind."""

    kind: TypeId

    def as_str(self, tcx: "TypeContext") -> str:
        return f"List[{tcx.reconstruct(self.kind)}]"


@dataclass
class Callable(TypeInfo):
    """Functions, lambda's, classes, etc...Anything implementing `__call__`."""

    parameters: TypeId = field(default=Primitive.Unknown)
    output: TypeId = field(default=Primitive.Unknown)

    def as_str(self, tcx: "TypeContext") -> str:
        return f"Callable[{tcx.reconstruct(self.parameters)}, {tcx.reconstruct(self.output)}]"


@dataclass
class PhantomRef(TypeInfo):
    """A reference type is used as a form of indirection when unifying types in the inference engine."""

    target: TypeId

    def as_str(self, tcx: "TypeContext"):
        return tcx[self.target].as_str(tcx)

@dataclass
class Pointer(TypeInfo):
    """A generic pointer type."""

    ty: TypeId

    def as_str(self, tcx: "TypeContext"):
        return f"Pointer({tcx[self.ty].as_str(tcx)})"

    def size(self, *, bits: int = 64) -> int:
        assert bits in {32, 64}, f"{bits=!r}"
        return {
            64: 8,
            32: 4,
        }[bits]
