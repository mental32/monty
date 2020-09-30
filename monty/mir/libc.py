import ctypes
from dataclasses import dataclass, field
from pathlib import Path
from typing import Tuple as _Tuple, Set, List

from monty.language import Item
from monty.typing import Primitive, Callable, TypeInfo, Tuple

__all__ = ("ExternalFunction", "CDLL", "LIBC_FUNCTIONS")


@dataclass
class ExternalFunction:
    """Represents an externally defined function."""

    name: str
    signature: _Tuple[List[TypeInfo], TypeInfo]

    def __hash__(self) -> int:
        return hash(self.name)

    def __repr__(self) -> str:
        args = ", ".join(map((lambda arg: arg.name), self.signature[0]))
        signature = f"{self.name}({args}) -> {self.signature[1].name}"
        return f"<ExternalFunction: {signature=!r}>"

    def into_item(self, unit: "CompilationUnit") -> Item:
        # fmt: off
        args, out = self.signature
        # fmt: on

        func = Callable()
        parameters = Tuple(inner=[unit.tcx.insert(kind) for kind in args])
        func.parameters = unit.tcx.insert(parameters)
        func.output = unit.tcx.insert(out)

        ty = unit.tcx.insert(func)
        return Item(ty=ty, node=None, unit=unit)


@dataclass
class CDLL:
    """Represents a DLL with a C calling convention."""

    name: str
    path: Path
    functions: Set[ExternalFunction] = field(default_factory=set)


fopen = ExternalFunction(
    name="fopen", signature=([Primitive.I32, Primitive.StrSlice], Primitive.Int)
)

fwrite = ExternalFunction(
    name="fwrite",
    signature=(
        [Primitive.I32, Primitive.I32, Primitive.I32, Primitive.I32],
        Primitive.Int,
    ),
)

fflush = ExternalFunction(name="fflush", signature=([Primitive.I32], Primitive.I32))

LIBC_FUNCTIONS = {fopen, fwrite, fflush}
