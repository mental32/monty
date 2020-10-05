import ctypes
from dataclasses import dataclass, field
from pathlib import Path
from typing import Tuple as _Tuple, Set, List

from monty.language import Item, PhantomModule
from monty.typing import primitives, Callable, TypeInfo, Tuple

__all__ = ("ExternalFunction", "CDLL", "LIBC_FUNCTIONS", "LIBC_MODULE")


@dataclass
class ExternalFunction:
    """Represents an externally defined function."""

    name: str
    signature: _Tuple[List[TypeInfo], TypeInfo]

    def __hash__(self) -> int:
        return hash(self.name)

    def __repr__(self) -> str:
        args = ", ".join(map(repr, self.signature[0]))
        signature = f"{self.name}({args}) -> {self.signature[1]!r}"
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
    name="fopen", signature=([primitives.Int32(), primitives.StrSlice()], primitives.Integer())
)

fwrite = ExternalFunction(
    name="fwrite",
    signature=(
        [primitives.Int32(), primitives.Int32(), primitives.Int32(), primitives.Int32()],
        primitives.Integer(),
    ),
)

fflush = ExternalFunction(name="fflush", signature=([primitives.Int32()], primitives.Int32()))

LIBC_FUNCTIONS = {fopen, fwrite, fflush}

LIBC_MODULE = PhantomModule(
    name="libc", namespace={extern.name: extern for extern in LIBC_FUNCTIONS}
)
