from dataclasses import dataclass, field
from pathlib import Path
from typing import Optional, Set
from functools import lru_cache

from . import ImportDecl

__all__ = ("Module",)


@dataclass
class Module:
    """Represents an imported module."""

    # Fully qualified name
    name: str

    # Path on disk
    path: Path

    # Builder use to lower into MIR
    builder: Optional["monty.mir.ModuleBuilder"] = field(default=None, repr=False)

    @property
    def imports(self) -> Set[ImportDecl]:
        from ..typing import Primitive

        def produce_import_decls():
            return {
                ImportDecl(node=node)
                for item in self.builder.root.scope.items
                for node in item.node.names
                if item.ty is Primitive.Import
            }

        return (
            produce_import_decls()
            if self.builder is not None
            else set()
        )
