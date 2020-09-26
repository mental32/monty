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
                ImportDecl(node=node, parent=item.node)
                for item in self.builder.root.scope.items
                for node in item.node.names
                if item.ty is Primitive.Import
            }

        return (
            produce_import_decls()
            if self.builder is not None
            else set()
        )

    def getattr(self, attribute: str):
        assert self.builder is not None, "How are we supposed to reference the root without it??"

        results = self.builder.root.scope.lookup(attribute)

        if results is None:
            # Try checking the module's path.
            #
            # for instance:
            #
            #   from x import y
            #
            # Could have a structure like
            #
            #   x/y/__init__.py
            #
            # At that point we want the `y` module object.
            raise NotImplementedError()
        else:
            assert results, "no..."
            return results.pop()
