from abc import ABC, abstractproperty, abstractmethod
from dataclasses import dataclass, field
from pathlib import Path
from typing import Optional, Set, Dict, Any, TYPE_CHECKING
from functools import lru_cache

from . import ImportDecl

if TYPE_CHECKING:
    from monty.language import Item
    from monty.mir import ModuleBuilder

__all__ = ("Module", "PhantomModule", "AbstractModule")


@dataclass
class AbstractModule(ABC):
    """Abstract mixin used for module types."""

    # Fully qualified name
    name: str

    # Path on disk
    path: Optional[Path] = field(default=None)

    # Builder use to lower into MIR
    builder: Optional["ModuleBuilder"] = field(default=None, repr=False)

    # Additional namespace hooks.
    namespace: Dict[str, Any] = field(default_factory=dict, repr=False)

    @property
    def imports(self) -> Set[ImportDecl]:
        from ..typing import primitives

        def produce_import_decls():
            return {
                ImportDecl(node=node, parent=item.node)
                for item in self.builder.root.scope.items
                for node in item.node.names
                if isinstance(item.ty, primitives.ImportType)
            }

        return produce_import_decls() if self.builder is not None else set()

    @abstractmethod
    def getattr(
        self, attribute: str, *, unit: Optional["CompilationUnit"] = None
    ) -> Any:
        """Imitate an `object.__getattr__`"""

    @abstractmethod
    def as_item(self) -> "Item":
        """Get the module as a language item."""


class Module(AbstractModule):
    """Represents a compiled module."""

    def getattr(
        self, attribute: str, *, unit: Optional["CompilationUnit"] = None
    ) -> Any:
        if (obj := self.namespace.get(attribute, None)) is not None:
            return obj

        assert (
            self.builder is not None
        ), "How are we supposed to reference the root without it??"

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

        assert results
        return results.pop()

    def as_item(self) -> "Item":
        return self.builder.root


class PhantomModule(AbstractModule):
    """Phantom modules are used to describe legitimate modules without compiling anything."""

    def getattr(
        self, attribute: str, *, unit: Optional["CompilationUnit"] = None
    ) -> Any:
        return self.namespace.get(attribute, None) or (
            unit is not None and getattr(unit, attribute)
        )

    def as_item(self) -> "Item":
        from . import Item
        from ..typing import primitives

        return Item(ty=primitives.ModuleType(), node=self)
