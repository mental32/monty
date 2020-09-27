from dataclasses import dataclass, field
from pathlib import Path
from typing import Optional, Set, Dict, Any, TYPE_CHECKING
from functools import lru_cache

from . import ImportDecl
from monty.typing import Primitive

if TYPE_CHECKING:
    from monty.language import Item

__all__ = ("Module", "MontyModule")


@dataclass
class Module:
    """Represents an imported module."""

    # Fully qualified name
    name: str

    # Path on disk
    path: Path

    # Builder use to lower into MIR
    builder: Optional["monty.mir.ModuleBuilder"] = field(default=None, repr=False)

    # Additional namespace hooks.
    namespace: Dict[str, Any] = field(default_factory=dict, repr=False)

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

        return produce_import_decls() if self.builder is not None else set()

    def getattr(self, attribute: str) -> Any:
        # print(self, attribute, self.namespace)
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
        else:
            assert results, "no..."
            return results.pop()

    def as_item(self) -> "Item":
        return self.builder.root


@dataclass
class MontyModule(Module):
    unit: "CompilationUnit" = field(default=None)
    libc: "CDLL" = field(default=None)

    __namespace: dict = field(init=False, repr=False, default_factory=dict)

    def __post_init__(self):
        from monty.mir import CDLL, LIBC_FUNCTIONS
        from monty.language import Item

        self.libc = libc_dll = CDLL(name="libc", path=None)
        libc_dll.functions.update(LIBC_FUNCTIONS)

        module = Module(name="__libc", path=None, namespace={
            extern.name: extern for extern in LIBC_FUNCTIONS
        })

        self.__namespace["libc"] = Item(ty=Primitive.Module, node=module, unit=self.unit)

    def getattr(self, attribute: str) -> Any:
        return self.__namespace.get(attribute, None) or getattr(self.unit, attribute)

    def as_item(self) -> "Item":
        from . import Item
        from ..typing import Primitive

        return Item(ty=Primitive.Module, node=self)
