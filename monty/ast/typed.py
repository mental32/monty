import ast
from dataclasses import dataclass, field
from typing import Dict, Union, Optional, Callable, Any, Iterator, Tuple

from ..mir import Scope, RawType, Import
from . import _silent_field

__all__ = ("TypedAST",)


@dataclass
class TypedAST:
    """Some CPython AST object with type information attached."""

    tree: ast.AST
    scope: Scope = field(default_factory=Scope)

    _nodes: Dict[int, Union[RawType, "TypedAST"]] = _silent_field(default_factory=dict)
    _ledger: Dict[ast.AST, int] = _silent_field(default_factory=dict)
    _ident: int = _silent_field(default=0)

    def __iter__(self):
        inverted = {value: key for key, value in self._ledger.items()}
        stream = sorted(tuple(self._nodes.items()))

        for ident, kind in stream:

            if not isinstance(kind, TypedAST):
                assert ident in inverted, f"{ident!r} not in {inverted!r} ({kind=!r})"
                node = inverted[ident]
            else:
                node = kind.tree

            yield (ident, node, kind)

    def get(self, node: ast.AST) -> Optional[Union[RawType, "TypedAST"]]:
        return self._nodes.get(self._ledger.get(node, -1), None)

    def find(self, predicate: Callable[[int, ast.AST, Any], bool]) -> Iterator[Tuple[int, ast.AST, Any]]:
        for item in self:
            if predicate(item):
                yield item

    def add(self, node: ast.AST, kind: Union[RawType, "TypedAST"]):
        assert self._ident not in self._nodes

        self._nodes[self._ident] = kind
        self._ledger[node] = self._ident
        self._ident += 1
