import ast
from dataclasses import dataclass, field
from typing import List, Union
from functools import lru_cache

__all__ = ("ImportDecl",)

# import x.y as z
# 
# qualname = ("x", "y")
# access_path = "z"
#
# scope.lookup("z") == ImportDecl(qualname = ("x", "y"))
@dataclass
class ImportDecl:
    node: ast.alias = field(repr=False)
    parent: Union[ast.Import, ast.ImportFrom] = field(repr=False)

    def __repr__(self) -> str:
        return f"ImportDec({ast.dump(self.node)=!r})"

    @property
    @lru_cache(maxsize=1)
    def qualname(self) -> List[str]:
        return self.node.name.split(".")

    @property
    @lru_cache(maxsize=1)
    def realname(self) -> str:
        return self.node.asname or self.node.name

    def __hash__(self) -> int:
        return hash(self.node)

    def __str__(self) -> str:
        return self.realname
