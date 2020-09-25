import ast
from dataclasses import dataclass
from typing import List
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
    node: ast.alias

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
