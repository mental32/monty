import ast

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
        return self.node.alias or self.node.name
