import ast
import sys
import typing
from abc import ABC, abstractmethod
from collections import deque
from contextlib import contextmanager
from dataclasses import InitVar, dataclass, field
from functools import (
    cached_property,
    lru_cache,
    partial,
    singledispatch,
    singledispatchmethod,
)
from pathlib import Path
from sys import modules, path
from typing import (
    Any,
    Callable,
    Dict,
    Iterator,
    List,
    NamedTuple,
    Optional,
    Sequence,
    Set,
    Tuple,
    Type,
    TypeVar,
    Union,
)

NULL_SCOPE = object()
ASTNode = ast.AST
ASTInfix = Union[
    ast.Eq,
    ast.NotEq,
    ast.Lt,
    ast.LtE,
    ast.Add,
    ast.Sub,
    ast.Mult,
    ast.Div,
    ast.Gt,
    ast.GtE,
]
STDLIB_PATH = Path(__file__).parent.parent.joinpath("std").absolute()
CAN_HAVE_SCOPE = (ast.Module, ast.ClassDef, ast.FunctionDef, ast.AsyncFunctionDef)
LEGAL_NODE_TYPES = {
    ast.ClassDef,
    ast.Module,
    ast.FunctionDef,
    ast.AnnAssign,
    ast.Assign,
    ast.Name,
    ast.Load,
    ast.Store,
    ast.Constant,
    ast.arguments,
    ast.Return,
    ast.arg,
    ast.Call,
    ast.Compare,
    ast.Add,
    ast.Mult,
    ast.Sub,
    ast.AugAssign,
    ast.While,
    ast.BinOp,
    ast.If,
    ast.Expr,
    ast.Import,
    ast.alias,
    ast.ImportFrom,
    ast.Attribute,
    ast.Pass,
    ast.LtE,
}


def panic(msg: str = "..."):
    raise AssertionError(msg)


class Builtin:
    """Base class used as a market type for all compiler built-in stuff."""

class TypeInfo(ABC):
    """Base class for all types."""

    @abstractmethod
    def size(self) -> int:
        """Get the size of this type."""

class AbstractEbb:
    pass


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

    @cached_property
    def qualname(self) -> List[str]:
        return self.node.name.split(".")

    @cached_property
    def realname(self) -> str:
        return self.node.asname or self.node.name

    def __hash__(self) -> int:
        return hash(self.node)

    def __str__(self) -> str:
        return self.realname


def collapse_attribute(n: Union[ast.Attribute, ast.Name]) -> str:
    if isinstance(n, ast.Attribute):
        assert isinstance(n.value, (ast.Attribute, ast.Name))
        return f"{collapse_attribute(n.value)!s}.{n.attr}"

    if isinstance(n, ast.Name):
        return n.id

    assert False


def is_visible(n: ASTNode) -> bool:
    return not isinstance(
        n, (ast.Load, ast.Store, ast.Add, ast.Sub, ast.Module, ast.arguments)
    )
