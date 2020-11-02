import ast
from abc import ABC, abstractmethod
from pathlib import Path
from sys import modules, path
from typing import Union

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


class TypeInfo(ABC):
    """Base class for all types."""

    @abstractmethod
    def size(self) -> int:
        """Get the size of this type."""


class AbstractEbb:
    pass


def collapse_attribute(n: Union[ast.Attribute, ast.Name]) -> str:
    """Recursively fold an attribute or name node into a dotted name string."""
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
