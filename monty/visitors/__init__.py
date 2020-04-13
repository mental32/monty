import ast
from typing import TypeVar, Set, Any, Dict, Optional, Type
from types import MappingProxyType
from pathlib import Path
from contextlib import suppress
from dataclasses import dataclass, field

from ..mir import Module, Scope, Function, Argument, ArgKind, RawType
from ..errors import CompilationError, MissingTypeAnnotation, BadReturnType

class UnsupportedNode(CompilationError):
    """Raised when a node does not have a visitor."""


__all__ = (
    "BaseVisitor",
    "UnsupportedNode",
    "FunctionVisitor",
    "ReturnVisitor",
    "AssignVisitor",
    "NameVisitor",
    "ConstantVisitor",
    "BinOpVisitor",
)

from .base import BaseVisitor
from .func import FunctionVisitor
from .name import NameVisitor
from .assign import AssignVisitor
from .const import ConstantVisitor
from .return_ import ReturnVisitor
from .binop import BinOpVisitor

T = TypeVar("T")
VISITORS: Dict[Type[T], Type[BaseVisitor]] = {
    ast.Return: ReturnVisitor,
    ast.Constant: ConstantVisitor,
    ast.BinOp: BinOpVisitor,
    ast.Assign: AssignVisitor,
    ast.Name: NameVisitor,
}
