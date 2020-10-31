import ast
import sys
import inspect
import types
from typing import Union, Callable, Any
from pathlib import Path

from .utils import *
from .ty import *
from .mirgen import *
from .item import *
from .context import *
from .driver import MontyDriver


_SourceObjectTypes = (types.ModuleType, types.MethodType, types.FunctionType, types.CodeType)
_SourceObjectType = Union[types.ModuleType, types.MethodType, types.FunctionType, types.CodeType]

Compileable = Union[str, Path, ASTNode, _SourceObjectType, Callable[..., Any]]

def compile(source: Compileable, **kwargs) -> MontyDriver:
    """Parse and typecheck the provided source."""
    if isinstance(source, Path):
        with open(source) as inf:
            return compile(source=inf.read(), **kwargs)

    elif isinstance(source, _SourceObjectTypes) or callable(source):
        assert type(source) is type((lambda: None))
        return compile(source=inspect.getsource(source), **kwargs)

    elif isinstance(source, ASTNode):
        assert sys.version_info >= (3, 9)
        assert hasattr(ast, "unparse")
        return compile(source=ast.unparse(source), **kwargs)  # type: ignore

    else:
        assert isinstance(source, str), type(source)

    driver = MontyDriver()
    module = driver.compile(source, **kwargs)
    assert module.name in driver.ctx.modules
    return driver
