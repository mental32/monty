from dataclasses import field
from functools import partial

_silent_field = partial(field, repr=False, init=False)

__all__ = ("TypedAST", "BaseVisitor")

from .typed import *
from .native import *
