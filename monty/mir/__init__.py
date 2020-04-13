from typing import NewType, TypeVar, Dict

SSAValue = NewType("SSAValue", lambda block, discrim: (block, discrim))

__all__ = ("SSAValue",)

from .raw_type import *
from .block import *
from .scope import *
from .function import *
from .module import *
