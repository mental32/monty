from typing import NewType, TypeVar, Dict

SSAValue = NewType("SSAValue", int)

V = TypeVar("V")
def _next_ssa_value(mapping: Dict[SSAValue, V]) -> SSAValue:
    return SSAValue(max(mapping) + 1 if mapping else 0)

from .raw_type import *
from .block import *
from .scope import *
from .function import *
from .module import *
