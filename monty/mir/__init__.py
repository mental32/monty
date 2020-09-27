SSAValue = int
BlockId = int
VariableId = int

from .libc import *
from .instr import *
from .ebb import Ebb, FluidBlock
from .builder import *
