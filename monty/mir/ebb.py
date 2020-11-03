import ast
import sys
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional, Tuple, Union

from ..ty import TypeInfo

# (op, in(args), out(ret))
Instr = Tuple[str, List[Any], int]

SSAValue = int
BlockId = int


@dataclass
class Ebb:
    """(E)xtended (B)asic (B)lock builder used to compose MIR."""

    blocks: List[List[Instr]] = field(default_factory=list)
    stack_slots: Dict[SSAValue, Tuple[int, TypeInfo]] = field(default_factory=dict)

    ast_node_to_ssa: Dict[ast.AST, SSAValue] = field(default_factory=dict)
    name_to_stack_slot: Dict[str, SSAValue] = field(default_factory=dict)
    refs: Dict[SSAValue, Any] = field(default_factory=dict)
    vars: Dict[Any, TypeInfo] = field(default_factory=dict)

    __last_ssa_value: SSAValue = field(default=-1)
    __cursor: int = -1

    @property
    def is_empty(self) -> bool:
        return not bool(self.blocks)

    @property
    def cursor(self) -> int:
        return self.__cursor

    @property
    def head(self) -> List[Instr]:
        assert not self.is_empty, self
        return self.blocks[self.__cursor]

    # Helpers

    def __emit(self, *, opstr: str, args: List[Any], ret: SSAValue = -1) -> SSAValue:
        if ret == -1:
            self.__last_ssa_value += 1
            slot = self.__last_ssa_value
        else:
            slot = ret

        instr = (opstr, args, slot)
        self.head.append(instr)
        return slot

    def reference(self, obj: Any) -> SSAValue:
        if self.refs:
            ref = max(self.refs) + 1
        else:
            ref = 0

        self.refs[ref] = obj
        return ref

    def switch_to_block(self, block_id: BlockId) -> BlockId:
        """Switch the cursor to a block."""
        _, self.__cursor = self.__cursor, block_id
        return _

    def create_block(self) -> BlockId:
        """Create a new block."""
        self.blocks.append([])
        return len(self.blocks) - 1

    # Constants

    def int_const(self, value: int, bits: int = 64, signed: bool = True) -> SSAValue:
        """Produce an integer constant."""
        return self.__emit(opstr="int.const", args=[value, bits, signed])

    def bool_const(
        self, value: Union[bool, int], *, is_ssa_value: bool = False
    ) -> SSAValue:
        """Produce a boolean constant."""
        return self.__emit(opstr="bool.const", args=[is_ssa_value, value])

    def str_const(self, const_idx: int) -> SSAValue:
        assert isinstance(const_idx, int)
        return self.__emit(opstr="str.const", args=[const_idx])

    # Data-casting

    def cast_bool_to_int(self, ty: TypeInfo, value: SSAValue) -> SSAValue:
        """cast a boolean value to an integer one of some type."""
        return self.__emit(opstr="bint", args=[ty, value])

    # Arithmetic operations

    def int_add(self, left: SSAValue, right: SSAValue) -> SSAValue:
        """Add two integer values."""
        return self.__emit(opstr="iadd", args=[left, right])

    def int_sub(self, left: SSAValue, right: SSAValue) -> SSAValue:
        """Add two integer values."""
        return self.__emit(opstr="isub", args=[left, right])

    def icmp(self, op: str, lhs: SSAValue, rhs: SSAValue) -> SSAValue:
        """Perform an integer-based comparison."""
        return self.__emit(opstr="icmp", args=[op, lhs, rhs])

    # Variable operations

    def use(self, ident: Any) -> SSAValue:
        """use a variable as an ssa value."""
        return self.__emit(opstr="usevar", args=[ident])

    def assign(self, ident: Any, value: SSAValue, ty: TypeInfo):
        """Assign a value to a variable."""
        # self._typecheck(value, expected=ty)
        self.vars[ident] = ty
        assert isinstance(value, SSAValue)
        return self.__emit(opstr="assign", args=[value], ret=ident)

    # Stack operations

    def create_stack_slot(self, size: int, ty: TypeInfo) -> SSAValue:
        """Creates a new stack slot of some `size` and returns its ssavalue."""
        self.__last_ssa_value += 1
        slot = self.__last_ssa_value
        self.stack_slots[slot] = (size, ty)
        return slot

    def stack_load(self, slot: SSAValue) -> SSAValue:
        (size, slot_memory_type) = self.stack_slots[slot]
        return self.__emit(opstr="stackload", args=[slot, size, slot_memory_type])

    def stack_store(self, slot: SSAValue, value: SSAValue) -> SSAValue:
        return self.__emit(opstr="stackstore", args=[slot, value], ret=sys.maxsize)

    # Data load/store

    def load_data(self, data_ref: SSAValue) -> SSAValue:
        return self.__emit(opstr="dataload", args=[data_ref])

    # Flow-control

    def nop(self):
        """Emit a no-op."""
        self.__emit(opstr="nop", args=[], ret=sys.maxsize)

    def jump(self, target: BlockId):
        """Jump to a target block."""
        self.__emit(opstr="jump", args=[target], ret=sys.maxsize)

    def branch_icmp(
        self,
        mode: str,
        left: SSAValue,
        right: SSAValue,
        target: BlockId,
    ) -> SSAValue:
        return self.__emit(opstr="bicmp", args=[mode, left, right, target])

    def return_(self, value: Optional[SSAValue]):
        """Return from the function with a value."""
        # self._typecheck(value, expected=self.returns)
        args = [value] if value is not None else []
        return self.__emit(opstr="return", args=args, ret=-1)

    def call(self, function: SSAValue, args: List[SSAValue]) -> SSAValue:
        return self.__emit(opstr="call", args=[function, args])
