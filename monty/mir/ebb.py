from contextlib import contextmanager
from dataclasses import dataclass, field
from itertools import count
from typing import NamedTuple, Tuple, Dict, final, List, Optional, Any, Set

from ..typing import TypeId
from . import VariableId, BlockId, BlockInstr, SSAValue, InstrOp

__all__ = ("Ebb", "BasicBlock", "FluidBlock")

_NULL = object()


@final
class BasicBlock(NamedTuple):
    """A single basic block."""

    body: Tuple[BlockInstr]
    parameters: Dict[SSAValue, TypeId]

    def get_all_referenced_values(self) -> Set[SSAValue]:
        return {value for instr in self.body for value in instr.ssa_values}

    def get_all_produced_values(self) -> Set[SSAValue]:
        return {instr.product for instr in self.body if instr.product is not None}


@final
class Ebb(NamedTuple):
    """A collection of basic blocks."""

    # Ebb(i64, i64) -> i64
    parameters: Tuple[TypeId]
    return_value: TypeId

    # Ebb.variables.{variable_id} -> i64
    variables: Dict[VariableId, TypeId]

    # Ebb.stack_slots.{slot_id} -> StackSlot(size: u32, ty: TypeId)
    stack_slots: Dict[SSAValue, Tuple[int, TypeId]]

    # Ebb.refs.{ref_id} -> Function
    refs: Dict[SSAValue, Any]

    # Ebb.block_id.{block_id} = {b0: iconst.i64(1), b1: iconst.i64(1), b2: iadd(v1, v2)}
    blocks: Dict[BlockId, BasicBlock]

    def ssa_body_mapping(self) -> Dict[SSAValue, BlockInstr]:
        return {
            instr.ret: instr
            for block in self.blocks.values()
            for instr in block.body
            if isinstance(instr.ret, SSAValue)
        }

    def sorted_blocks(self) -> List[Tuple[BlockId, BasicBlock]]:
        return sorted(self.blocks.items())


@dataclass
class FluidBlock:
    """An ebb that is being formed."""

    parameters: List[TypeId] = field(default_factory=list)
    returns: Optional[TypeId] = None

    variables: Dict[VariableId, TypeId] = field(default_factory=dict)
    stack_slots: Dict[SSAValue, Tuple[int, TypeId]] = field(default_factory=dict)
    blocks: Dict[BlockId, BasicBlock] = field(default_factory=dict)
    ssa_value_types: Dict[SSAValue, TypeId] = field(default_factory=dict)
    refs: Dict[SSAValue, Any] = field(default_factory=dict)

    __cursor: Optional[BlockId] = field(init=False, default=None)
    __last_ssa_value: int = -1

    @property
    def _cursor(self) -> BasicBlock:
        return self.blocks[self.__cursor]

    @property
    def current_block_id(self) -> BlockId:
        return self.__cursor

    def _emit(self, *, op, args, ret: Any = _NULL) -> SSAValue:
        if ret is _NULL:
            self.__last_ssa_value += 1
            slot = self.__last_ssa_value
        else:
            slot = ret

        instr = BlockInstr(op=op, args=args, ret=slot)
        self._cursor.body.append(instr)
        return slot

    def _typecheck(self, value: SSAValue, expected: TypeId):
        if (
            actual := self.ssa_value_types.get(value)
        ) is not None and actual != expected:  # pylint: disable=used-before-assignment
            raise TypeError(f"{value=!r} had type {actual=!r} but {expected=!r}")

    def reference(self, obj: Any) -> SSAValue:
        if self.refs:
            ref = max(self.refs) + 1
        else:
            ref = 0

        self.refs[ref] = obj
        return ref

    def finalize(self) -> Ebb:
        """Produce a finalized EBB."""
        parameters = tuple(self.parameters[:])
        return_value = self.returns
        variables = {**self.variables}
        stack_slots = {**self.stack_slots}

        blocks = {}
        for block_id, block in self.blocks.items():
            if False and not block.parameters:
                parameters = {}

                referenced = block.get_all_referenced_values()
                produced = block.get_all_produced_values()

                print(
                    f"{block_id=!r} {referenced=!r} {produced=!r}, {referenced.difference(produced)=!r}"
                )

                for foreign in referenced.difference(produced):
                    parameters[foreign] = self.ssa_value_types[foreign]
            else:
                parameters = {**block.parameters}

            body = tuple(block.body)

            copy = BasicBlock(body=body, parameters=parameters)
            blocks[block_id] = copy

        refs = {**self.refs}

        assert isinstance(return_value, int), f"{return_value=!r}"

        return Ebb(
            parameters=parameters,
            return_value=return_value,
            variables=variables,
            stack_slots=stack_slots,
            blocks=blocks,
            refs=refs,
        )

    # BasicBlock methods

    def switch_to_block(self, block_id: BlockId):
        """Switch the cursor to a block."""
        self.__cursor = block_id

    def create_block(self, *, parameters: Dict[SSAValue, TypeId] = None) -> BlockId:
        """Create a new block."""
        block_id = max(self.blocks) + 1 if self.blocks else 0
        parameters = parameters or {}

        block = BasicBlock(body=[], parameters=parameters)
        self.blocks[block_id] = block

        return block_id

    @contextmanager
    def with_block(self, target: Optional[BlockId] = None):
        """Create a new block, switch to it and then switch back."""
        previous_block_id = self.__cursor

        if target is None:
            new_block_id = self.create_block()
        else:
            new_block_id = target

        self.switch_to_block(new_block_id)
        yield new_block_id
        self.switch_to_block(previous_block_id)

    # Instruction emitting methods

    def int_const(self, value: int, bits: int = 64, signed: bool = True) -> SSAValue:
        """Produce an integer constant."""
        return self._emit(op=InstrOp.IntConst, args=[value, bits, signed])

    def bool_const(self, value: bool, *, is_ssa_value: bool = False) -> SSAValue:
        """Produce a boolean constant."""
        return self._emit(op=InstrOp.BoolConst, args=[is_ssa_value, value])

    # Data-casting

    def cast_bool_to_int(self, ty: TypeId, value: SSAValue) -> SSAValue:
        """cast a boolean value to an integer one of some type."""
        return self._emit(op=InstrOp.BInt, args=[ty, value])

    # Arithmetic operations

    def int_add(self, left: SSAValue, right: SSAValue) -> SSAValue:
        """Add two integer values."""
        return self._emit(op=InstrOp.IAdd, args=[left, right])

    def int_sub(self, left: SSAValue, right: SSAValue) -> SSAValue:
        """Add two integer values."""
        return self._emit(op=InstrOp.ISub, args=[left, right])

    def icmp(self, op: str, lhs: SSAValue, rhs: SSAValue) -> SSAValue:
        """Perform an integer-based comparison."""
        return self._emit(op=InstrOp.IntCmp, args=[op, lhs, rhs])

    # Variable operations

    def use(self, ident: "T") -> SSAValue:
        """use a variable as an ssa value."""
        return self._emit(op=InstrOp.UseVar, args=[ident])

    def assign(self, ident: "T", value: SSAValue, ty: TypeId):
        """Assign a value to a variable."""
        self._typecheck(value, expected=ty)
        self.variables[ident] = ty
        assert isinstance(value, SSAValue)
        return self._emit(op=InstrOp.Assign, args=[value], ret=ident)

    # Stack operations

    def create_stack_slot(self, size: int, ty: TypeId) -> SSAValue:
        """Creates a new stack slot of some `size` and returns its ssavalue."""
        self.__last_ssa_value += 1
        slot = self.__last_ssa_value
        self.stack_slots[slot] = (size, ty)
        return slot

    def stack_load(self, slot: SSAValue) -> SSAValue:
        (size, slot_memory_type,) = self.stack_slots[slot]
        return self._emit(op=InstrOp.StackLoad, args=[slot, size, slot_memory_type])

    def stack_store(self, slot: SSAValue, value: SSAValue) -> SSAValue:
        return self._emit(op=InstrOp.StackStore, args=[slot, value], ret=None)

    # Data load/store

    def load_data(self, data_ref: SSAValue) -> SSAValue:
        return self._emit(op=InstrOp.DataLoad, args=[data_ref])

    # Flow-control

    def nop(self):
        """Emit a no-op."""
        self._emit(op=InstrOp.NoOp, args=[], ret=None)

    def jump(self, target: BlockId):
        """Jump to a target block."""
        self._emit(op=InstrOp.Jump, args=[target], ret=None)

    def branch_icmp(
        self, mode: str, left: SSAValue, right: SSAValue, target: BlockId
    ) -> SSAValue:
        return self._emit(op=InstrOp.BranchIntCmp, args=[mode, left, right, target])

    def return_(self, value: Optional[SSAValue]):
        """Return from the function with a value."""
        self._typecheck(value, expected=self.returns)
        args = [value] if value is not None else []
        return self._emit(op=InstrOp.Return, args=args, ret=None)

    def call(self, function: SSAValue, *args) -> SSAValue:
        return self._emit(op=InstrOp.Call, args=[function, args])
