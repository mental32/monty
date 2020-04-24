from abc import ABC, abstractmethod
from contextlib import contextmanager
from dataclasses import dataclass, field
from enum import IntEnum, auto
from typing import Tuple, Optional, List, Dict, Set

from ..language import Function


SSAValue = int
BlockId = int


class InstrOp(IntEnum):
    IntConst = auto()
    StrConst = auto()
    Assign = auto()


@dataclass
class BlockInstr:
    op: InstrOp
    args: List[SSAValue]
    ret: Optional[SSAValue] = None


@dataclass
class BasicBlock:
    instructions: List[BlockInstr] = field(default_factory=list)


@dataclass
class Ebb:
    """A collection of basic blocks used to form a routine."""

    blocks: Dict[BlockId, BasicBlock] = field(default_factory=dict)
    namespace: Set[str] = field(default_factory=set)

    last_ssa: SSAValue = field(init=False, default=0)

    _cursor: Optional[BlockId] = field(init=False, default=None)
    _cursor_pinned: bool = field(init=False, default=False)

    def _next_ssa_value(self) -> SSAValue:
        self.last_ssa += 1
        return self.last_ssa

    @property
    def cursor(self):
        return self.blocks[self._cursor]

    def using_clean_block(self) -> BlockId:
        if not self._cursor_pinned and self._cursor is None or self.cursor.instructions:
            ident, _, = self.create_block()
            self.switch_to_block(ident)
        else:
            ident = self._cursor

        return ident

    @contextmanager
    def pin_head(self):
        self._cursor_pinned = True
        yield
        self._cursor_pinned = False

    def switch_to_block(self, ident: BlockId):
        if self._cursor_pinned:
            raise Exception("cursor is pinned!")

        self._cursor = ident

    def create_block(self) -> Tuple[BlockId, BasicBlock]:
        ident = (self.blocks and (max(self.blocks.keys()) + 1)) or 0
        block = BasicBlock()

        self.blocks[ident] = block

        return (ident, block)

    def assign(self, name: str, value: SSAValue):
        self.namespace.add(name)
        self.cursor.instructions.append(
            BlockInstr(op=InstrOp.Assign, args=[value], ret=name)
        )

    def str_const(self, ref: int) -> SSAValue:
        slot = self._next_ssa_value()
        self.cursor.instructions.append(
            BlockInstr(op=InstrOp.StrConst, args=[ref], ret=slot)
        )
        return slot

    def int_const(self, value: int, bits: int = 64, signed: bool = True) -> SSAValue:
        slot = self._next_ssa_value()
        self.cursor.instructions.append(
            BlockInstr(op=InstrOp.IntConst, args=[value, bits, signed], ret=slot)
        )
        return slot
