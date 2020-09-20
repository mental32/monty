import json
from abc import ABC, abstractmethod
from contextlib import contextmanager
from collections import namedtuple
from dataclasses import dataclass, field
from enum import IntEnum, auto
from typing import Tuple, Optional, List, Dict, Set, NamedTuple

from . import SSAValue


__all__ = ("InstrOp", "BlockInstr")


class InstrOp(IntEnum):
    IntConst = auto()
    StrConst = auto()

    Return = auto()
    NoOp = auto()

    IAdd = auto()
    ISub = auto()

    UseVar = auto()
    Assign = auto()

    StackStore = auto()
    StackLoad = auto()
    StackAddr = auto()

    DataLoad = auto()
    DataStore = auto()

    Jump = auto()
    IntCmp = auto()

    BInt = auto()
    BoolConst = auto()
    BranchIntCmp = auto()
    Call = auto()

    def __str__(self) -> str:
        return self.name.lower()  # pylint: disable=no-member


class BlockInstr(NamedTuple):
    """An instruction in a block."""

    op: InstrOp
    args: Optional[List[SSAValue]] = None
    ret: Optional[SSAValue] = None

    def to_json(self, *, json_dumps=json.dumps) -> str:
        return json_dumps({"op": self.op, "args": self.args, "ret": self.ret})

    @property
    def product(self):
        # pylint: disable=unpacking-non-sequence,not-an-iterable,unsubscriptable-object
        if (op := self.op) in (
            InstrOp.IntConst,
            InstrOp.IntCmp,
            InstrOp.BInt,
            InstrOp.BoolConst,
            InstrOp.ISub,
        ):
            return self.ret

        elif op in (
            InstrOp.Assign,
            InstrOp.Jump,
            InstrOp.UseVar,
            InstrOp.BranchIntCmp,
            InstrOp.Return,
        ):
            return None

        else:
            raise Exception(f"Unhandled case! {op=!r}")

    @property
    def ssa_values(self):
        # pylint: disable=unpacking-non-sequence,not-an-iterable,unsubscriptable-object
        if (op := self.op) in (InstrOp.UseVar, InstrOp.IntConst):
            return [self.ret]

        elif op in (InstrOp.StrConst, InstrOp.Assign):
            print("<<", repr(self), self.args)
            return self.args[:1]

        elif op in (InstrOp.BInt, InstrOp.BoolConst):
            return self.args[1:]

        elif (
            op in (InstrOp.NoOp, InstrOp.Jump) or not self.args and op is InstrOp.Return
        ):
            return ()

        elif op is InstrOp.Return:
            assert self.args
            return self.args

        elif op in (InstrOp.IAdd, InstrOp.ISub):
            return self.args[0:2] + [self.ret]

        elif op in (InstrOp.BranchIntCmp, InstrOp.IntCmp):
            return self.args[1:3]  # ["eq", v1, v2, b3]

        else:
            raise Exception(f"Unhandled case! {op=!r}")

    def __str__(self) -> str:
        # pylint: disable=unpacking-non-sequence,not-an-iterable
        if self.ret is not None:
            ret = f"v{self.ret!r} = "
        else:
            ret = ""

        if (op := self.op) is InstrOp.IntConst:
            assert self.args is not None
            const, bits, *_ = self.args
            rest = f"iconst.{bits} {const}"

        elif op is InstrOp.Call:
            assert self.args is not None
            n, args, = self.args
            args = ", ".join(map(str, args))
            rest = f"call r{n}({args})"

        elif op is InstrOp.StrConst:
            assert self.args is not None
            value, *_ = self.args
            rest = f"str.const({value=!r})"

        elif op is InstrOp.Return:
            args = self.args
            if args is None or not args:
                rest = "return"
            else:
                args = ", ".join(f"v{n}" for n in self.args)
                rest = f"return {args}"

        elif op is InstrOp.NoOp:
            rest = "nop"

        elif op is InstrOp.IAdd:
            assert self.args is not None
            lhs, rhs, *_ = self.args
            rest = f"iadd v{lhs!r} v{rhs!r}"

        elif op is InstrOp.ISub:
            assert self.args is not None
            lhs, rhs, *_ = self.args
            rest = f"isub v{lhs!r} v{rhs!r}"

        elif op is InstrOp.UseVar:
            assert self.args is not None
            var_id, *_ = self.args
            value = self.ret
            rest = f"{var_id} -> v{value}"
            ret = ""

        elif op is InstrOp.Assign:
            assert self.args is not None
            n, *_ = self.args
            rest = f"v{n}"
            ret = f"{self.ret} = "

        elif op is InstrOp.Jump:
            assert self.args is not None
            n, *_ = self.args
            rest = f"jump -> b{n}"
            ret = ""

        elif op is InstrOp.BranchIntCmp:
            assert self.args is not None
            mode, left, right, target, *_ = self.args
            ret = ""
            rest = f"br_icmp (v{left!r} {mode!r} v{right!r}) -> b{target}"

        else:
            rest = f"{self!r}"
            ret = ""

        return f"{ret}{rest}"
