from dataclasses import dataclass, field
from typing import Dict, List, Any, TypeVar, Tuple, Optional

from . import RawType, SSAValue

__all__ = ("AbstractBlock",)


@dataclass
class AbstractBlock:
    block_ident: int
    ssa_map: Dict[SSAValue, RawType] = field(default_factory=dict)
    params: Dict[SSAValue, RawType] = field(default_factory=dict)
    kwargs: Dict[str, SSAValue] = field(default_factory=dict)
    instructions: List[dict] = field(default_factory=list)

    def _next_ssa_value(self) -> SSAValue:
        return SSAValue(
            max(map((lambda v: v[-1]), self.ssa_map)) + 1 if self.ssa_map else 0
        )

    def _insert_new_kind(self, kind: RawType, block: Optional[int] = None) -> SSAValue:
        value = self._next_ssa_value()

        if block is None:
            block = self.block_ident

        self.ssa_map[(block, value)] = kind

        return (block, value)

    # Public API

    def is_pristine(self) -> bool:
        return all(
            not value
            for value in [self.ssa_map, self.params, self.kwargs, self.instructions]
        )

    def add_kwarg(self, *, name: str, kind: RawType) -> SSAValue:
        ident = self._insert_new_kind(kind)
        self.params[ident] = kind
        self.kwargs[name] = ident
        return ident

    def into_raw(self, refs):
        def sanitize(mapping: Dict[Tuple[int, int], str]) -> Dict[int, str]:
            return {
                (block << 32 | discrim): refs.get(ident := value.name, ident)
                for (block, discrim), value in mapping.items()
            }

        T = TypeVar("T")

        def sanitize_instruction(node: T) -> T:
            assert "instr" in node, repr(node)

            instr = node["instr"]

            if "kind" in node:
                kind_ref = node["kind"]
                kind = str(kind_ref.name if isinstance(kind_ref, RawType) else kind_ref)

                node = {**node, "kind": refs.get(kind, kind)}

            if "rvals" in node:
                node = {
                    **node,
                    "rvals": [
                        (block << 32 | discrim) for block, discrim in node["rvals"]
                    ],
                }

            if "block" in node:
                node = {
                    **node,
                    "block": node["block"].block_ident,
                    "args": [
                        (block << 32 | discrim) for block, discrim in node["args"]
                    ],
                }

            for key in ("ssa_value", "x", "y"):
                try:
                    block, discrim, = node[key]
                except KeyError:
                    continue
                else:
                    node[key] = block << 32 | discrim

            return node

        return {
            "ssa_map": sanitize(self.ssa_map),
            "params": sanitize(self.params),
            "kwargs": {
                key: (block << 32 | discrim)
                for key, (block, discrim) in self.kwargs.items()
            },
            "code": [sanitize_instruction(instr) for instr in self.instructions],
        }

    # Instructions

    def __binop(self, instr: str, kind: RawType, x: SSAValue, y: SSAValue) -> SSAValue:
        block, discrim, = self._insert_new_kind(kind)
        self.instructions.append(
            {"instr": instr, "x": x, "y": y, "kind": kind, "ssa_discrim": discrim}
        )
        return (block, discrim)

    def def_var(self, ident: str, value: SSAValue):
        self.instructions.append(
            {"instr": "DefVar", "ident": ident, "ssa_value": value}
        )

    def use_var(self, ident: str, kind: RawType) -> SSAValue:
        ssa_value = self._insert_new_kind(kind)
        self.instructions.append(
            {"instr": "UseVar", "ident": ident, "kind": kind, "ssa_value": ssa_value}
        )
        return ssa_value

    def iconst(self, kind: RawType, imm: int) -> SSAValue:
        block, discrim, = self._insert_new_kind(kind)
        self.instructions.append(
            {"instr": "Const", "kind": kind, "imm": imm, "ssa_discrim": discrim}
        )
        return (block, discrim)

    def sub(self, kind: RawType, x: SSAValue, y: SSAValue) -> SSAValue:
        return self.__binop("Sub", kind, x, y)

    def add(self, kind: RawType, x: SSAValue, y: SSAValue) -> SSAValue:
        return self.__binop("Add", kind, x, y)

    def mul(self, kind: RawType, x: SSAValue, y: SSAValue) -> SSAValue:
        return self.__binop("Mul", kind, x, y)

    def jump(self, block: "AbstractBlock", *args: SSAValue):
        self.instructions.append({"instr": "Jump", "block": block, "args": args})

    def return_(self, *rvals: SSAValue):
        self.instructions.append({"instr": "Return", "rvals": rvals[:]})
