from dataclasses import dataclass, field
from typing import Dict, List, Any, TypeVar

from . import RawType, SSAValue, _next_ssa_value

__all__ = ("AbstractBlock",)


@dataclass
class AbstractBlock:
    ssa_map: Dict[SSAValue, RawType] = field(default_factory=dict)
    params: Dict[SSAValue, RawType] = field(default_factory=dict)
    kwargs: Dict[str, SSAValue] = field(default_factory=dict)
    instructions: List[dict] = field(default_factory=list)

    # Public API

    def add_kwarg(self, *, name: str, kind: RawType) -> SSAValue:
        ident = _next_ssa_value(self.ssa_map)
        self.params[ident] = kind
        return ident

    def into_raw(self, refs):
        K = TypeVar("K")
        V = TypeVar("V")

        def sanitize(mapping: Dict[K, V]) -> Dict[K, V]:
            return {
                key: refs.get(ident := value.name, ident)
                for key, value in mapping.items()
            }

        T = TypeVar("T")

        def sanitize_instruction(node: T) -> T:
            assert "instr" in node, repr(node)

            instr = node["instr"]

            if "kind" in node:
                kind_ref = node["kind"]
                kind = str(kind_ref.name if isinstance(kind_ref, RawType) else kind_ref)

                return {**node, "kind": refs.get(kind, kind)}

            return node

        return {
            "ssa_map": sanitize(self.ssa_map),
            "params": sanitize(self.params),
            "code": [sanitize_instruction(instr) for instr in self.instructions],
        }

    # Instructions

    def __binop(self, instr: str, kind:  RawType, x: SSAValue, y: SSAValue) -> SSAValue:
        ident = _next_ssa_value(self.ssa_map)

        self.instructions.append({"instr": instr, "x": x, "y": y, "kind": kind})

        self.ssa_map[ident] = kind

        return ident

    def iconst(self, kind: RawType, imm: int) -> SSAValue:
        ident = _next_ssa_value(self.ssa_map)

        self.instructions.append({"instr": "IConst", "kind": kind, "imm": imm})

        self.ssa_map[ident] = kind

        return SSAValue(ident)

    def sub(self, kind: RawType, x: SSAValue, y: SSAValue) -> SSAValue:
        self.__binop("Sub", kind, x, y)

    def add(self, kind: RawType, x: SSAValue, y: SSAValue) -> SSAValue:
        self.__binop("Add", kind, x, y)

    def mul(self, kind: RawType, x: SSAValue, y: SSAValue) -> SSAValue:
        self.__binop("Mul", kind, x, y)

    def return_(self, *rvals: SSAValue):
        self.instructions.append({"instr": "Return", "rvals": rvals[:]})
