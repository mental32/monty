from typing import Dict

from monty.utils import SSAMap

from . import TypeInfo, TypeId, Primitive

__all__ = ("TypeContext",)


class TypeContext(SSAMap[TypeInfo]):
    def __init__(self):
        super().__init__()

        assert not self.mapping

        unknown = self.insert(Primitive.Unknown)
        assert unknown == 0, f"Failed to slot Primitive.Unknown at type_id 0!"

        self.insert(Primitive.I64)
        self.insert(Primitive.I32)
        self.insert(Primitive.Bool)
        self.insert(Primitive.None_)

    def __repr__(self) -> str:
        return f"<TypeContext: {self.mapping=!r}>"

    def size_of(self, type_id: TypeId) -> int:
        """Get the size of a type in bytes."""
        return self.mapping[type_id].size()

    def primitives(self) -> Dict[str, TypeId]:
        return {
            "int": self.mapping[Primitive.I64],
            "i64": self.mapping[Primitive.I64],
            "i32": self.mapping[Primitive.I32],
            "none": self.mapping[Primitive.None_],
            "bool": self.mapping[Primitive.Bool],
        }

    def reconstruct(self, kind: TypeId) -> str:
        return self[kind].as_str(self)
