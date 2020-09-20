from typing import Dict

from monty.utils import SSAMap

from . import TypeInfo, TypeId, Primitive

__all__ = ("TypeContext",)


class TypeContext(SSAMap[TypeInfo]):
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

    # fmt: off
    def get_id_or_insert(self, type_info: TypeInfo) -> TypeId:
        for type_id, value, in self.mapping.items():
            if type_info == value:
                return type_id
        else:
            return self.insert(type_info)
    # fmt: on
