from typing import Dict

from monty.utils import SSAMap

from . import TypeInfo, TypeId

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

    def get_id_or_insert(self, type_info: TypeInfo) -> TypeId:
        inverse = {value: type_id for value, type_id, in self.mapping.items()}

        try:
            return inverse[type_info]
        except KeyError:
            return self.insert(type_info)
