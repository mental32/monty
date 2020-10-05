import ast
from typing import Dict, Optional, List as _List, Optional

import monty
from monty.diagnostic import Error
from monty.errors import TypeCheckError
from monty.language import ImportDecl, Item, Scope, Function
from monty.utils import SSAMap

from . import TypeInfo, TypeId, primitives
from . import compound

__all__ = ("TypeContext",)


class TypeContext(SSAMap[TypeInfo]):
    def __init__(self):
        super().__init__()

        assert not self.mapping

        unknown = self.insert(primitives.Unknown())
        assert unknown == 0, f"Failed to slot Primitive.Unknown at type_id 0!"

        self.insert(primitives.Int64())
        self.insert(primitives.Int32())
        self.insert(primitives.Boolean())
        self.insert(primitives.NoneType())

    def __repr__(self) -> str:
        return f"<TypeContext: {self.mapping=!r}>"

    # Helpers

    def primitives(self) -> Dict[str, TypeId]:
        return {
            "int": self[primitives.Integer()],
            "i64": self[primitives.Int64()],
            "i32": self[primitives.Int32()],
            "none": self[primitives.NoneType()],
            "bool": self[primitives.Boolean()],
        }

    def reconstruct(self, kind: TypeId) -> str:
        return self[kind].as_str(self)
