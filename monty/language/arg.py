import ast
from enum import IntEnum, auto
from dataclasses import dataclass, field
from typing import Optional


class ArgumentKind(IntEnum):
    PosOnly = auto()
    KwOnly = auto()
    Regular = auto()
    Vararg = auto()
    Kwargs = auto()


@dataclass
class Argument:
    node: ast.arg
    kind: ArgumentKind

    def __str__(self) -> str:
        name = self.name
        annotation = self.annotation
        return f"{name}" if annotation is None else f"{name}: {annotation}"

    @property
    def name(self) -> str:
        return self.node.arg

    @property
    def annotation(self) -> Optional[str]:
        annotation = None

        if (ann := self.node.annotation) is not None:
            if isinstance(ann, ast.Name):
                annotation = f"{ann.id}"
            else:
                annotation = ast.dump(ann)

        elif comment := self.node.type_comment:
            annotation = f"{comment}"

        return annotation
