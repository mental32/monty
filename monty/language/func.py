import ast
from dataclasses import dataclass, field
from enum import IntEnum, auto
from typing import Set, Optional


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


@dataclass
class Function:
    node: ast.FunctionDef
    arguments: Set[Argument] = field(init=False)
    return_type: str = field(init=False)
    item: Optional["Item"] = field(default=False)
    return_nodes: Set[ast.Return] = field(init=False, default_factory=set)

    def __post_init__(self):
        args = self.node.args

        self.arguments = (
            [Argument(node, ArgumentKind.PosOnly) for node in args.posonlyargs]
            + [Argument(node, ArgumentKind.Regular) for node in args.args]
            + ([Argument(args.vararg, ArgumentKind.Vararg)] if args.vararg else [])
            + [Argument(node, ArgumentKind.KwOnly) for node in args.kwonlyargs]
            + ([Argument(args.kwarg, ArgumentKind.Kwargs)] if args.kwarg else [])
        )

        self.return_type = self.node.returns or ast.Constant(value=None, kind=None)

    def __hash__(self) -> int:
        return hash(self.node)

    def __repr__(self) -> str:
        args = ", ".join(map(str, self.arguments))

        if (ty := self.return_type) is not None:
            if isinstance(ty, str):
                ret_ty = ty
            elif isinstance(ty, ast.Name):
                ret_ty = f"{ty.id}"
            elif isinstance(ty, ast.Constant) and ty.value is None:
                ret_ty = "None"
            else:
                assert isinstance(ty, ast.AST)
                ret_ty = ast.dump(ty)
        else:
            ret_ty = repr(ty)

        signature = f"{self.node.name}({args}) -> {ret_ty}"

        return f"<Function: {signature=!r}>"

    @property
    def name(self):
        return self.node.name

    @property
    def docstring(self) -> str:
        return ast.get_docstring(self.node)

    def disassemble(self) -> str:
        return repr(self)
