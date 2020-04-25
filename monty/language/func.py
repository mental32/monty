import ast
from dataclasses import dataclass, field
from typing import Set, Optional

from . import Argument, ArgumentKind


@dataclass
class Function:
    node: ast.FunctionDef
    arguments: Set[Argument] = field(init=False)
    return_type: str = field(init=False)

    _returns: Set[ast.Return] = field(init=False, default_factory=set)

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

    def __repr__(self) -> str:
        args = ", ".join(map(str, self.arguments))

        if (ty := self.return_type) is not None:
            if isinstance(ty, str):
                ret_ty = ty
            elif isinstance(ty, ast.Name):
                ret_ty = f"{ty.id}"
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
    def return_nodes(self):
        return self._returns

    def add_return(self, node: ast.Return):
        self._returns.add(node)
