import ast
from enum import IntEnum, auto
from dataclasses import dataclass, field
from typing import List, Union, Dict, Optional

from . import RawType, Scope, AbstractBlock


class ArgKind(IntEnum):
    PosOnly = auto()
    Regular = auto()
    KwOnly = auto()
    VarArgs = auto()
    KwArgs = auto()


@dataclass
class Argument:
    kind: ArgKind
    name: str
    annotation: Union[str]


@dataclass
class Function:
    """A Python function."""

    qualname: str
    returns: RawType
    linkage: str = field(default="Export")
    arguments: Dict[str, Argument] = field(default_factory=dict)
    variables: Dict[str, RawType] = field(default_factory=dict)
    blocks: Dict[Union[int, str], AbstractBlock] = field(default_factory=dict)
    scope: Scope = field(default_factory=Scope)
    current_block: Optional[AbstractBlock] = field(init=False, default=None)

    # Alternate constructors

    @classmethod
    def from_ast_node(cls, path, node, *args, **kwargs) -> "Function":
        returns = RawType(name=node.returns and node.returns.id)

        raw_args = node.args

        collected_arguments = [
            (ArgKind.PosOnly, raw_args.posonlyargs),
            (ArgKind.Regular, raw_args.args),
            (ArgKind.VarArgs, [raw_args.vararg]),
            (ArgKind.KwOnly, raw_args.kwonlyargs),
            (ArgKind.KwArgs, [raw_args.kwarg]),
        ]

        arguments = {}

        for kind, group in collected_arguments:

            if group is None or set(group) == {None}:
                continue

            for argument in group:
                if argument.annotation is None:
                    raise Exception("Missing annotation!")

                annotation = (
                    argument.annotation.id
                    if isinstance(argument.annotation, ast.Name)
                    else argument.annotation.value
                )

                arguments[argument.arg] = Argument(
                    kind=kind, name=argument.arg, annotation=annotation
                )

        self = cls(
            qualname=f"{path}.{node.name}",
            returns=returns,
            arguments=arguments,
            *args,
            **kwargs,
        )

        self.scope.names.update(
            {name: RawType(name=arg.annotation) for name, arg in arguments.items()}
        )

        return self

    # Properties

    @property
    def name(self) -> str:
        return self.qualname

    # Public API

    def create_block(self) -> AbstractBlock:
        ident = max(self.blocks) + 1 if self.blocks else 0
        self.blocks[ident] = block = AbstractBlock()
        return block

    def into_raw(self, refs) -> dict:
        returns = self.returns.name

        def serialize(value: Argument) -> Dict[str, Union[str, int]]:
            return {
                "name": value.name,
                "annotation": refs.get(value.annotation, value.annotation),
                "kind": int(value.kind),
            }

        arguments = {name: serialize(arg) for name, arg in self.arguments.items()}

        blocks = {
            ident: {"ident": ident, **block.into_raw(refs)}
            for ident, block in self.blocks.items()
        }

        return {
            "qualname": self.qualname,
            "returns": refs.get(returns, returns),
            "arguments": arguments,
            "blocks": blocks,
            "linkage": self.linkage,
            "scope": {key: value.name for key, value in self.scope.names.items()},
            "variables": {
                name: refs.get(value.name, value.name)
                if isinstance(value, RawType)
                else value
                for name, value in self.variables.items()
            },
        }
