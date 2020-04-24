import ast
from dataclasses import dataclass, field
from enum import IntEnum, auto
from typing import List, Iterator, Union, Optional

from monty.typechecker import TypeId, Primitive
from monty.diagnostic import Diagnostic, Error

ScopeableNodes = Union[ast.FunctionDef, ast.Lambda, ast.Module]


@dataclass
class Item:
    node: ast.AST = field(repr=False)
    ty: Union[TypeId, Primitive] = field(default=Primitive.Unknown)

    scope: Optional["Scope"] = None
    function: Optional["Function"] = None

    def __post_init__(self):
        from monty.language import Scope, Function

        if not isinstance(self.ty, (TypeId, Primitive)):
            raise TypeError(
                f"Item type must be a TypeId or a Primitive! got {self.ty=!r}"
            )

        if self.scope is None and self.is_scopable:
            self.scope = Scope.from_node(self.node)

        if self.function is None and isinstance(self.node, ast.FunctionDef):
            self.function = Function(self.node)

    @property
    def is_scopable(self) -> bool:
        return isinstance(self.node, ScopeableNodes.__args__)

    def validate(self, *, diagnostics: Optional[List[Diagnostic]] = None):
        diagnostics = diagnostics or []

        if isinstance(self.node, ast.Module):
            for scoped_item in (module := self.scope).items:
                if isinstance(scoped_item.node, ast.Return):
                    diagnostics.append(Error(f"Found return outside of function!"))

        elif self.function is not None:
            if (args := self.function.arguments) :
                seen = set()

                for arg in args:
                    if arg.name in seen:
                        diagnostics.append(
                            Error(
                                f"Duplicate argument '{arg.name}' in function definition."
                            )
                        )

                    elif arg.annotation is None:
                        diagnostics.append(
                            Error(f"Argument '{arg.name}' is missing a type annotation")
                        )

                    seen.add(arg.name)

        if (scope := self.scope) is not None:
            for scoped_item in scope.items:
                scoped_item.validate(diagnostics=diagnostics)
