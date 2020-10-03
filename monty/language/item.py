import ast
from dataclasses import dataclass, field, InitVar
from enum import IntEnum, auto
from typing import List, Iterator, Union, Optional, Any

from . import AbstractModule
from monty.typing import TypeId, Primitive
from monty.diagnostic import Diagnostic, Error

ScopeableNodes = Union[ast.FunctionDef, ast.Lambda, ast.Module, ast.ClassDef]


@dataclass
class Item:
    node: ast.AST = field(repr=False)
    unit: InitVar["CompilationUnit"] = field(default=None)
    ty: Union[TypeId, Primitive] = field(default=Primitive.Unknown)

    scope: Optional["Scope"] = None
    function: Optional["Function"] = None

    def __hash__(self) -> int:
        return hash(self.node)

    def __post_init__(self, unit):
        from monty.language import Scope, Function
        from monty.unit import CompilationUnit

        assert unit is None or isinstance(unit, CompilationUnit), repr(unit)

        if not isinstance(self.ty, (TypeId, Primitive)):
            raise TypeError(
                f"Item type must be a TypeId or a Primitive! got {self.ty=!r}"
            )

        if self.scope is None and self.is_scopable:
            module = self if isinstance(self.node, ast.Module) else None
            self.scope = Scope.from_node(self.node, module=module, unit=unit)

        if self.function is None and isinstance(self.node, ast.FunctionDef):
            self.function = Function(self.node)

    @property
    def is_scopable(self) -> bool:
        return isinstance(self.node, ScopeableNodes.__args__)

    def recursively_validate(self, *, diagnostics: Optional[List[Diagnostic]] = None):
        diagnostics = diagnostics or []

        if isinstance(self.node, ast.Module):
            for scoped_item in self.scope.items:
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
                scoped_item.recursively_validate(diagnostics=diagnostics)

    def getattr(self, attr: str) -> Any:
        if self.ty is Primitive.Module:
            assert isinstance(self.node, AbstractModule)
            return self.node.getattr(attr)
        else:
            raise NotImplementedError(attr)
