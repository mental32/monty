import ast
from dataclasses import dataclass, field
from enum import IntEnum, auto
from typing import List, Iterator, Union, Optional, Dict, Callable as _Callable

from monty.utils import StrictASTVisitor
from .item import Item, ScopeableNodes
from . import ImportDecl

__all__ = ("Scope", "ScopeWalker")


@dataclass
class Scope:
    """Represents a semantic scope."""

    node: ast.AST
    unit: "CompilationUnit" = field()
    items: List[Item] = field(default_factory=list)
    parent: Optional["Scope"] = None
    ribs: List[Dict[str, "TypeInfo"]] = field(default_factory=list)
    module: Optional["Item"] = None

    def __hash__(self):
        return hash(self.node)

    @classmethod
    def from_node(
        cls,
        node: ScopeableNodes,
        *,
        module: Optional["Item"] = None,
        unit: "CompilationUnit",
    ) -> "Scope":
        assert unit is not None
        walker = ScopeWalker(scope=(s := Scope(node, module=module, unit=unit)))
        walker.visit(s.node)
        return walker.scope

    @property
    def scopes(self) -> List["Scope"]:
        from ..typing import Scope

        inner = []

        for item in self.items:
            if isinstance((func := item.node), ast.FunctionDef):
                inner.append(Scope.from_node(func))

        return inner

    def find(self, predicate: _Callable[[Item], bool]) -> Optional[Item]:
        for item in self.items:
            if predicate(item):
                return item
        else:
            return None

    def get_item_by_node(self, node: ast.AST) -> Optional["Item"]:
        for item in self.items:
            if item.node == node:
                return item
        else:
            return None

    def lookup(self, target_name: str) -> Optional[List[Item]]:
        """Perform a scope lookup of a particular name."""
        from ..typing import Callable, primitives, TypeInfo

        results = []

        if isinstance(self.node, ast.FunctionDef):
            item = next(filter((lambda it: it.node == self.node), self.parent.items))
            assert item.function is not None
            assert self.unit is not None

            for arg in item.function.arguments:
                if arg.name == target_name:
                    arg_type = self.unit.resolve_annotation(
                        ann_node=arg.node.annotation, scope=self
                    )
                    return [Item(ty=arg_type, node=arg)]

        for item in self.items:
            if isinstance(item.node, (ast.Assign, ast.AnnAssign)):
                assert isinstance(item.node, ast.AnnAssign)
                assert isinstance(item.node.target, ast.Name)

                if item.node.target.id == target_name:
                    results.append(item)

            elif item.scope is not None and (
                isinstance(item.scope.node, ast.FunctionDef)
                and item.function.name == target_name
            ):
                results.append(item)

            elif isinstance(item.ty, primitives.ImportType):
                if isinstance(item.node, ast.ImportFrom):
                    for alias in item.node.names:
                        if (
                            alias.asname is not None
                            and alias.asname == target_name
                            or alias.name == target_name
                        ):
                            module = self.unit.modules[item.node.module]
                            results.append(module.getattr(target_name))
                else:
                    assert isinstance(item.node, ast.Import)

                    for module_name in item.node.names:
                        if (
                            module_name.asname is not None
                            and module_name.asname == target_name
                            or module_name.name == target_name
                        ):
                            decl = ImportDecl(node=module_name, parent=item.node)
                            module = self.unit.modules[decl.qualname[0]]
                            results.append(module.as_item())

            elif False:
                raise NotImplementedError(item)

        if (parent := self.parent) is not None:
            results += parent.lookup(target_name)

        return results


@dataclass
class ScopeWalker(StrictASTVisitor):
    scope: Scope

    def __post_init__(self):
        assert self.scope is not None

    def add_item(self, **kwargs):
        item = Item(**kwargs, unit=self.scope.unit)

        if item.scope is not None:
            item.scope.parent = self.scope
            item.scope.module = self.scope.module

        self.scope.items.append(item)

    # Visitors

    visit_arguments = lambda _, __: None

    def visit_Module(self, module):
        from ..typing import primitives

        if self.scope.node is not module:
            self.add_item(ty=primitives.ModuleType(), node=module)

        self.generic_visit(module)

    def visit_FunctionDef(self, func):
        if func is self.scope.node:
            self.generic_visit(func)
        else:
            self.add_item(node=func)

    def visit_Import(self, imp):
        from ..typing import primitives

        self.add_item(node=imp, ty=primitives.ImportType())

    def visit_ImportFrom(self, imp):
        from ..typing import primitives

        self.add_item(node=imp, ty=primitives.ImportType())

    def visit_ClassDef(self, klass):
        raise NotImplementedError("Classes are not supported!")

    def visit_Assign(self, assign):
        raise NotImplementedError("Regular assignment is not supported!")

    def visit_AnnAssign(self, assign):
        from ..typing import primitives

        self.add_item(ty=primitives.Unknown(), node=assign)

    def visit_AugAssign(self, assign):
        raise NotImplementedError("AugAssign is not supported!")

    def visit_Return(self, ret):
        from ..typing import primitives

        self.add_item(ty=primitives.Unknown(), node=ret)

    def visit_Assert(self, assert_):
        from ..typing import primitives

        self.add_item(ty=primitives.Unknown(), node=assert_)        

    def visit_Expr(self, expr):
        from ..typing import primitives

        self.add_item(ty=primitives.Unknown(), node=expr)

    def visit_Name(self, name):
        pass # Noop
