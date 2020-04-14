import ast
from dataclasses import dataclass, field
from types import MappingProxyType
from typing import Dict, Union, Optional, Callable, Any, Tuple, Iterator
from functools import partial

from ..mir import Scope, RawType, Import
from . import TypedAST

__all__ = ("BaseVisitor",)


@dataclass
class BaseVisitor(ast.NodeVisitor):
    """Core AST visitor class."""

    typed: Optional[TypedAST] = field(init=False, default=None)

    def __getattribute__(self, name):
        try:
            return object.__getattribute__(self, name)
        except AttributeError:
            print(f"Attribute doesn't exist! {name!r}")
            raise

    # Helpers

    def _map_import(self, node, func, typed = None):
        typed = typed

        for alias in node.names:
            imp = func(alias)
            typed.scope.imports.add(imp)
            typed.add(alias, imp)

    # Public

    def visit_raw(self, tree: ast.AST, *, typed: Optional[TypedAST] = None) -> TypedAST:
        """Visit a raw CPython ast.AST and produce a TypedAST."""
        assert self.typed is None, repr(self)

        self.typed = typed or TypedAST(tree=tree)
        self.visit(tree)

        return self.typed

    # Visitors

    def visit_Import(self, node):
        self._map_import(
            node,
            func=(lambda alias: Import(name=str(alias.name), alias=alias.asname, level=0)),
            typed=self.typed,
        )

    def visit_ImportFrom(self, node):
        def into_import(alias) -> Import:
            return Import(
                name=f"{node.module}.{alias.name}", alias=alias.asname, level=node.level
            )

        self._map_import(node, into_import, typed=self.typed)

    def visit_FunctionDef(self, func):
        typed_func = TypedAST(tree=func)

        klass = type(self)

        for node in func.body:

            if isinstance(node, (ast.Import, ast.ImportFrom)):
                klass().visit_raw(node, typed=typed_func)
            else:
                typed_func.add(node, klass().visit_raw(node))

        self.typed.add(func, typed_func)

    def visit_Expr(self, expr):
        raise NotImplementedError()

    def visit_UnaryOp(self, unary):
        raise NotImplementedError()

    def visit_BinOp(self, binop):
        raise NotImplementedError()

    def visit_BoolOp(self, boolop):
        raise NotImplementedError()

    def visit_Compare(self, cmp):
        raise NotImplementedError()

    def visit_Call(self, call):
        raise NotImplementedError()

    def visit_Return(self, ret):
        klass = type(self)

        typed = self.typed if not (is_module := isinstance(self.typed.tree, ast.Module)) else TypedAST(tree=ret)

        typed.add(ret.value, klass().visit_raw(ret.value))

        if is_module:
            assert self.typed is not typed
            self.typed.add(ret, typed)

    def visit_Constant(self, const):
        const_ty = type(const.value)
        const_name = const_ty.__name__

        const_value = {
            int: ast.Num,
            str: ast.Str,
        }[const_ty]

        self.typed.add(const, RawType(name=const_name, impl=const_ty))
