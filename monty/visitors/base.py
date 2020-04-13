import ast
from dataclasses import dataclass, field
from typing import Optional

from .. import UnsupportedNode, CompilationError
from ..mir.module import Module
from ..mir.function import Function


@dataclass
class BaseVisitor(ast.NodeVisitor):
    module: Module
    func: Optional[Function] = field(default=None)

    def find_visitor(self, node) -> "BaseVisitor":
        from . import VISITORS

        node_type = type(node)

        visitor_type = VISITORS.get(node_type := type(node), None)
        if visitor_type is None:
            raise UnsupportedNode(f"Unsupported node kind! {node_type!r}")

        visitor = visitor_type(module=self.module, func=self.func,)

        return visitor

    def visit_FunctionDef(self, func_node):
        from . import FunctionVisitor

        try:
            func = Function.from_ast_node(self.module.qualname, func_node)
        except CompilationError as exc:
            raise exc

        self.module.add_function(func)
        visitor = FunctionVisitor(module=self.module, func=func)
        visitor.visit(func_node)
