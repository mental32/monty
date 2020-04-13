import ast

from .. import CompilationError
from . import BaseVisitor


class NameVisitor(BaseVisitor):
    def visit_Name(self, node):
        if isinstance(node.ctx, ast.Load):

            if node.id not in self.func.variables:
                raise CompilationError(f"Unknown name! {ast.dump(node)=!r}")

            value = self.func.current_block.use_var(
                ident=node.id, kind=self.func.variables[node.id]
            )

            return value

        raise CompilationError(
            f"Unsopported context on NameVisitor for node {ast.dump(node)=}"
        )
