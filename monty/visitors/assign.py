import ast

from . import BaseVisitor


class AssignVisitor(BaseVisitor):
    def visit_Assign(self, node):
        visitor = self.find_visitor(value := node.value)
        visitor.visit(node=value)

        top_value = max((block := self.func.current_block).ssa_map)
        value_kind = block.ssa_map[top_value]

        for target in node.targets:
            if isinstance(target, ast.Name):
                assert isinstance(
                    target.ctx, ast.Store
                ), f"Target name context was not store for an assignment! {ast.dump(node)}"
                self.func.variables[target.id] = value_kind
            else:
                raise CompilationError(
                    f"Assignment target not supported yet! {ast.dump(target)}"
                )

        block.def_var(target.id, top_value)
