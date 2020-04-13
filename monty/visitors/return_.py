import ast

from . import BaseVisitor


class ReturnVisitor(BaseVisitor):
    def visit_Return(self, node):
        return_block = self.func.create_block()

        self.func.current_block = return_block

        if isinstance(node.value, ast.Constant):
            ssa_value = ConstantVisitor.encode_with_block(
                const=node.value, block=return_block
            )
            return_type = return_block.ssa_map[ssa_value]
        else:
            if self.func.current_block is None:
                self.func.current_block = return_block

            visitor = self.find_visitor(return_node := node.value)
            visitor.visit(node=return_node)

            assert self.func.current_block is return_block
            assert return_block.ssa_map, repr(return_block)

            return_type = self.func.returns
            ssa_value = max(return_block.ssa_map)

        if return_type != self.func.returns:
            raise BadReturnType(module=self.module, func=self.func, actual=return_type)

        return_block.return_(ssa_value)
