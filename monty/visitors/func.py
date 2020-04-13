class FunctionVisitor(BaseVisitor):
    def visit_FunctionDef(self, func_node):
        if self.func.current_block is None:
            self.func.current_block = self.func.create_block()

        for subnode in func_node.body:
            visitor = self.find_visitor(subnode)
            visitor.visit(node=subnode)
