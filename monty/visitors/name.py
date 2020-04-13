class NameVisitor(BaseVisitor):
    def visit_Name(self, node):
        if isinstance(node.ctx, ast.Load):
            value = self.func.current_block.use_var(
                ident=node.id, kind=self.func.variables[node.id]
            )
        else:
            raise CompilationError(
                f"Unsopported context on NameVisitor for node {ast.dump(node)=}"
            )
