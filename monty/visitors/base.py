@dataclass
class BaseVisitor(ast.NodeVisitor):
    module: Module
    func: Optional[Function] = field(default=None)

    def find_visitor(self, node) -> "BaseVisitor":
        node_type = type(node)

        visitor_type = VISITORS.get(node_type := type(node), None)
        if visitor_type is None:
            raise UnsupportedNode(f"Unsupported node kind! {node_type!r}")

        visitor = visitor_type(module=self.module, func=self.func,)

        return visitor

    def visit_FunctionDef(self, func_node):
        try:
            func = Function.from_ast_node(self.module.qualname, func_node)
        except CompilationError as exc:
            raise exc
        else:
            self.module.add_function(func)
            visitor = FunctionVisitor(module=self.module, func=func)

            try:
                visitor.visit(func_node)
            except UnsupportedNode:
                raise
            except CompilationError:
                pass
