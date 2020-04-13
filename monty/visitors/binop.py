
class BinOpVisitor(BaseVisitor):
    def visit_BinOp(self, node):
        block = self.func.current_block

        results = {node.left: None, node.right: None}
        for subnode in [node.left, node.right]:
            visitor = self.find_visitor(subnode)
            visitor.visit(node=subnode)

            assert self.func.current_block is block
            results[subnode] = max(self.func.current_block.ssa_map)

        left, right = results[node.left], results[node.right]

        assert left is not None
        assert right is not None

        if block.ssa_map[left] != block.ssa_map[right]:
            raise CompilationError("Bad types for BinOp")

        kind = block.ssa_map[left]

        match = {
            ast.Sub: block.sub,
            ast.Add: block.add,
            ast.Mult: block.mul,
            ast.FloorDiv: block.floor_div,
            ast.Mod: block.modulo,
            ast.Pow: block.pow,
            ast.LShift: block.lshift,
            ast.RShift: block.rshift,
            ast.BitOr: block.bitwise_or,
            ast.BitXor: block.bitwise_xor,
            ast.BitAnd: block.bitwise_and,
        }

        try:
            func = match[type(node.op)]
        except KeyError:
            raise CompilationError(f"Op not implemented yet! {ast.dump(node)}")
        else:
            func(kind, x=left, y=right)
