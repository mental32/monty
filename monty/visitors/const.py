class ConstantVisitor(BaseVisitor):
    @staticmethod
    def encode_with_block(
        *, const: ast.Constant, block: "ir.AbstractBlock"
    ) -> "ir.SSAValue":
        if const.kind is None:
            # Gotta rely on the type(node.value)

            kind = type(const.value)

            if kind is int:
                value = block.iconst(RawType(name="int"), const.value)
            else:
                assert False, f"Not supported! {ast.dump(const)!r}"
        else:
            assert False, "Not implemented!"

        return value

    def visit_Constant(self, node):
        assert (
            self.func.current_block is not None
        ), f"Not focused on any block! {self.func!r}"
        self.encode_with_block(const=node, block=self.func.current_block)
