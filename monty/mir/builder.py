import ast
from dataclasses import field, dataclass

from monty.mir import Ebb


@dataclass
class MirBuilder(ast.NodeVisitor):
    """Takes a regular AST and produces some MIR"""

    unit: "monty.driver.CompilationUnit"
    ebb: Ebb = field(default_factory=Ebb)

    @classmethod
    def compile_function(cls, unit: CompilationUnit, func: Function) -> Ebb:
        self = cls(unit)
        self.visit(func.node)
        return self.ebb

    def visit_Assign(self, assign):
        self.ebb.using_clean_block()

        with self.ebb.pin_head():
            self.generic_visit(assign)

        target = assign.targets[0].id

        self.ebb.assign("rv", self.ebb.last_ssa)

    def visit_Constant(self, const):
        self.ebb.using_clean_block()

        assert const.kind is None, f"Unhandled case! {ast.dump(const)=!r}"
        assert isinstance(
            const.value, (int, str)
        ), f"Only able to handle integer and string constants"

        if isinstance(value := const.value, str):
            st_ref = self.unit.intern_string(value)
            self.ebb.str_const(st_ref)
        else:
            self.ebb.int_const(value)
