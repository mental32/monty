import ast
from contextlib import contextmanager
from dataclasses import field, dataclass
from typing import Dict, Any, Iterator

from monty import typing
from monty.language import Item
from monty.typing import TypeId, TypeInfo, Primitive, Pointer
from monty.utils import swapattr
from monty.unit import CompilationUnit

from . import Ebb, SSAValue, FluidBlock


__all__ = ("Module", "MirBuilder")


@dataclass
class MirBuilder(ast.NodeVisitor):
    """An `ast.NodeVisitor` that can lower a Python `ast.FunctionDef` into MIR."""

    unit: CompilationUnit
    item: Item

    _ebb: FluidBlock = field(default_factory=FluidBlock)
    _ast_node_to_ssa: Dict[ast.AST, SSAValue] = field(default_factory=dict)
    _name_to_stack_slot: Dict[str, SSAValue] = field(default_factory=dict)

    @staticmethod
    def compile_item(item: Item, *, unit: CompilationUnit) -> Ebb:
        """Walk and lower a function item and ast into an Ebb."""
        if item.function is None:
            raise TypeError(f"language item was not a function! {item=!r}")

        assert item.function is not None
        assert isinstance(item.node, ast.FunctionDef)

        function = item.function

        callable_t = unit.tcx[function.type_id]

        assert isinstance(callable_t, typing.Callable)

        self = MirBuilder(unit, item)

        self._ebb.parameters += [callable_t.parameters]
        self._ebb.returns = callable_t.output

        for arg in function.arguments:
            value_ty = self.unit.resolve_annotation(scope=self.item.scope, ann_node=arg.node.annotation)
            ty_size = self.unit.size_of(value_ty) if isinstance(value_ty, Primitive) else self.unit.tcx[value_ty].size()
            self._name_to_stack_slot[arg.name] = slot = self._ebb.create_stack_slot(
                ty_size, value_ty
            )
 
        with self._ebb.with_block():
            self.visit(function.node)

        return self._ebb.finalize()

    # Helpers

    @contextmanager
    def _visiting_names(self):
        def visit_name(self, name):
            assert isinstance(name.ctx, ast.Load)

            target = name.id

            if target in self._name_to_stack_slot:
                slot = self._name_to_stack_slot[target]
                value = self._ebb.stack_load(slot)
                value_type = self.unit.reveal_type(
                    name, self.item.scope
                )

            else:
                item, *_ = self.item.scope.lookup(target)
                data_ref = self.unit.data.fetch_by_origin(item)
                value = self._ebb.load_data(data_ref)
                value_type = self.unit.reveal_type(item.node, item.scope)

            self._ast_node_to_ssa[name] = value
            self._ebb.ssa_value_types[value] = value_type


        with swapattr(self, "_visit_name", None, visit_name):
            yield

    # Visitors

    def visit_AnnAssign(self, assign):
        value_node = assign.value
        value_ty = self.unit.reveal_type(value_node, self.item.scope)

        self.generic_visit(assign)

        assign_value = self._ast_node_to_ssa[value_node]
        target_id = assign.target.id

        # variable_id = self.unit.get_variable_id(target_id, self.item)

        # self._ebb.assign(ident=target_id, value=assign_value, ty=value_ty)
        if target_id not in self._name_to_stack_slot:
            ty_size = self.unit.size_of(value_ty) if isinstance(value_ty, Primitive) else self.unit.tcx[value_ty].size()
            self._name_to_stack_slot[target_id] = slot = self._ebb.create_stack_slot(
                ty_size, value_ty
            )
        else:
            slot = self._name_to_stack_slot[target_id]

        self._ebb.stack_store(slot, assign_value)
        # print(f"{self._name_to_stack_slot=!r}")

    def visit_Pass(self, _):
        self._ebb.nop()

    def visit_Return(self, ret):
        ret_value_node = ret.value

        if ret_value_node is not None:
            with self._visiting_names():
                self.generic_visit(ret)

            ret_value = self._ast_node_to_ssa[ret_value_node]
        else:
            ret_value = None

        self._ebb.return_(value=ret_value)

    def visit_Name(self, name):
        # Hack to selectively encode ast.Name nodes.
        if callable(fn := getattr(self, "_visit_name", None)):
            assert callable(fn)
            fn(self, name)  # pylint: disable=not-callable

    def visit_Constant(self, const):
        assert const.kind is None, f"Unhandled case! {ast.dump(const)=!r}"
        assert type(const.value) in (
            int,
            bool,
            str,
        ), f"Only able to handle integer, boolean, and string constants {ast.dump(const)=!r}"

        ty = type(value := const.value)

        if ty is str:
            value = self.unit.data.insert(value, origin=const)
            fn = self._ebb.str_const
            inner_ty = self.unit.tcx.insert(Primitive.StrSlice)
            value_ty = self.unit.tcx.insert(Pointer(ty=inner_ty))

        elif ty is bool:
            fn = self._ebb.bool_const
            value_ty = self.unit.tcx.insert(Primitive.Bool)

        elif ty is int:
            fn = self._ebb.int_const
            value_ty = self.unit.tcx.insert(Primitive.I64)

        else:
            assert False, f"Unknown fn handler for {ty=!r}!"

        self._ast_node_to_ssa[const] = ssa = fn(value)
        self._ebb.ssa_value_types[ssa] = value_ty

    def visit_BinOp(self, binop):
        with self._visiting_names():
            self.generic_visit(binop)

        lhs = self._ast_node_to_ssa[binop.left]
        rhs = self._ast_node_to_ssa[binop.right]

        ty = self.unit.reveal_type(binop, self.item.scope)

        NUMBER_TYPES = (Primitive.I64, Primitive.I32, Primitive.Int)

        assert (
            self.unit.tcx[ty] in NUMBER_TYPES
        ), f"{self.unit.tcx.reconstruct(ty)!r}"

        kind = self.unit.tcx[ty]
        op = type(binop.op)

        if kind in NUMBER_TYPES:
            fn = {ast.Add: self._ebb.int_add, ast.Sub: self._ebb.int_sub,}[op]

            value = fn(lhs, rhs)
            self._ebb.ssa_value_types[value] = self.unit.tcx.insert(
                Primitive.I64
            )
        else:
            raise Exception(f"Attempted BinOp on unknown kinds {ast.dump(binop)}")

        self._ast_node_to_ssa[binop] = value

    def visit_Call(self, call):
        # TODO: Name mangling...
        # name = self.resolve_name_to_mangled_form(name, ...)

        func = self.unit.resolve_into_function(call, self.item.scope)

        assert func is not None, f"{ast.dump(call)=!r}, {self.item.scope=!r}"

        if func not in self._ebb.refs:
            func = self._ebb.reference(func)

        args = []

        with self._visiting_names():
            for arg in call.args:
                self.visit(arg)
                arg_value = self._ast_node_to_ssa[arg]
                args.append(arg_value)

        result = self._ebb.call(func, args=args)

        self._ast_node_to_ssa[call] = result

    def visit_Compare(self, compare):
        left = compare.left

        with self._visiting_names():
            self.visit(left)

        result_ty = self.unit.reveal_type(left, self.item.scope)
        result = self._ast_node_to_ssa[left]

        i64 = self.unit.tcx.insert(Primitive.I64)

        for (op, rvalue,) in zip(compare.ops, compare.comparators):
            rvalue_type = self.unit.reveal_type(rvalue, self.item.scope)

            self.visit(rvalue)

            rvalue_ssa = self._ast_node_to_ssa[rvalue]
            rvalue_type = self.unit.tcx[rvalue_type]

            if rvalue_type == Primitive.Bool:
                rvalue_ssa = self._ebb.cast_bool_to_int(i64, rvalue_ssa)
                rvalue_type = Primitive.I64

            if rvalue_type in (Primitive.I64, Primitive.I32, Primitive.Int):
                ops = {ast.Eq: "eq", ast.NotEq: "neq", ast.Gt: "gt"}

                if (op := type(op)) not in ops:
                    raise Exception(f"Unknown op {ast.dump(compare)=!r}")
                else:
                    result = self._ebb.icmp(ops[op], result, rvalue_ssa)

                result = self._ebb.cast_bool_to_int(i64, result)
                result_ty = i64
            else:
                raise Exception(f"Unkown rvalue type on comparator {rvalue_type=!r}")

        result_ty = self.unit.tcx[result_ty]

        if result_ty in (Primitive.I64, Primitive.I32, Primitive.Int,):
            result = self._ebb.bool_const(result, is_ssa_value=True)

        self._ast_node_to_ssa[compare] = result
        self._ebb.ssa_value_types[result] = self.unit.tcx.insert(
            Primitive.Bool
        )

    def visit_If(self, if_):
        with self._visiting_names():
            self.visit(if_.test)

        expr_value = self._ast_node_to_ssa[if_.test]

        if self._ebb.ssa_value_types[expr_value] != (
            i64 := self.unit.tcx.insert(Primitive.I64)
        ):
            expr_value = self._ebb.cast_bool_to_int(i64, expr_value)

        with self._ebb.with_block() as ident:
            for node in if_.body:
                self.visit(node)
            head = ident

        tail = None
        for node in if_.orelse:
            with self._ebb.with_block() as ident:
                self.visit(node)
                tail = ident

        one = self._ebb.int_const(1)
        self._ebb.branch_icmp("eq", expr_value, one, head)

        if tail is not None:
            self._ebb.jump(tail)

    def visit_While(self, while_):
        i64 = self.unit.tcx.insert(Primitive.I64)

        # print(f"{ast.dump(while_)=!r}")

        head = self._ebb.create_block()
        body = self._ebb.create_block()
        tail = self._ebb.create_block()

        with self._visiting_names():
            with self._ebb.with_block(head):
                self.visit(test := while_.test)
                test_value = self._ast_node_to_ssa[test]
                assert (a := self._ebb.ssa_value_types[test_value]) == (
                    e := self.unit.tcx.insert(Primitive.Bool)
                ), f"{a=!r} {e=!r}"
                const_one = self._ebb.int_const(1)
                test_value = self._ebb.cast_bool_to_int(i64, test_value)
                self._ebb.branch_icmp("eq", test_value, const_one, body)
                self._ebb.jump(tail)

        with self._ebb.with_block(body):
            for node in while_.body:
                self.visit(node)
            self._ebb.jump(head)

        self._ebb.jump(head)
        self._ebb.switch_to_block(tail)


@dataclass
class Module:
    """Used to construct a module."""

    unit: CompilationUnit
    root: Item
    output: Any = field(default=None)

    def walk_function_items(self) -> Iterator[Item]:
        seen = set()

        for sub in self.root.scope.items:
            if sub.function is not None and sub.function.node not in seen:
                yield sub
                seen.add(sub.function.node)

    def lower_into_mir(self) -> Dict[str, Ebb]:
        return {
            item.function.name: MirBuilder.compile_item(unit=self.unit, item=item)
            for item in self.walk_function_items()
        }
