import ast
from dataclasses import dataclass, field
from functools import singledispatchmethod
import sys
from sys import breakpointhook
from typing import Optional, Set, Dict, TYPE_CHECKING, Union

from .item import Item
from .mirgen import Ebb
from .utils import ASTNode, TypeInfo
from .ty import Function, KlassType, IntegerType, BoolType, TypeRef, UnknownType

if TYPE_CHECKING:
    from monty import Module


@dataclass
class Context:
    """Big blob of compilation state."""

    modules: Dict[str, "Module"] = field(default_factory=dict)
    functions: Set[Function] = field(default_factory=set)
    klass_types: Set[KlassType] = field(default_factory=set)

    ast_nodes: Dict[ASTNode, Item] = field(default_factory=dict)
    source_map: Dict[ast.Module, str] = field(default_factory=dict)
    builtin_types: Dict[TypeInfo, ASTNode] = field(default_factory=dict)

    def __post_init__(self):
        self.autoimpls = autoimpls = {}
        autoimpls[IntegerType] = [
            Function(name="__add__", args=[IntegerType, IntegerType], ret=IntegerType),
            Function(name="__mul__", args=[IntegerType, IntegerType], ret=IntegerType),
            Function(name="__sub__", args=[IntegerType, IntegerType], ret=IntegerType),
            Function(name="__eq__", args=[IntegerType, IntegerType], ret=BoolType),
            Function(name="__ne__", args=[IntegerType, IntegerType], ret=BoolType),
            Function(name="__gt__", args=[IntegerType, IntegerType], ret=BoolType),
            Function(name="__lt__", args=[IntegerType, IntegerType], ret=BoolType),
            Function(name="__ge__", args=[IntegerType, IntegerType], ret=BoolType),
            Function(name="__le__", args=[IntegerType, IntegerType], ret=BoolType),
        ]

    def node_entry(self, node: ASTNode) -> Item:
        return self.ast_nodes[node]

    def type_eq(self, lhs: TypeInfo, rhs: TypeInfo) -> bool:
        return self.resolve_type(lhs) == self.resolve_type(rhs)

    def get_qualname(self, item: Item) -> str:
        assert isinstance(item.node, (ast.FunctionDef, ast.Module, ast.ClassDef))

        def name_of(node: ASTNode) -> str:
            if isinstance(node, ast.ClassDef):
                stem = node.name

            elif isinstance(node, ast.Module):
                stem = cursor.span.module_name

            elif isinstance(node, ast.FunctionDef):
                return self._ast_nodes[node].kind.name  # type: ignore

            else:
                raise NotImplementedError(parent)

            return stem

        qualname = f"{name_of(item.node)!s}"

        cursor = item
        parent = None

        while cursor is not parent:
            parent = cursor.get_direct_parent_node(ctx=self)
            assert parent is not None

            qualname = f"{name_of(parent)}.{qualname!s}"

            if isinstance(parent, ast.Module):
                break
            else:
                cursor = self.ast_nodes[parent]

        return qualname

    def resolve_type(self, inty: Union[TypeInfo, ASTNode]) -> TypeInfo:
        """Return a fully qualified type i.e. not a type-ref."""

        assert isinstance(inty, (TypeInfo, ASTNode))

        if isinstance(inty, ASTNode):
            ty = self.ast_nodes[inty].kind
        else:
            assert isinstance(inty, TypeInfo)
            ty = inty

        assert isinstance(ty, TypeInfo), repr(ty)

        while isinstance(ty, TypeRef):
            ty = self.ast_nodes[ty.other.node].kind

        if isinstance(ty, KlassType):
            inverted_bultin_map = {
                node: prim for prim, node in self.builtin_types.items()
            }

            if ty.node in inverted_bultin_map:
                ty = inverted_bultin_map[ty.node]

        return ty

    def check_builtins(self, target: str) -> Optional[Function]:
        for func in self.functions:
            if func.name == f"builtins.{target!s}":
                return func
        else:
            return None

    def fmt(self, node: ASTNode) -> str:
        entry = self.ast_nodes[node]
        assert entry.kind is not None

        kind = self.resolve_type(entry.kind)
        st = (
            kind.__str__()
            if not isinstance(kind, KlassType)
            else self.get_qualname(self.ast_nodes[kind.node])
        )

        return f"{entry.span.display()!r}" f" has type {st!r}"

    def lower_into_mir(self, item: Item):
        ebb = Ebb()
        self._into_mir(item.node, item=item, ebb=ebb)
        return ebb

    @singledispatchmethod
    def _into_mir(self, node: ASTNode, item: Item, ebb: Ebb):
        assert False, (node, ebb)

    @_into_mir.register
    def _funcdef_into_mir(self, func: ast.FunctionDef, item: Item, ebb: Ebb):
        assert ebb.is_empty, ebb

        function = item.kind
        assert isinstance(function, Function)

        # ebb.parameters += [callable_t.parameters]
        # ebb.returns = callable_t.output

        for arg, _arg in zip(function.args, func.args.args):
            value_ty = self.resolve_type(inty=arg)
            ty_size = value_ty.size()
            slot = ebb.create_stack_slot(ty_size, value_ty)
            ebb.name_to_stack_slot[_arg.arg] = slot

        _ = ebb.create_block()

        for part in func.body:
            self._into_mir(part, item=self.ast_nodes[part], ebb=ebb)

    @_into_mir.register
    def _assign_into_mir(self, assign: ast.Assign, item: Item, ebb: Ebb):
        [target_node] = assign.targets
        assert isinstance(target_node, ast.Name)

        value_node = assign.value
        value_type = self.resolve_type(inty=self.ast_nodes[value_node].kind)

        self._into_mir(assign.value, item=self.ast_nodes[assign.value], ebb=ebb)

        assign_value = ebb.ast_node_to_ssa[value_node]

        if (target_id := target_node.id) not in ebb.name_to_stack_slot:
            ebb.name_to_stack_slot[target_id] = slot = ebb.create_stack_slot(
                size=value_type.size(), ty=value_type
            )

        slot = ebb.name_to_stack_slot[target_id]
        ebb.stack_store(slot, assign_value)

    @_into_mir.register
    def _pass_into_mir(self, _: ast.Pass, item: Item, ebb: Ebb):
        ebb.nop()

    @_into_mir.register
    def _return_into_mir(self, ret: ast.Return, item: Item, ebb: Ebb):
        ret_value: Optional[int]

        if (v := ret.value) is not None:
            self._into_mir(ret.value, item=self.ast_nodes[v], ebb=ebb)
            ret_value = ebb.ast_node_to_ssa[v]
        else:
            ret_value = None

        ebb.return_(value=ret_value)

    @_into_mir.register
    def _name_into_mir(self, name: ast.Name, item: Item, ebb: Ebb):
        if isinstance(name.ctx, ast.Load):
            target = name.id

            if target in ebb.name_to_stack_slot:
                slot = ebb.name_to_stack_slot[target]

                value = ebb.stack_load(slot)
                # value_type = item.kind

            else:
                it = item.scope.lookup(target, ctx=self)
                assert it is not None

                if isinstance(it.node, ast.arg):
                    slot = ebb.name_to_stack_slot[target]
                    value = ebb.stack_load(slot)
                    # value_type = item.kind
                else:
                    # value_type = it.kind
                    value = sys.maxsize

            ebb.ast_node_to_ssa[name] = value

    @_into_mir.register
    def _const_into_mir(self, const: ast.Constant, item: Item, ebb: Ebb):
        assert type(const.value) in (
            int,
            bool,
        ), f"Only able to handle integer, boolean, and string constants {ast.dump(const)=!r}"

        ty = type(value := const.value)

        # if ty is str:
        #     # value = self.data.insert(value, origin=const)
        #     ssa = ebb.str_const(Pointer(inner=StringType))

        if ty is bool:
            ssa = ebb.bool_const(value)

        elif ty is int:
            ssa = ebb.int_const(value)

        else:
            assert False

        ebb.ast_node_to_ssa[const] = ssa
        # ebb.ssa_value_types[ssa] = value_ty

    @_into_mir.register
    def _binop_into_mir(self, binop: ast.BinOp, item: Item, ebb: Ebb):
        self._into_mir(binop.left, item=self.ast_nodes[binop.left], ebb=ebb)
        self._into_mir(binop.right, item=self.ast_nodes[binop.right], ebb=ebb)

        lhs = ebb.ast_node_to_ssa[binop.left]
        rhs = ebb.ast_node_to_ssa[binop.right]

        kind = item.kind
        op = type(binop.op)

        if kind is IntegerType:
            fn = {
                ast.Add: ebb.int_add,
                ast.Sub: ebb.int_sub,
            }[op]

            value = fn(lhs, rhs)
            ebb.ast_node_to_ssa[binop] = value
        else:
            raise Exception(f"Attempted BinOp on unknown kinds {ast.dump(binop)}")

    @_into_mir.register
    def _call_into_mir(self, call: ast.Call, item: Item, ebb: Ebb):
        # TODO: Name mangling...
        # name = self.resolve_name_to_mangled_form(name, ...)

        # func = self.unit.resolve_into_function(call, self.item.scope)
        func = self.ast_nodes[call.func].kind

        assert func is not None, f"{ast.dump(call)=!r}"

        if func not in ebb.refs:
            func_def = ebb.reference(func)
        else:
            inv = {o: s for s, o in ebb.refs.items()}
            func_def = inv[func]

        args = []

        for arg in call.args:
            self._into_mir(arg, item=self.ast_nodes[arg], ebb=ebb)
            arg_value = ebb.ast_node_to_ssa[arg]
            args.append(arg_value)

        result = ebb.call(func_def, args=args)
        ebb.ast_node_to_ssa[call] = result

    @_into_mir.register
    def _compare_into_mir(self, compare: ast.Compare, item: Item, ebb: Ebb):
        left = self.ast_nodes[compare.left]

        self._into_mir(compare.left, item=left, ebb=ebb)

        result_ty = left.kind
        result = ebb.ast_node_to_ssa[compare.left]

        for (op, rvalue) in zip(compare.ops, compare.comparators):
            rvalue_item = self.ast_nodes[rvalue]
            rvalue_type = rvalue_item.kind

            self._into_mir(rvalue, item=rvalue_item, ebb=ebb)

            rvalue_ssa = ebb.ast_node_to_ssa[rvalue]

            if rvalue_ssa is BoolType:
                rvalue_ssa = ebb.cast_bool_to_int(IntegerType, rvalue_ssa)
                rvalue_type = IntegerType

            if rvalue_type is IntegerType:
                ops = {ast.Eq: "eq", ast.NotEq: "neq", ast.Gt: "gt", ast.LtE: "sle"}

                if (op := type(op)) not in ops:  # type: ignore
                    raise Exception(f"Unknown op {ast.dump(compare)=!r}")
                else:
                    result = ebb.icmp(ops[op], result, rvalue_ssa)  # type: ignore

                result = ebb.cast_bool_to_int(IntegerType, result)
                result_ty = IntegerType
            else:
                raise Exception(f"Unkown rvalue type on comparator {rvalue_type=!r}")

        if result_ty is IntegerType:
            result = ebb.bool_const(result, is_ssa_value=True)

        ebb.ast_node_to_ssa[compare] = result

    #     ebb.ssa_value_types[result] = self.unit.tcx.insert(primitives.Boolean())

    @_into_mir.register
    def _ifstmt_into_mir(self, if_: ast.If, item: Item, ebb: Ebb):
        def flatten(node: ast.If):
            seq = []

            while True:
                current = node

                test = node.test
                body = node.body
                orelse = node.orelse

                print(orelse)

                # Detect an `elif`
                if len(orelse) == 1 and isinstance(orelse[0], ast.If):
                    [node] = orelse  # type: ignore
                    assert isinstance(node, ast.If)
                    assert node is not current

                if not isinstance(test, ast.Constant):
                    # we can't prove this atm.
                    seq.append((test, body, current))

                elif bool(test.value):
                    # a true case in the middle of a chain invalidates the rest of the checks
                    # due to short-circuiting.
                    seq.append((test, body, current))
                    break

                assert orelse

                # if `node` hasn't changed then there is no next `elif`
                # this means we're at the `else`
                if node is current:
                    tail = ast.If(test=(t := ast.Constant(value=True)), body=orelse)
                    seq.append((t, orelse, tail))
                    break

                del test, body, orelse

            return seq

        [(test, body, _), *orelse] = flatten(if_)

        assert isinstance(_, ast.If), _

        if not orelse:
            assert test.value

            for node in body:
                self._into_mir(node, item=self.ast_nodes[node], ebb=ebb)

            return

        [*orelse, final] = orelse

        assert isinstance(body, list)
        assert isinstance(test, ASTNode)

        if not ebb.head:
            test_ = ebb.blocks.index(ebb.head)
            tmp = None
        else:
            test_ = ebb.create_block()
            tmp = ebb.switch_to_block(test_)

        self._into_mir(test, item=self.ast_nodes[test], ebb=ebb)

        expr_value = ebb.ast_node_to_ssa[test]
        expr_value = ebb.cast_bool_to_int(IntegerType, expr_value)

        head = ebb.create_block()
        _ = ebb.switch_to_block(head)

        for node in body:
            self._into_mir(node, item=self.ast_nodes[node], ebb=ebb)

        tail = None
        for (_, _, node_) in orelse:
            tail = ebb.create_block()
            _ = ebb.switch_to_block(tail)

            self._into_mir(node_, item=self.ast_nodes[node_], ebb=ebb)

        tail = ebb.create_block()
        _ = ebb.switch_to_block(tail)

        (_, else_body, _) = final
        for node_ in else_body:
            self._into_mir(node_, item=self.ast_nodes[node_], ebb=ebb)

        _ = ebb.switch_to_block(test_)

        one = ebb.int_const(1)
        ebb.branch_icmp("eq", expr_value, one, head)

        if tail is not None:
            ebb.jump(tail)

        if tmp is not None:
            ebb.switch_to_block(tmp)

    @_into_mir.register
    def _while_into_mir(self, while_: ast.While, item: Item, ebb: Ebb):
        head = ebb.create_block()
        body = ebb.create_block()
        tail = ebb.create_block()

        _ = ebb.switch_to_block(head)
        self._into_mir(test := while_.test, item=self.ast_nodes[while_.test], ebb=ebb)
        test_value = ebb.ast_node_to_ssa[test]

        # assert (a := ebb.ssa_value_types[test_value]) == (
        #     e := BoolType
        # ), f"{a=!r} {e=!r}"

        const_one = ebb.int_const(1)
        test_value = ebb.cast_bool_to_int(IntegerType, test_value)

        ebb.branch_icmp("eq", test_value, const_one, body)
        ebb.jump(tail)

        _ = ebb.switch_to_block(body)

        for node in while_.body:
            self._into_mir(node, item=self.ast_nodes[node], ebb=ebb)
        else:
            ebb.jump(head)

        ebb.jump(head)
        ebb.switch_to_block(tail)
