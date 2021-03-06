import ast
import sys
from dataclasses import dataclass, field
from functools import singledispatch, singledispatchmethod
from typing import Any, Dict, List, Optional, Tuple, Union

from ..context import Context
from ..item import Item
from ..ty import Function, IntegerType, TypeInfo, BoolType
from ..utils import ASTNode


from .ebb import Ebb


def lower_into_mir(item: Item, ctx: Context):
    ebb = Ebb()
    _into_mir(item.node, item=item, ebb=ebb, ctx=ctx)
    return ebb


@singledispatch
def _into_mir(node: ASTNode, item: Item, ebb: Ebb, ctx: Context):
    assert False, (node, ebb)


@_into_mir.register
def _expr_into_mir(expr: ast.Expr, item: Item, ebb: Ebb, ctx: Context):
    _into_mir(expr.value, item, ebb, ctx)

@_into_mir.register
def _funcdef_into_mir(func: ast.FunctionDef, item: Item, ebb: Ebb, ctx: Context):
    assert ebb.is_empty, ebb

    function = item.kind
    assert isinstance(function, Function)

    # ebb.parameters += [callable_t.parameters]
    # ebb.returns = callable_t.output

    for arg, _arg in zip(function.args, func.args.args):
        value_ty = ctx.resolve_type(inty=arg)
        ty_size = value_ty.size()
        slot = ebb.create_stack_slot(ty_size, value_ty)
        ebb.name_to_stack_slot[_arg.arg] = slot

    _ = ebb.create_block()

    for part in func.body:
        _into_mir(part, item=ctx.ast_nodes[part], ebb=ebb, ctx=ctx)


@_into_mir.register
def _assign_into_mir(assign: ast.Assign, item: Item, ebb: Ebb, ctx: Context):
    [target_node] = assign.targets
    assert isinstance(target_node, ast.Name)

    value_node = assign.value
    value_type = ctx.resolve_type(inty=ctx.ast_nodes[value_node].kind)

    _into_mir(assign.value, item=ctx.ast_nodes[assign.value], ebb=ebb, ctx=ctx)

    assign_value = ebb.ast_node_to_ssa[value_node]

    if (target_id := target_node.id) not in ebb.name_to_stack_slot:
        ebb.name_to_stack_slot[target_id] = slot = ebb.create_stack_slot(
            size=value_type.size(), ty=value_type
        )

    slot = ebb.name_to_stack_slot[target_id]
    ebb.stack_store(slot, assign_value)


@_into_mir.register
def _pass_into_mir(_: ast.Pass, item: Item, ebb: Ebb, ctx: Context):
    ebb.nop()


@_into_mir.register
def _return_into_mir(ret: ast.Return, item: Item, ebb: Ebb, ctx: Context):
    ret_value: Optional[int]

    if (v := ret.value) is not None:
        _into_mir(ret.value, item=ctx.ast_nodes[v], ebb=ebb, ctx=ctx)
        ret_value = ebb.ast_node_to_ssa[v]
    else:
        ret_value = None

    ebb.return_(value=ret_value)


@_into_mir.register
def _name_into_mir(name: ast.Name, item: Item, ebb: Ebb, ctx: Context):
    if isinstance(name.ctx, ast.Load):
        target = name.id

        if target in ebb.name_to_stack_slot:
            slot = ebb.name_to_stack_slot[target]

            value = ebb.stack_load(slot)
            # value_type = item.kind

        else:
            it = item.scope.lookup(target, ctx=ctx)
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
def _const_into_mir(const: ast.Constant, item: Item, ebb: Ebb, ctx: Context):
    if const.value is None:
        ssa = ebb.int_const(0)

    elif (ty := type(value := const.value)) is bool:
        ssa = ebb.bool_const(value)

    elif ty is int:
        ssa = ebb.int_const(value)

    else:
        assert False, "unimplemented"

    ebb.ast_node_to_ssa[const] = ssa
    # ebb.ssa_value_types[ssa] = value_ty


@_into_mir.register
def _binop_into_mir(binop: ast.BinOp, item: Item, ebb: Ebb, ctx: Context):
    _into_mir(binop.left, item=ctx.ast_nodes[binop.left], ebb=ebb, ctx=ctx)
    _into_mir(binop.right, item=ctx.ast_nodes[binop.right], ebb=ebb, ctx=ctx)

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
def _call_into_mir(call: ast.Call, item: Item, ebb: Ebb, ctx: Context):
    # TODO: Name mangling...
    # name = self.resolve_name_to_mangled_form(name, ...)

    # func = self.unit.resolve_into_function(call, self.item.scope)
    func = ctx.ast_nodes[call.func].kind

    assert func is not None, f"{ast.dump(call)=!r}"

    if func not in ebb.refs:
        func_def = ebb.reference(func)
    else:
        inv = {o: s for s, o in ebb.refs.items()}
        func_def = inv[func]

    args = []

    for arg in call.args:
        _into_mir(arg, item=ctx.ast_nodes[arg], ebb=ebb, ctx=ctx)
        arg_value = ebb.ast_node_to_ssa[arg]
        args.append(arg_value)

    result = ebb.call(func_def, args=args)
    ebb.ast_node_to_ssa[call] = result


@_into_mir.register
def _compare_into_mir(compare: ast.Compare, item: Item, ebb: Ebb, ctx: Context):
    left = ctx.ast_nodes[compare.left]

    _into_mir(compare.left, item=left, ebb=ebb, ctx=ctx)

    result_ty = left.kind
    result = ebb.ast_node_to_ssa[compare.left]

    for (op, rvalue) in zip(compare.ops, compare.comparators):
        rvalue_item = ctx.ast_nodes[rvalue]
        rvalue_type = rvalue_item.kind

        _into_mir(rvalue, item=rvalue_item, ebb=ebb, ctx=ctx)

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
    # ebb.ssa_value_types[result] = self.unit.tcx.insert(primitives.Boolean())


@_into_mir.register
def _ifstmt_into_mir(if_: ast.If, item: Item, ebb: Ebb, ctx: Context):
    def flatten(node: ast.If):
        seq = []

        while True:
            current = node

            test = node.test
            body = node.body
            orelse = node.orelse

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
            _into_mir(node, item=ctx.ast_nodes[node], ebb=ebb, ctx=ctx)

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

    _into_mir(test, item=ctx.ast_nodes[test], ebb=ebb, ctx=ctx)

    expr_value = ebb.ast_node_to_ssa[test]
    expr_value = ebb.cast_bool_to_int(IntegerType, expr_value)

    head = ebb.create_block()
    _ = ebb.switch_to_block(head)

    for node in body:
        _into_mir(node, item=ctx.ast_nodes[node], ebb=ebb, ctx=ctx)

    tail = None
    for (_, _, node_) in orelse:
        tail = ebb.create_block()
        _ = ebb.switch_to_block(tail)

        _into_mir(node_, item=ctx.ast_nodes[node_], ebb=ebb, ctx=ctx)

    tail = ebb.create_block()
    _ = ebb.switch_to_block(tail)

    (_, else_body, _) = final
    for node_ in else_body:
        _into_mir(node_, item=ctx.ast_nodes[node_], ebb=ebb, ctx=ctx)

    _ = ebb.switch_to_block(test_)

    one = ebb.int_const(1)
    ebb.branch_icmp("eq", expr_value, one, head)

    if tail is not None:
        ebb.jump(tail)

    if tmp is not None:
        ebb.switch_to_block(tmp)


@_into_mir.register
def _while_into_mir(while_: ast.While, item: Item, ebb: Ebb, ctx: Context):
    head = ebb.create_block()
    body = ebb.create_block()
    tail = ebb.create_block()

    _ = ebb.switch_to_block(head)
    _into_mir(test := while_.test, item=ctx.ast_nodes[while_.test], ebb=ebb, ctx=ctx)
    test_value = ebb.ast_node_to_ssa[test]

    const_one = ebb.int_const(1)
    test_value = ebb.cast_bool_to_int(IntegerType, test_value)

    ebb.branch_icmp("eq", test_value, const_one, body)
    ebb.jump(tail)

    _ = ebb.switch_to_block(body)

    for node in while_.body:
        _into_mir(node, item=ctx.ast_nodes[node], ebb=ebb, ctx=ctx)
    else:
        ebb.jump(head)

    ebb.jump(head)
    ebb.switch_to_block(tail)
