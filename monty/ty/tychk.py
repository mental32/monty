import ast
from functools import partial, singledispatch
import typing
from monty.ty import BoolType, Function, KlassType, ModuleType, NoneType, PRIMITIVE_TYPES, PrimitiveBase, TypeRef, UnknownType
from typing import List, Optional, TYPE_CHECKING, Tuple, Union

from ..utils import ASTInfix, ASTNode, LEGAL_NODE_TYPES, TypeInfo, collapse_attribute, is_visible, panic
from ..item import Item
from ..context import Context


def _validate_argument(argument: ast.arg, item: Item) -> ast.arg:
    assert isinstance(argument, ast.arg)

    if getattr(argument, "annotation", None) is None:
        span = item.span
        assert (
            False
        ), f"[{span.lineno}:{span.col_offset}]: All non-self parameters must be type annotated."

    return argument

# Typechecking

def typecheck(entry: Item, ctx: Context):
    """Recursively typecheck some node entry."""

    assert entry.scope is not None

    for item in entry.scope:
        item_node: ASTNode = item.node

        if item_node is entry.node:
            continue

        _typecheck_node(item.node, item=item, ctx=ctx)
        continue

@singledispatch
def _typecheck_node(node: ASTNode, item: Item, ctx: Context):
    SUPPORTED_NODES = {
        ast.While,
        ast.Store,
        ast.Load,
        ast.Name,
        ast.Constant,
        ast.arg,
        ast.Expr,
        ast.alias,
        ast.Import,
        ast.ImportFrom,
        ast.Attribute,
        ast.Pass,
        ast.BinOp,
        ast.LtE,
        ast.Add,
        ast.Sub,
        ast.Mult,
    }

    if type(node) not in SUPPORTED_NODES:
        raise NotImplementedError(item)

@_typecheck_node.register
def _typecheck_classdef(node: ast.ClassDef, item: Item, ctx: Context):
    typecheck(entry=item, ctx=ctx)

@_typecheck_node.register
def _typecheck_arguments(node: ast.arguments, item: Item, ctx: Context):
    names = set()

    for posarg in node.posonlyargs:
        assert posarg.arg not in names, "duplicate posonly argument in function definition."
        names.add(posarg.arg)

    for regarg in node.args:
        assert regarg.arg not in names, "duplicate argument in function definition."
        names.add(regarg.arg)

@_typecheck_node.register
def _typecheck_funcdef(node: ast.FunctionDef, item: Item, ctx: Context):
    assert isinstance(item.kind, Function)

    typecheck(entry=item, ctx=ctx)

    if (
        not any(isinstance(it.node, ast.Return) for it in item.scope)
        and item.kind.ret is not NoneType
    ):
        span_info = item.span

        raise TypeError(
            f"\n| [{span_info.lineno}:{span_info.col_offset}]: expected type {item.kind.ret}, found {NoneType}."
            f"\n|\t{span_info.display(lines=1)!r}"
                "\n|"
            f"\n| implicitly returns None as its body has no `return` statement."
        )

@_typecheck_node.register
def _typecheck_attribute(node: ast.Attribute, item: Item, ctx: Context):
    assert item.kind is not None

    if ctx.type_eq(item.kind, UnknownType):
        kinds = "\n".join(
            f"|\t{ctx.fmt(node)!s}"
            for node in ast.walk(item.node)
            if is_visible(node) and node is not item.node
        )

        raise TypeError(
            f"\n| [{item.span.lineno}:{item.span.col_offset}]: Unable to infer type for attribute access..."
            f"\n|\t{item.span.display()!r}"
            f"\n| where:\n{kinds!s}"
        )

@_typecheck_node.register
def _typecheck_ifstmt(node: ast.If, item: Item, ctx: Context):
    t = ctx.ast_nodes[node.test]
    t_k = t.kind
    assert t_k is not None

    t_t = ctx.resolve_type(t_k)

    if isinstance(node.test, ast.Constant):
        _ = bool(node.test.value)
        t_t = BoolType

    assert t_t is BoolType, ast.dump(t.node)

@_typecheck_node.register
def _typecheck_return(node: ast.Return, item: Item, ctx: Context):
    assert item.kind is not None

    parent_node = item.get_direct_parent_node(ctx=ctx)
    assert parent_node is not None
    assert isinstance(parent_node, ast.FunctionDef), parent_node

    parent_item = ctx.ast_nodes[parent_node]
    assert isinstance(parent_item.kind, Function)

    parent_item_kind = (
        parent_item.kind.ret
        if isinstance(parent_item.kind, Function)
        else parent_item.kind
    )

    expected = ctx.resolve_type(parent_item_kind)
    actual = ctx.resolve_type(item.kind)

    if not ctx.type_eq(expected, actual):
        span_info = ctx.ast_nodes[item.node].span

        kinds = "\n".join(
            f"|\t{ctx.fmt(node)!s}"
            for node in ast.walk(node)
            if is_visible(node) and node is not node
        )

        raise TypeError(
            f"\n| [{span_info.lineno}:{span_info.col_offset}]: Attempted to return with type {actual}, expected type {expected}"
            f"\n|\t{span_info.display()!r}"
            + (f"\n| where:\n{kinds!s}" if kinds else "")
        )

@_typecheck_node.register
def _typecheck_augassign(node: ast.AugAssign, item: Item, ctx: Context):
    raise NotImplementedError()

@_typecheck_node.register
def _typecheck_annassign(node: ast.AnnAssign, item: Item, ctx: Context):
    annotation, value = node.annotation, node.value

    assert value is not None

    expected_kind = ctx.ast_nodes[annotation].kind
    assert expected_kind is not None
    expected = ctx.resolve_type(expected_kind)

    actual_kind = ctx.ast_nodes[value].kind
    assert actual_kind is not None
    actual = ctx.resolve_type(actual_kind)

    if not ctx.type_eq(expected, actual):
        span_info = ctx.ast_nodes[item.node].span
        raise TypeError(
            f"[{span_info.lineno}:{span_info.col_offset}]: Expected type {expected.__str__()!r} instead got {actual.__str__()!r}"
        )

@_typecheck_node.register(ast.Compare)
@_typecheck_node.register(ast.BinOp)
def _typecheck_binop(node, item: Item, ctx: Context):
    if item.kind is UnknownType:
        parent = item.get_direct_parent_node(ctx=ctx)
        assert parent is not None
        parent_entry = ctx.ast_nodes[parent]
        item_span = parent_entry.span

        kinds = "\n".join(
            f"|\t{ctx.fmt(node)!s}"
            for node in ast.iter_child_nodes(parent)
            if is_visible(node) and node is not item.node
        )

        raise TypeError(
            f"\n| [{item_span.lineno}:{item_span.col_offset}]: Unable to infer the result type of this expression..."
            f"\n|\t{item_span.display()!r}"
            f"\n| where:\n{kinds!s}"
        )

@_typecheck_node.register
def _typecheck_assign(node: ast.Assign, item: Item, ctx: Context):
    assert len(node.targets) == 1

@_typecheck_node.register
def _typecheck_call(node: ast.Call, item: Item, ctx: Context):
    assert isinstance(item.node, ast.Call)
    f_entry = ctx.ast_nodes[item.node.func]

    if isinstance(f_entry.kind, TypeRef):
        f_entry.kind = ctx.resolve_type(f_entry.kind)

    span_info = ctx.ast_nodes[item.node].span
    func_kind = f_entry.kind

    if not isinstance(func_kind, Function):
        assert func_kind is not None
        actual = func_kind

        raise TypeError(
            f"[{span_info.lineno}:{span_info.col_offset}]: Type {actual} is not callable."
        )

    assert isinstance(func_kind, Function)

    if isinstance(func_kind.ret, TypeRef):
        func_kind.ret = ctx.resolve_type(func_kind.ret)

    for i, arg in enumerate(func_kind.args):
        func_kind.args[i] = ctx.resolve_type(arg)

    args: List[TypeInfo] = [
        ctx.resolve_type(infer(ctx.ast_nodes[arg_], ctx=ctx) or panic())
        for arg_ in item.node.args
    ]

    assert all(arg not in {UnknownType, None} for arg in args)

    callsite_signature = Function(name=func_kind.name, args=args, ret=func_kind.ret)

    if callsite_signature != func_kind:
        if callsite_signature.args != func_kind.args:
            n_expected_args = len(func_kind.args)
            n_actual_args = len(callsite_signature.args)
            assert (
                n_expected_args != n_actual_args
            ), f"{callsite_signature.args} != {func_kind.args}"
            plural = lambda n: "s" if n > 1 else ""
            reason = (
                f"this function takes {n_expected_args}"
                f" argument{plural(n_expected_args)} but"
                f" {n_actual_args} arguments were supplied."
            )
        else:
            assert False

        raise TypeError(span_info.fmt(reason))

# Inference

def infer(item: Item, ctx: Context) -> Optional[TypeInfo]:
    """Attempt to infer the type of some item."""
    if item.kind not in {None, UnknownType}:
        # Happy path!
        return item.kind

    return _infer_node(item.node, item=item, ctx=ctx)

@singledispatch
def _infer_node(
    node: ASTNode, item: Item, ctx: Context
) -> Optional[TypeInfo]:
    item_node_type = type(item.node)

    if item_node_type not in LEGAL_NODE_TYPES:
        raise NotImplementedError(
            f"{item_node_type!r} is not supported at the moment."
        )

    if item_node_type is ast.arguments:
        return UnknownType

    if item_node_type in (
        ast.Store,
        ast.Load,
        ast.AnnAssign,
        ast.Assign,
        ast.AugAssign,
        ast.While,
        ast.ImportFrom,
        ast.Import,
        ast.Pass,
        ast.LtE,
        ast.If,
    ):
        return NoneType

    elif item_node_type is ast.Module:
        return ModuleType

    return None

@_infer_node.register
def _infer_classdef(
    node: ast.ClassDef, item: Item, ctx: Context
) -> Optional[TypeInfo]:
    return KlassType(node=node, name=node.name)

@_infer_node.register
def _infer_const(
    node: ast.Constant, item: Item, ctx: Context
) -> Optional[TypeInfo]:
    kind = PRIMITIVE_TYPES[type(node.value)]
    assert isinstance(kind, TypeInfo)
    return getattr(kind, "ret", kind)

@_infer_node.register
def _infer_name(
    node: ast.Name, item: Item, ctx: Context
) -> Optional[TypeInfo]:
    assert isinstance(item.node, ast.Name)

    if (
        isinstance(item.parent, ast.Assign)
        or isinstance(item.parent, ast.AnnAssign)
        and item.parent.annotation is not item.node
    ):
        # Type of the name depends on the type of the expression or the annotation.
        if isinstance(item.parent, ast.Assign):
            oof = typing.cast("ASTNode", item.parent.value)
        else:
            assert isinstance(item.parent, ast.AnnAssign)
            oof = typing.cast("ASTNode", item.parent.annotation)

        assert oof is not None

        dependent_entry = ctx.ast_nodes[oof]

        return TypeRef(other=dependent_entry)

    elif isinstance(item.node.ctx, ast.Load):
        assert item.scope is not None
        result = item.scope.lookup(target=item.node.id, ctx=ctx)

        return TypeRef(other=result) if result is not None else None

    else:
        assert isinstance(item.node.ctx, ast.Store)
        parent_node = item.get_direct_parent_node(ctx=ctx)
        assert parent_node is not None
        assert isinstance(parent_node, (ast.Assign, ast.AnnAssign))
        assert parent_node.value is not None
        value = ctx.ast_nodes[parent_node.value]
        return TypeRef(other=value)

@_infer_node.register
def _infer_funcdef(
    node: ast.FunctionDef, item: Item, ctx: Context
) -> Optional[TypeInfo]:
    assert isinstance(item.node, ast.FunctionDef)
    arguments: List[TypeInfo] = []

    if any(ast.iter_fields(item.node.args)):
        f_args = item.node.args

        for group in [f_args.posonlyargs, f_args.args, f_args.kwonlyargs]:
            for argument in group:
                if argument.arg != "self":
                    _validate_argument(argument, item)

                argument_entry = ctx.ast_nodes[argument]
                argument_type = infer(argument_entry, ctx=ctx)
                assert argument_type is not None
                arguments.append(argument_type)

    return_annotation = item.node.returns
    ret: TypeInfo = NoneType

    if return_annotation is not None:
        node_ = infer(ctx.ast_nodes[return_annotation], ctx=ctx)
        assert node_ is not None
        ret = node_.ret if isinstance(node_, Function) else node_

    return Function(name=item.node.name, args=arguments, ret=ret)

@_infer_node.register
def _infer_attribute(
    node: ast.Attribute, item: Item, ctx: Context
) -> Optional[TypeInfo]:
    assert isinstance(item.node, ast.Attribute)
    assert item.scope is not None

    result = item.scope.lookup(st := collapse_attribute(item.node), ctx=ctx)

    assert result is not None

    if result.node is item.node:
        for (name, module) in ctx.modules.items():
            module_node = module.root.node
            assert isinstance(module_node, ast.Module)

            if name == st:
                return ModuleType

            if st.startswith(name):
                assert st[len(name)] == "."
                me = ctx.ast_nodes[module_node]
                assert me.scope is not None
                result = me.scope.lookup(target=st[len(name) + 1 :], ctx=ctx)
                assert result is not None
                return result.kind
        else:
            return UnknownType
    else:
        return result.kind

@_infer_node.register
def _infer_alias(
    node: ast.alias, item: Item, ctx: Context
) -> Optional[TypeInfo]:
    assert isinstance(item.node, ast.alias)

    import_node = item.get_direct_parent_node(ctx=ctx)
    assert import_node is not None

    if not isinstance(import_node, ast.ImportFrom):
        return ModuleType

    module_name = import_node.module
    assert isinstance(module_name, str)

    module_ = ctx.modules[module_name].root
    assert module_.scope is not None

    result = module_.scope.lookup(target=item.node.name, ctx=ctx)
    assert result is not None
    return result.kind

@_infer_node.register(ast.Import)
@_infer_node.register(ast.ImportFrom)
def _infer_import(
    node: Union[ast.Import, ast.ImportFrom], item: Item, ctx: Context
) -> Optional[TypeInfo]:
    assert isinstance(item.node, (ast.ImportFrom, ast.Import))

    for al in item.node.names:
        ty = infer(ctx.ast_nodes[al], ctx=ctx)
        assert ty is not None
        ctx.ast_nodes[al].kind = ty

    return NoneType

@_infer_node.register
def _infer_expr(
    node: ast.Expr, item: Item, ctx: Context
) -> Optional[TypeInfo]:
    assert isinstance(item.node, ast.Expr)
    return TypeRef(other=ctx.ast_nodes[item.node.value])

def _infer_infix_func(
    op: ASTInfix, left: TypeInfo, right: TypeInfo, ctx: Context
) -> Optional[Function]:
    def into_resolved_parts(ty: TypeInfo):
        ty = ctx.resolve_type(inty=ty)

        if isinstance(ty, PrimitiveBase):
            impl = ctx.builtin_types[ty]
            imit = ctx.ast_nodes[impl]
            name = ctx.get_qualname(imit)
            return name, ty

        elif isinstance(ty, KlassType):
            inverted_builtin_map = {
                node: ty for ty, node in ctx.builtin_types.items()
            }

            if ty.node in inverted_builtin_map:
                ty = inverted_builtin_map[ty.node]
                return into_resolved_parts(ty)

        raise NotImplementedError(ty)

    _ast_binop_name_map = {
        ast.Add: "add",
        ast.Sub: "sub",
        ast.Mult: "mul",
        ast.Gt: "gt",
        ast.Lt: "lt",
        ast.Eq: "eq",
        ast.NotEq: "ne",
        ast.LtE: "le",
    }

    _ast_binop_name = _ast_binop_name_map[type(op)]



    (lhs_name, lhs) = into_resolved_parts(ty=left)
    assert isinstance(lhs, PrimitiveBase)

    (rhs_name, rhs) = into_resolved_parts(ty=right)
    assert isinstance(rhs, PrimitiveBase)

    ltr = Function(
        name=f"{lhs_name}.__{_ast_binop_name}__",
        args=[lhs, rhs],
        ret=UnknownType,
    )

    rtl = Function(
        name=f"{rhs_name}.__r{_ast_binop_name}__",
        args=[rhs, lhs],
        ret=UnknownType,
    )

    def unify(f: Function) -> Optional[Function]:
        for other in ctx.functions:
            if (
                other.name != f.name
                or len(other.args) != len(f.args)
                or f.ret is not UnknownType
                and f.ret != other.ret
            ):
                continue

            assert len(other.args) == len(f.args)

            def check(lr: Tuple[TypeInfo, TypeInfo]) -> bool:
                (l, r) = lr
                assert r is not UnknownType
                return (l is UnknownType) or (l == r)

            if all(map(check, zip(f.args, other.args))):
                return other
        else:
            return None

    return unify(ltr) or unify(rtl)

@_infer_node.register(ast.BinOp)
def _infer_binop(
    node: ast.BinOp, item: Item, ctx: Context
) -> Optional[TypeInfo]:
    f = partial(infer, ctx=ctx)
    (lhs, rhs) = map(f, map(ctx.node_entry, [node.left, node.right]))

    assert lhs is not None
    assert lhs is not UnknownType, node.left

    assert rhs is not None
    assert rhs is not UnknownType, node.right
    assert isinstance(node.op, ASTInfix.__args__)  # type: ignore

    func = _infer_infix_func(op=node.op, left=lhs, right=rhs, ctx=ctx)

    if func is None:
        return UnknownType

    return func.ret

@_infer_node.register(ast.Compare)
def _infer_compare(node, item: Item, ctx: Context) -> Optional[TypeInfo]:
    assert isinstance(node, ast.Compare)

    # The compare node is formed weirdly, we have to perform a shift-reduce
    # style operation.

    acc_node = node.left
    acc_item = ctx.ast_nodes[acc_node]

    acc = infer(item=acc_item, ctx=ctx)
    assert acc is not None

    acc = ctx.resolve_type(acc)

    for (op, rhs) in zip(node.ops, node.comparators):
        right = infer(item=ctx.ast_nodes[rhs], ctx=ctx)

        assert right is not None, (right, ast.dump(rhs))
        assert isinstance(right, TypeInfo)
        assert isinstance(op, ASTInfix.__args__)  # type: ignore

        assert acc is not None

        f = _infer_infix_func(op=op, left=acc, right=right, ctx=ctx)
        assert f is not None, (op, acc, right)

        acc = f.ret

    return acc

@_infer_node.register(ast.Add)
@_infer_node.register(ast.Sub)
@_infer_node.register(ast.Mult)
def _infer_infix_op(node, item: Item, ctx: Context) -> Optional[TypeInfo]:
    return NoneType
    parent = item.get_direct_parent_node(ctx=ctx)
    assert isinstance(parent, ast.BinOp), parent
    return _infer_binop(node=parent, item=ctx.ast_nodes[parent], ctx=ctx)

@_infer_node.register
def _infer_call(
    node: ast.Call, item: Item, ctx: Context
) -> Optional[TypeInfo]:
    func_kind: Optional[TypeInfo]

    if isinstance(node.func, ast.Attribute):
        attr_ty = infer(ctx.ast_nodes[node.func], ctx)
        assert attr_ty is not None
        assert attr_ty is not UnknownType
        func_kind = attr_ty
    else:
        assert isinstance(node.func, ast.Name)
        assert item.scope is not None

        if (f := item.scope.lookup(target=node.func.id, ctx=ctx)) is not None:
            assert isinstance(f, Item)

            if f.kind is UnknownType:
                k = infer(item=f, ctx=ctx)
                assert k is not None
                f.kind = k

            func_kind = f.kind
            assert func_kind is not UnknownType, f
        else:
            func_kind = ctx.check_builtins(node.func.id)

    if not isinstance(func_kind, Function):
        return UnknownType
    else:
        return func_kind.ret

@_infer_node.register
def _infer_arg(node: ast.arg, item: Item, ctx: Context) -> Optional[TypeInfo]:
    assert item.scope is not None
    assert isinstance(item.node, ast.arg)

    arg_parent = item.get_direct_parent_node(ctx=ctx)

    assert isinstance(arg_parent, ast.FunctionDef)

    funcdef_item = ctx.ast_nodes[arg_parent]
    funcdef_parent = funcdef_item.get_direct_parent_node(ctx)
    assert funcdef_parent is not None
    in_classdef = isinstance(funcdef_parent, ast.ClassDef)
    is_self_arg = node.arg == "self"

    if is_self_arg:
        assert in_classdef

    if in_classdef and is_self_arg:
        klass_type = ctx.ast_nodes[funcdef_parent]
        return TypeRef(other=klass_type)

    arg = _validate_argument(node, item)

    assert arg.annotation is not None

    assert isinstance(arg.annotation, (ast.Name, ast.Constant))

    if isinstance(arg.annotation, ast.Name):
        annotation_id = arg.annotation.id
    else:
        assert isinstance(arg.annotation, ast.Constant)
        annotation_id = arg.annotation.value

    result = item.scope.lookup(target=annotation_id, ctx=ctx)

    if result is None:
        return (
            f_.ret
            if (f_ := ctx.check_builtins(annotation_id)) is not None
            else None
        )
    else:
        return result.kind

@_infer_node.register
def _infer_return(
    node: ast.Return, item: Item, ctx: Context
) -> Optional[TypeInfo]:
    assert isinstance(item.node, ast.Return)

    if item.node.value is not None:
        value = ctx.ast_nodes[item.node.value]
        return TypeRef(other=value)
    else:
        return NoneType
