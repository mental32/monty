__all__ = ("TypeId", "TypeInfo", "Primitive", "List", "Callable", "Ref")

TypeId = int

from typing import List as _List, Optional

from monty.diagnostic import Error

from .type_info import *
from .primitives import *
from .compound import *
from .type_context import *

if TYPE_CHECKING:
    from monty.language import Item
    from monty.unit import CompilationUnit


def typecheck(
    item: "Item", unit: "CompilationUnit", *, item_type_id: TypeId = 0, type_errors: Optional[_List["Diagnostic"]] = None
) -> _List["Diagnostic"]:
    assert isinstance(getattr(unit, "tcx", None), TypeContext)

    if (scope := item.scope) is None:
        raise ValueError(f"Item was expected to be scopeable but was not! {item=!r}")

    type_errors = type_errors or []
    tcx = unit.tcx

    for target in scope.items:
        node = target.node

        if isinstance(target.ty, TypeInfo):
            type_id = tcx.get_id_or_insert(target.ty)
        else:
            type_id = target.ty

        assert isinstance(type_id, TypeId), f"{type_id=!r}"

        if tcx[type_id] == Primitive.Unknown and (func := target.function) is not None:
            # TODO: hack here args and ret are Unknown for the moment
            arguments_type = type_id
            output_type = type_id

            func.type_id = tcx.insert(Callable(arguments_type, output_type))

            return typecheck(target, unit, item_type_id=type_id)

        real_type = tcx[type_id]

        if real_type is Primitive.Return:
            assert isinstance(item.node, ast.FunctionDef)
            raise NotImplementedError()
            # value = node.value

            # expr_ty = unit.reveal_type(value, scope)

            # if tcx.is_callable(expr_ty):  # "return f(...)" check `f`.output == `func`.output
            #     expr_ty = tcx[expr_ty].output

            # if expr_ty != tcx[item_type_id].output:
            #     type_errors.append(TypeCheckError(f"Bad return value for function!"))
        else:
            raise TypeCheckError(
                f"Failed typechecking for a scoped item {scoped_item=!r}"
            )

    return type_errors
