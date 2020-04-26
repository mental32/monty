import ast
from typing import TYPE_CHECKING, List, Optional
from dataclasses import dataclass, field

from .type_info import *
from .inference_engine import *

from monty.language import Item
from monty.driver import CompilationUnit
from monty.errors import TypeCheckError


@dataclass
class LValue:
    name: str
    kind: TypeInfo = field(default=Primitive.Unknown)


def typecheck(item: Item, unit: CompilationUnit, *, item_type_id: TypeId = 0, type_errors = None):
    assert isinstance(
        getattr(unit, "type_ctx", None), InferenceEngine
    ), "Malformed CompilationUnit does not have a valid InferenceEngine at `unit.type_ctx`"

    if (scope := item.scope) is None:
        raise ValueError(f"Item was expected to be scopeable but was not! {item=!r}")

    type_errors = type_errors or []

    # We are going to be using a "Rib"-based approach for name/type tracking in scopes like the Rustc compiler.
    # See also: https://rustc-dev-guide.rust-lang.org/name-resolution.html?highlight=rib#scopes-and-ribs

    ribs: List[Dict[str, TypeInfo]] = []
    tcx = unit.type_ctx

    for scoped_item in scope.items:  # Construct the rib stack
        node = scoped_item.node

        type_id = (
            tcx.get_id_or_insert(real_ty)
            if isinstance(real_ty := scoped_item.ty, TypeInfo)
            else real_ty
        )

        assert isinstance(type_id, TypeId)

        if (
            tcx[type_id] == Primitive.Unknown
            and (func := scoped_item.function) is not None
        ):
            arguments = tcx.get_id_or_insert(Primitive.Nothing if not func.arguments else Primitive.Unknown)
            output = unit.resolve_annotation(scope, func.return_type)

            tcx[type_id] = Callable(arguments, output)

            return typecheck(scoped_item, unit, item_type_id=type_id)

        real_ty = tcx[type_id]

        if real_ty is Primitive.LValue:  # Redefinition or addition of new lvalue.
            assert isinstance(node, (ast.Assign, ast.AnnAssign))
            assert isinstance(node.target, (ast.Name, ast.Subscript, ast.Attribute))
            assert isinstance(node, ast.AnnAssign)

            ident = node.target.id

            assert isinstance(ident, str)

            if (ann := node.annotation) is not None:
                annotation_id = unit.resolve_annotation(scope, ann)
            else:
                annotation_id = tcx.get_id_or_insert(Primitive.Unknown)

            assert type(annotation_id) is TypeId, f"{type(annotation_id)!r}"

            annotation = tcx[annotation_id]

            if node.value is not None:
                pass  # TODO: Type inference on RValue and assert the TypeId matches that of annotation.

            last_rib = (ribs and ribs[-1]) or {}

            ribs.append({**last_rib, ident: annotation})

            assert isinstance(
                node.target, ast.Name
            ), f"Can't handle other target forms yet."

            # print(f"+ : {ident}: {tcx.reconstruct(annotation_id)}")
            # print(ribs)

        elif real_ty is Primitive.Return:
            assert isinstance(item.node, ast.FunctionDef)
            value = node.value

            if isinstance(value, ast.Constant):
                expr_ty = unit.resolve_annotation(scope, value)
            else:
                expr_ty = tcx.get_id_or_insert(Primitive.Unknown)

            if expr_ty != tcx[item_type_id].output:
                type_errors.append(TypeCheckError(f"Bad return value for function!"))

        else:
            raise TypeCheckError(
                f"Failed typechecking for a scoped item {scoped_item=!r}"
            )

    return type_errors
