import ast
from typing import TYPE_CHECKING
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


def typecheck(item: Item, unit: CompilationUnit):
    assert isinstance(
        getattr(unit, "type_ctx", None), InferenceEngine
    ), "Malformed CompilationUnit does not have a valid InferenceEngine at `unit.type_ctx`"

    if (scope := item.scope) is None:
        raise ValueError(f"Item was expected to be scopeable but was not! {item=!r}")

    # We need some sort of type-level scope resolution here.

    # We are going to be using a "Rib"-based approach similar to the Rustc compiler.
    # See also: https://rustc-dev-guide.rust-lang.org/name-resolution.html?highlight=rib#scopes-and-ribs

    ribs: List[Dict[str, TypeInfo]] = []
    tcx = unit.type_ctx

    for scoped_item in scope.items:  # Construct the rib stack
        node = scoped_item.node

        type_id = (
            tcx.get_id_or_insert(scoped_item.ty)
            if isinstance(real_ty := scoped_item.ty, TypeInfo)
            else real_ty
        )

        assert isinstance(type_id, TypeId)

        if (
            tcx[type_id] == Primitive.Unknown
            and (func := scoped_item.function) is not None
        ):
            tcx[type_id] = Callable()
            print(tcx)

        real_ty = tcx[type_id]

        if real_ty is Primitive.LValue:  # Redefinition or addition of new lvalue.
            assert isinstance(node, (ast.Assign, ast.AnnAssign))
            assert isinstance(node.target, (ast.Name, ast.Subscript, ast.Attribute))
            assert isinstance(node, ast.AnnAssign)

            ident = node.target.id

            if (ann := node.annotation) is not None:
                annotation = tcx[unit.resolve_annotation(ann)]
            else:
                annotation = Primitive.Unknown

            assert isinstance(ident, str)

            if node.value is not None:
                # TODO: Type inference on RValue and assert the TypeId matches that of annotation.
                pass

            last_rib = (ribs and ribs[-1]) or {}

            ribs.append({**last_rib, ident: annotation})

            assert isinstance(
                node.target, ast.Name
            ), f"Can't handle other target forms yet."

            print(f"+ : {ident}: {tcx.reconstruct(annotation)}")
            print(ribs)

        elif real_ty is Primitive.Return:
            assert isinstance(item.node, ast.FunctionDef)
            print(f"Need to infer for {ast.dump(node)=!r}")

        elif isinstance(real_ty, Callable):
            typecheck(scoped_item, unit)

        else:
            raise TypeCheckError(f"Failed typechecking for a scoped item {scoped_item=!r}")
