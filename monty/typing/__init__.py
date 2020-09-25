__all__ = ("TypeId", "TypeInfo", "Primitive", "List", "Callable", "Ref")

TypeId = int

import ast
from typing import List as _List, Optional


from monty.diagnostic import Error
from monty.errors import TypeCheckError
from monty.language import ImportDecl

from .type_info import *
from .primitives import *
from .compound import *
from .type_context import *

if TYPE_CHECKING:
    from monty.language import Item
    from monty.unit import CompilationUnit


def typecheck(
    item: "Item",
    unit: "CompilationUnit",
    *,
    item_type_id: TypeId = 0,
    type_errors: Optional[_List["Diagnostic"]] = None,
) -> _List["Diagnostic"]:
    import monty

    assert isinstance(getattr(unit, "tcx", None), TypeContext)

    if (scope := item.scope) is None:
        raise ValueError(f"Item was expected to be scopeable but was not! {item=!r}")

    type_errors = type_errors or []
    tcx = unit.tcx

    ribs: List[Dict[str, TypeInfo]] = []
    item.scope.ribs = ribs

    for target in scope.items:
        node = target.node

        if isinstance(target.ty, TypeInfo):
            type_id = tcx.get_id_or_insert(target.ty)
        else:
            type_id = target.ty

        assert isinstance(type_id, TypeId), f"{type_id=!r}"

        if (func := target.function) is not None:
            if tcx[type_id] == Primitive.Unknown or not hasattr(func, "type_id"):

                arguments = Tuple()
                arguments.inner = [unit.resolve_annotation(scope, arg.node.annotation) for arg in func.arguments]

                arguments_type = tcx.insert(arguments)
                output_type = unit.resolve_annotation(scope, func.return_type)

                func.type_id = tcx.insert(Callable(arguments_type, output_type))

            unit.functions[func.name] = func

            typecheck(target, unit, item_type_id=func.type_id)
            continue

        elif (real_type := tcx[type_id]) is Primitive.Return:
            assert isinstance(item.node, ast.FunctionDef)
            if (value := node.value) is not None:
                expr_ty = unit.reveal_type(value, scope)
            else:
                expr_ty = tcx.get_id_or_insert(Primitive.None_)

            # "return f(...)" check `f`.output == `func`.output
            if isinstance(tcx[expr_ty], Callable):
                expr_ty = tcx[expr_ty].output

            if expr_ty != tcx[item_type_id].output:
                type_errors.append(TypeCheckError(f"Bad return value for function!"))

        # Re-definition or addition of new name.
        elif real_type is Primitive.LValue:
            assert isinstance(node, (ast.Assign, ast.AnnAssign))
            assert isinstance(node, ast.AnnAssign)
            assert isinstance(
                node.target, ast.Name
            ), f"Can't handle other target forms yet."

            ident = node.target.id

            assert isinstance(ident, str)

            if (ann := node.annotation) is not None:
                annotation_id = unit.resolve_annotation(scope, ann)
            else:
                annotation_id = tcx.get_id_or_insert(Primitive.Unknown)

            assert type(annotation_id) is TypeId, f"{type(annotation_id)!r}"


            if node.value is not None:
                value_type_id = unit.reveal_type(node.value, scope)

            if value_type_id != annotation_id:
                value_type = tcx[value_type_id]
                annotation = tcx[annotation_id]
                raise TypeCheckError(
                    f"Type mismatch! expected {value_type.as_str(tcx)} instead got {annotation.as_str(tcx)}"
                )
            else:
                annotation = tcx[annotation_id]
                last_rib = (ribs and ribs[-1]) or {}
                ribs.append({**last_rib, ident: annotation})

            if tcx[annotation_id] is Primitive.Str:
                assert isinstance(node.value, ast.Constant)
                assert type(node.value.value) == str
                st = node.value.value
                unit.data.insert(value=st, origin=target)

        elif real_type is Primitive.Import:
            for alias in target.node.names:
                module = unit.import_module(decl := ImportDecl(node=alias))

                if module is None:
                    raise ModuleNotFoundError(repr(decl))

                unit.modules[module.name] = module
                assert module.path.exists()

                if module.path.is_file():
                    file_name = module.path
                else:
                    assert module.path.is_dir()
                    file_name = module.path / "__init__.py"

                with open(file_name) as inf:
                    monty.compile(inf, unit=unit, module_name=module.name)

        else:
            raise TypeCheckError(f"Failed typechecking for a scoped item {target=!r}")

    return type_errors
