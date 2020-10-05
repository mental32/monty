import ast
from typing import Dict, Optional, List as _List, Optional

import monty
from monty.diagnostic import Error
from monty.errors import TypeCheckError
from monty.language import ImportDecl, Item, Scope, Function
from monty.utils import SSAMap

from . import TypeInfo, TypeId, primitives
from . import compound

__all__ = ("TypeContext",)


class TypeContext(SSAMap[TypeInfo]):
    def __init__(self):
        super().__init__()

        assert not self.mapping

        unknown = self.insert(primitives.Unknown())
        assert unknown == 0, f"Failed to slot Primitive.Unknown at type_id 0!"

        self.insert(primitives.Int64())
        self.insert(primitives.Int32())
        self.insert(primitives.Boolean())
        self.insert(primitives.NoneType())

    def __repr__(self) -> str:
        return f"<TypeContext: {self.mapping=!r}>"

    # Helpers

    def primitives(self) -> Dict[str, TypeId]:
        return {
            "int": self[primitives.Integer()],
            "i64": self[primitives.Int64()],
            "i32": self[primitives.Int32()],
            "none": self[primitives.NoneType()],
            "bool": self[primitives.Boolean()],
        }

    def reconstruct(self, kind: TypeId) -> str:
        return self[kind].as_str(self)

    # "is_*" helpers

    def is_tuple(self, type_id: TypeId) -> bool:
        return type(self[type_id]) == compound.Tuple

    # Typechecking

    def _typecheck_expr(
        self,
        *,
        item: "Item",
        unit: "CompilationUnit",
        scope: "Scope",
    ) -> _List["Diagnostic"]:
        from monty.mir import libc

        if isinstance(item, Item):
            assert isinstance(item.node, ast.Expr), f"{ast.dump(item.node)=!r}"
            node = item.node.value
        else:
            node = item

        print(node, unit.is_node_docstring(node))

        if unit.is_node_docstring(node):
            # Skip doc-string nodes.
            return []

        type_errors = [Error(f"oof: {ast.dump(node)}")]

        if isinstance(node, ast.Call):
            # Call(value: ast.AST, args: List[ast.AST])
            #
            #  * Make sure the value being called IS callable
            #  * Make sure the arguments and keyword arguments can match the
            #    values function signature.

            func = unit.resolve_into_function(node, scope=scope)

            assert isinstance(func, (Function, libc.ExternalFunction)), f"{func=!r}"

            if isinstance(func, Function):
                f_args = list(map(self.__getitem__, [unit.resolve_annotation(ann_node=argument.annotation, scope=scope) for argument in func.arguments]))
            else:
                f_args, _, = func.signature

            if (n := len(f_args)) != (k := len(node.args)):
                type_errors.append(Error(f"Function call expected {n} arguments, instead got {k}"))
                return type_errors

            def type_of(arg: ast.AST):
                if isinstance(arg, ast.Name):
                    assert isinstance(arg.ctx, ast.Load), f"{ast.dump(arg)}"
                    tmp = scope.lookup(arg.id)
                    assert tmp is not None
                    a, *_ = tmp

                    if type(a.ty) is int:
                        t = self[a.ty]
                    else:
                        t = a.ty

                    assert isinstance(t, TypeInfo), repr(t)
                    return t

                elif isinstance(arg, ast.Constant):
                    return primitives.Primitive.from_builtin_type(type(arg.value))

                elif isinstance(arg, ast.Call):
                    self._typecheck_expr(item=arg, unit=unit, scope=scope)
                    func_ = unit.resolve_into_function(arg, scope=scope)
                    if isinstance(func_, Function):
                        return self[unit.resolve_annotation(ann_node=func_.return_type, scope=scope)]
                    else:
                        _, ret, = func_.signature
                        return ret

                else:
                    assert False, f"{ast.dump(arg)}"

                return primitives.Unknown()

            actual_types = [type_of(arg) for arg in node.args]

            assert all(isinstance(ty, TypeInfo) for ty in actual_types), repr(actual_types)

            for idx, [expected, actual], in enumerate(zip(f_args, actual_types)):
                # breakpoint()
                if expected != actual:
                    print(expected, actual)
                    type_errors.append(Error(f"Bad argument, expected type {expected.as_str(self)!r} instead got {actual.as_str(self)} at {idx} position function {func=!r}"))

            # assert f_args == actual_types, f"{f_args=!r} != {actual_types=!r}"
            # assert False, f"{func} {ast.dump(node)} {f_args} {actual_types}"

        else:
            raise NotImplementedError(f"{ast.dump(node)}")

        return type_errors

    def typecheck(
        self,
        item: "Item",
        unit: "CompilationUnit",
        *,
        item_type_id: TypeId = 0,
    ) -> _List["Diagnostic"]:
        if (scope := item.scope) is None:
            raise ValueError(
                f"Item was expected to be scopeable but was not! {item=!r}"
            )

        type_errors = []

        ribs: List[Dict[str, TypeInfo]] = []
        item.scope.ribs = ribs

        # Set the type_id of all function items first.
        for target in filter((lambda item: item.function is not None), scope.items):
            if isinstance(target.ty, TypeInfo):
                type_id = self.insert(target.ty)
            else:
                type_id = target.ty

            if (func := target.function) is not None:
                if isinstance(self[type_id], primitives.Unknown) or not hasattr(func, "type_id"):

                    arguments = compound.Tuple()
                    arguments.inner = [
                        unit.resolve_annotation(
                            scope=scope, ann_node=arg.node.annotation
                        )
                        for arg in func.arguments
                    ]

                    arguments_type = self.insert(arguments)
                    output_type = unit.resolve_annotation(
                        scope=scope, ann_node=func.return_type
                    )

                    func.type_id = self.insert(
                        func_type := compound.Callable(arguments_type, output_type)
                    )
                    target.ty = func_type

                unit.functions[func.name] = func
                continue

            assert False, f"{target=!r}"

        # Procede with the remaining items.
        for target in scope.items:
            node = target.node

            if isinstance(target.ty, TypeInfo) or (isinstance(target.ty, type) and issubclass(target.ty, TypeInfo)):
                type_id = self.insert(target.ty)
            else:
                type_id = target.ty

            assert isinstance(type_id, TypeId), f"{type_id=!r}"

            if (func := target.function) is not None:
                assert isinstance(self[func.type_id], compound.Callable)
                assert isinstance(target.node, ast.FunctionDef)
                type_errors += self.typecheck(target, unit, item_type_id=func.type_id)

            elif isinstance(node, ast.Return):
                assert isinstance(item.node, ast.FunctionDef)
                if (value := node.value) is not None:
                    expr_ty = unit.reveal_type(value, scope)
                else:
                    expr_ty = self.insert(primitives.NoneType())

                # "return f(...)" check `f`.output == `func`.output
                if isinstance(self[expr_ty], compound.Callable):
                    expr_ty = self[expr_ty].output

                if expr_ty != self[item_type_id].output:
                    type_errors.append(
                        TypeCheckError(f"Bad return value for function!")
                    )

            # Re-definition or addition of new name.
            elif isinstance(node, (ast.Assign, ast.AnnAssign)):
                assert isinstance(node, ast.AnnAssign)
                assert isinstance(
                    node.target, ast.Name
                ), f"Can't handle other target forms yet."

                ident = node.target.id

                assert isinstance(ident, str)

                if (ann := node.annotation) is not None:
                    annotation_id = unit.resolve_annotation(scope=scope, ann_node=ann)
                else:
                    annotation_id = self.insert(primitives.Unknown())

                assert type(annotation_id) is TypeId, f"{type(annotation_id)!r}"

                if node.value is not None:
                    value_type_id = unit.reveal_type(node.value, scope)

                if value_type_id != annotation_id:
                    raise TypeCheckError(
                        f"Type mismatch! expected {self.reconstruct(value_type_id)} instead got {self.reconstruct(annotation_id)}"
                    )
                else:
                    annotation = self[annotation_id]
                    last_rib = (ribs and ribs[-1]) or {}
                    ribs.append({**last_rib, ident: annotation})

                if isinstance(self[annotation_id], primitives.StrSlice):
                    assert isinstance(node.value, ast.Constant)
                    assert type(node.value.value) == str
                    st = node.value.value
                    unit.data.insert(value=st, origin=target)

            elif isinstance(self[type_id], primitives.ImportType):
                for alias in target.node.names:
                    module = unit.import_module(
                        decl := ImportDecl(node=alias, parent=target.node)
                    )

                    if module is None:
                        raise ModuleNotFoundError(repr(decl))

                    elif module.name == "__monty":
                        continue

                    unit.modules[module.name] = module
                    assert module.path.exists()

                    if module.path.is_file():
                        file_name = module.path
                    else:
                        assert module.path.is_dir()
                        file_name = module.path / "__init__.py"

                    with open(file_name) as inf:
                        monty.compile(inf, unit=unit, module_name=module.name)

            elif isinstance(target.node, ast.Expr):
                type_errors += self._typecheck_expr(item=target, unit=unit, scope=scope)

            elif isinstance(target.node, ast.Assert):
                print(f"Skipping {ast.dump(target.node)}")

            else:
                raise TypeCheckError(
                    f"Failed typechecking for a scoped item {target=!r} {ast.dump(target.node)=!r}"
                )

        return type_errors