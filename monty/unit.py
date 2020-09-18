import ast
import textwrap
from dataclasses import dataclass, field
from typing import Dict, Any, Optional

from monty.language import Scope, Function
from monty.typing import Primitive, TypeContext


@dataclass
class CompilationUnit:
    tcx: TypeContext = field(default_factory=TypeContext)

    # "sys" => "<Module: name='sys'>"
    modules: Dict[str, "ModuleBuilder"] = field(default_factory=dict)

    # "__main__.main" => "<Function: signature='main() -> None'>"
    functions: Dict[str, "Function"] = field(default_factory=dict, repr=False)

    def __post_init__(self):
        assert not self.tcx.mapping

        unknown = self.tcx.insert(Primitive.Unknown)
        assert unknown == 0, f"Failed to slot Primitive.Unknown at type_id 0!"

        self.tcx.insert(Primitive.I64)
        self.tcx.insert(Primitive.I32)
        self.tcx.insert(Primitive.Bool)
        self.tcx.insert(Primitive.None_)

    def disassemble(self) -> str:
        st = ""

        INDENT = " " * 2

        for name, module in self.modules.items():
            st += f"{name!s}:\n{INDENT}"

            for name, ebb, in module.output.items():
                ret = self.tcx[ebb.return_value].reconstruct(self.tcx)
                st += f"func {name!s}({ebb.parameters}) -> {ret}\n"

                for n, obj, in ebb.refs.items():
                    st += f"{INDENT * 2}r{n} = {obj}\n"

                for block_id, block in ebb.blocks.items():
                    st += f"{INDENT * 2}b{block_id}:"

                    for instr in block.body:
                        st += f"\n{INDENT * 3}{instr!s};"

                st += F"\n\n"

        return st.strip()

    def resolve_into_function(self, obj: Any, scope: Scope) -> Optional[Function]:
        assert isinstance(obj, ast.Call), f"{obj=!r}"

        # FIXME: at the moment `ast.FunctionDef`s only live in the module scope.

        assert isinstance(obj.func, ast.Name), "Cant do attribute access yet..."

        target = obj.func.id

        for item in scope.items:
            func = item.function

            if func is not None and target == func.name:
                assert hasattr(
                    func, "type_id"
                ), f"function object did not have a `type_id` {func=!r}"
                return func
        else:
            return None


# def reveal_type(self, node: ast.AST, scope: Scope) -> Optional[TypeId]:
#     """Attempt to reveal the [product] type of a AST node."""
#     assert isinstance(node, ast.AST)
#     assert isinstance(scope, Scope), f"{scope=!r}"

#     print(f"{ast.dump(node)=!r}")

#     if isinstance(node, ast.BinOp):
#         op = node.op

#         if isinstance(op, ast.Add):
#             op = constraints.Operation.Add
#         elif isinstance(op, ast.Sub):
#             op = constraints.Operation.Sub
#         else:
#             assert False

#         lhs = self.reveal_type(node.left, scope)
#         rhs = self.reveal_type(node.right, scope)

#         lty = self.type_ctx[lhs]
#         rty = self.type_ctx[rhs]

#         if lty == Primitive.I64 and rty == Primitive.I64:
#             return self.type_ctx.get_id_or_insert(Primitive.I64)
#         else:
#             raise RuntimeError(f"{lty}, {rty}")

#     elif isinstance(node, ast.Call):
#         return self.reveal_type(node.func, scope)

#     elif isinstance(node, ast.Compare):  # All comparisions produce a boolean value anyway.
#         return self.type_ctx.get_id_or_insert(Primitive.Bool)

#     elif isinstance(node, ast.Constant):
#         return self.resolve_annotation(Scope(node), node)

#     elif isinstance(node, ast.Name):
#         assert isinstance(node.ctx, ast.Load), f"{ast.dump(node)}"
#         target = node.id

#         # FIXME: Multiple instances of the same name in the same function scope are going to be wrong here.
#         #
#         #     x: int = ...
#         #     x: bool = ...
#         #
#         #    When revealing for the first `x` during AST lowering we'll get the `bool` definition.
#         #    we need to mangle every ast.Name node with a ast.Store context...
#         for stack in scope.ribs[::-1]:
#             if target in stack:
#                 return self.type_ctx.get_id_or_insert(stack[target])

#         for item_ in scope.items:
#             # print(f">>", item_)
#             func = item_.function

#             if func is not None and target == func.name:
#                 assert hasattr(func, "type_id"), f"function object did not have a `type_id` {func=!r}"
#                 return func.type_id

#         # couldn't find the name in the local function scope.
#         # search the module's global scope...
#         module_scope = scope.module.scope

#         return self.reveal_type(node, module_scope)

#     raise RuntimeError(f"We don't know jack... {ast.dump(node)}")

# def resolve_annotation(
#     self,
#     scope: Scope,
#     ann_node: Union[ast.Str, ast.Subscript, ast.Name, ast.Attribute],
# ) -> TypeId:
#     if isinstance(ann_node, ast.Str):
#         tree = ast.parse(ann_node, mode="eval")
#         assert isinstance(
#             tree, (ast.Subscript, ast.Name, ast.Attribute, ast.Constant)
#         ), ast.dump(tree)
#     else:
#         tree = ann_node

#     def check_parent_scope(parent_scope: Scope) -> Optional[TypeId]:
#         return None

#     def check_builtins() -> Optional[TypeId]:
#         builtin_map = {
#             int: Primitive.I64,
#             float: Primitive.Number,
#             bool: Primitive.Bool,
#             type(None): Primitive.None_,
#         }

#         if isinstance(tree, ast.Constant):
#             value = tree.value
#             assert value is None or isinstance(value, (str, int))

#             kind = builtin_map.get(type(value), Primitive.Unknown)

#             return self.type_ctx.get_id_or_insert(kind)

#         elif isinstance(tree, ast.Name) and (builtin := getattr(builtins, tree.id)):
#             assert isinstance(tree.ctx, ast.Load)

#             if (ty := builtin_map.get(builtin, None)) is None:
#                 raise Exception("Unsupported builtin type!")

#             return self.type_ctx.get_id_or_insert(ty)

#         else:
#             return None

#     return (
#         check_parent_scope(scope.parent)
#         or check_builtins()
#         or self.type_ctx.get_id_or_insert(Primitive.Unknown)
#     )

# def get_function(self, name: str) -> Optional[Function]:
#     module, name, *_ = name.split(".", maxsplit=1)

#     for item in self.modules[module].walk_function_items():
#         if (func := item.function).name == name:
#             return func
#     else:
#         return None