import ast
import builtins
import textwrap
from dataclasses import dataclass, field, InitVar
from pathlib import Path
from typing import Dict, Any, Optional, Union

from monty.language import Scope, Function, ImportDecl, Module
from monty.typing import Primitive, TypeContext, TypeId


@dataclass
class DataContext:
    _mapping: Dict[int, Any] = field(default_factory=dict)
    _origin: Dict[int, Any] = field(default_factory=dict)

    def insert(self, value, origin) -> int:
        ident = max(self._mapping) if self._mapping else -1
        ident += 1
        self._mapping[ident] = value
        self._origin[ident] = origin
        return ident

    def fetch_by_origin(self, origin) -> int:
        for (
            ident,
            target,
        ) in self._origin.items():
            if target == origin:
                return ident
        else:
            raise KeyError(origin)


@dataclass
class CompilationUnit:
    # 'Path(~/.monty/stdlib)'
    path_to_stdlib: InitVar[Path]

    tcx: TypeContext = field(default_factory=TypeContext)

    # "sys" => "<Module: name='sys'>"
    modules: Dict[str, Module] = field(default_factory=dict)

    # "__main__.main" => "<Function: signature='main() -> None'>"
    functions: Dict[str, "Function"] = field(default_factory=dict, repr=False)

    # 'd0' => "Hello, World!"
    data: DataContext = field(default_factory=DataContext)

    def __post_init__(self, path_to_stdlib: Path):
        assert not self.tcx.mapping

        unknown = self.tcx.insert(Primitive.Unknown)
        assert unknown == 0, f"Failed to slot Primitive.Unknown at type_id 0!"

        self.tcx.insert(Primitive.I64)
        self.tcx.insert(Primitive.I32)
        self.tcx.insert(Primitive.Bool)
        self.tcx.insert(Primitive.None_)

        self._stdlib_path = path_to_stdlib

    def import_module(self, decl: ImportDecl) -> Optional[Module]:
        """Attempt to import a module from an import declaration into the current unit."""
        paths_to_inspect = [Path("."), self._stdlib_path]

        assert decl.qualname

        def search(curdir: Path, expected: str) -> Optional[Path]:
            for path in curdir.iterdir():
                is_py_file = path.is_file() and path.name.endswith(".py")

                # We cant have "." in module names.
                if not is_py_file and "." in path.name:
                    continue

                if is_py_file:
                    name = path.name[:-3]
                else:
                    name = path.name

                if name == part:
                    return path
            else:
                return None

        module: Optional[Module] = None

        for target in paths_to_inspect:
            qualname = iter(decl.qualname)
            final_path = None

            # "x.y" <- ("x", "y")
            # "./x/y.py"
            while (part := next(qualname, None)) is not None:
                final_path = search(target, part)
 
            # "./x/y/__init__.py"
            if (
                final_path is not None
                and final_path.is_dir()
                and not (final_path / "__init__.py").exists()
            ):
                final_path = None

            if final_path is not None:
                module = Module(name=".".join(decl.qualname), path=final_path)

        return module

    def disassemble(self) -> str:
        st = ""

        INDENT = " " * 2

        st += "data:\n"
        for n, value in self.data._mapping.items():
            st += f"{INDENT}s{n} = {value!r}\n"
        else:
            st += "\n"

        for name, module in self.modules.items():
            st += f"{name!s}:\n{INDENT}"

            for (
                name,
                ebb,
            ) in module.builder.output.items():
                ret = self.tcx[ebb.return_value].as_str(self.tcx)
                st += f"func {name!s}({ebb.parameters}) -> {ret}\n"

                for (
                    n,
                    obj,
                ) in ebb.refs.items():
                    st += f"{INDENT * 2}r{n} = {obj}\n"

                for block_id, block in ebb.blocks.items():
                    st += f"{INDENT * 2}b{block_id}:"

                    for instr in block.body:
                        st += f"\n{INDENT * 3}{instr!s};"

            st += "\n\n"

        return st.strip()

    def resolve_into_function(self, obj: ast.AST, scope: Scope) -> Optional[Function]:
        assert isinstance(obj, ast.Call), f"{obj=!r}"

        # FIXME: at the moment `ast.FunctionDef`s only live in the module scope.

        assert isinstance(obj.func, ast.Name), "Cant do attribute access yet..."

        results = scope.lookup(obj.func.id)
        return results[0] if results is not None else None

    def reveal_type(self, node: ast.AST, scope: Scope) -> Optional[TypeId]:
        """Attempt to reveal the [product] type of a AST node."""
        assert isinstance(node, ast.AST), f"{node=!r}"
        assert scope is None or isinstance(scope, Scope), f"{scope=!r}"

        if isinstance(node, ast.BinOp):
            lhs = self.reveal_type(node.left, scope)
            rhs = self.reveal_type(node.right, scope)

            lty = self.tcx[lhs]
            rty = self.tcx[rhs]

            if lty == Primitive.I64 and rty == Primitive.I64:
                return self.tcx.get_id_or_insert(Primitive.I64)
            else:
                raise RuntimeError(f"{lty}, {rty}")

        elif isinstance(node, ast.AnnAssign):
            return self.resolve_annotation(scope, node.annotation)

        elif isinstance(node, ast.Call):
            return self.reveal_type(node.func, scope)

        elif isinstance(
            node, ast.Compare
        ):  # All comparisions produce a boolean value anyway.
            return self.tcx.get_id_or_insert(Primitive.Bool)

        elif isinstance(node, ast.Constant):
            return self.resolve_annotation(Scope(node), node)

        elif isinstance(node, ast.Name):
            assert isinstance(node.ctx, ast.Load), f"{ast.dump(node)}"
            target = node.id

            # FIXME: Multiple instances of the same name in the same function scope are going to be wrong here.
            #
            #     x: int = ...
            #     x: bool = ...
            #
            #    When revealing for the first `x` during AST lowering we'll get the `bool` definition.
            #    we need to mangle every ast.Name node with a ast.Store context...
            for stack in scope.ribs[::-1]:
                if target in stack:
                    return self.tcx.get_id_or_insert(stack[target])

            for item_ in scope.items:
                # print(f">>", item_)
                func = item_.function

                if func is not None and target == func.name:
                    assert hasattr(
                        func, "type_id"
                    ), f"function object did not have a `type_id` {func=!r}"
                    return func.type_id

            # couldn't find the name in the local function scope.
            # search the module's global scope...
            module_scope = scope.module.scope

            if scope != scope.module.scope:
                return self.reveal_type(node, module_scope)

        raise RuntimeError(f"We don't know jack... {ast.dump(node)}")

    def resolve_annotation(
        self,
        scope: Scope,
        ann_node: Union[ast.Str, ast.Subscript, ast.Name, ast.Attribute],
    ) -> TypeId:
        if isinstance(ann_node, ast.Str):
            tree = ast.parse(ann_node, mode="eval")
        else:
            tree = ann_node

        if __debug__:
            allowed = (ast.Subscript, ast.Name, ast.Attribute, ast.Constant)
            assert isinstance(tree, allowed), ast.dump(tree)

        def check_builtins() -> Optional[TypeId]:
            if isinstance(tree, ast.Constant):
                value = tree.value
                assert value is None or isinstance(value, (str, int))

                kind = Primitive.from_builtin_type(type(value)) or Primitive.Unknown

                return self.tcx.get_id_or_insert(kind)

            elif isinstance(tree, ast.Name) and (
                builtin_type := getattr(builtins, tree.id)
            ):
                assert isinstance(tree.ctx, ast.Load)

                if (ty := Primitive.from_builtin_type(builtin_type)) is None:
                    raise Exception("Unsupported builtin type!")

                return self.tcx.get_id_or_insert(ty)

            else:
                return None

        return check_builtins() or self.tcx.get_id_or_insert(Primitive.Unknown)
