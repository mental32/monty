import ast
import builtins
import textwrap
from dataclasses import dataclass, field, InitVar
from pathlib import Path
from typing import Dict, Any, Optional, Union, List

from monty.language import Scope, Function, ImportDecl, Module, MontyModule, Item
from monty.typing import Primitive, TypeContext, TypeId, Callable, Pointer
from monty.mir import ExternalFunction


@dataclass
class DataContext:
    _mapping: Dict[int, Any] = field(default_factory=dict)
    _origin: Dict[int, Any] = field(default_factory=dict)

    def __getitem__(self, key):
        return self._mapping[key]

    def insert(self, value, origin) -> int:
        ident = max(self._mapping) if self._mapping else -1
        ident += 1
        self._mapping[ident] = value
        self._origin[ident] = origin
        return ident

    def fetch_by_origin(self, origin) -> Optional[int]:
        for (
            ident,
            target,
        ) in self._origin.items():
            if target == origin:
                return ident
        else:
            return None


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
        self._monty_module = monty_module = MontyModule(name="__monty", path=None, unit=self)
        self.modules["__monty"] = monty_module

    def import_module(self, decl: ImportDecl) -> Optional[Module]:
        """Attempt to import a module from an import declaration into the current unit."""
        if (idx := self.data.fetch_by_origin(origin=decl)) is not None:
            return self.data[idx]

        paths_to_inspect = [Path("."), self._stdlib_path]

        assert decl.qualname

        if isinstance(decl.parent, ast.ImportFrom):
            qualname = [decl.parent.module] + decl.qualname
        else:
            qualname = decl.qualname

        assert qualname
        if qualname[0] == "__monty":
            return self._monty_module

        def search(curdir: Path, expected: str) -> Optional[Path]:
            for path in curdir.iterdir():
                is_py_file = path.is_file() and path.name.endswith(".py")

                # We cant have "." in module names.
                if not is_py_file and "." in path.name:
                    continue

                name = path.name

                if is_py_file:
                    name = path.name[:-3]

                if name == part:
                    return path
            else:
                return None

        module: Optional[Module] = None

        def insert_module(final_path: Path, qualname: List[str]):
            module = Module(name=".".join(qualname), path=final_path)
            self.data.insert(value=module, origin=decl)
            return module

        for target in paths_to_inspect:
            qualname_iter = iter(qualname)
            final_qualname = []
            final_path = None

            # "x.y" <- ("x", "y")
            # "./x/y.py"
            while (part := next(qualname_iter, None)) is not None:
                final_path = target = search(target, part)

                if target is None:
                    break

                elif final_path.is_dir() and (final_path / "__init__.py").exists():
                    # special case?
                    # "./x/y/__init__.py"
                    #
                    # from x.y import z
                    #
                    # or
                    #
                    # from x import y
                    return insert_module(final_path, qualname=final_qualname + [part])

                else:
                    final_qualname.append(part)

        return insert_module(final_path, qualname=qualname) if final_path is not None else None

    def disassemble(self) -> str:
        st = ""

        INDENT = " " * 2

        st += "data:\n"
        for n, value in self.data._mapping.items():
            st += f"{INDENT}s{n} = {value!r}\n"
        else:
            st += "\n"

        for name, module in self.modules.items():
            if module.builder is None:
                st += f"{name!s}: <empty>\n"
                continue

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
                    if isinstance(obj, Item) and obj.function is not None:
                        obj = repr(obj.function)
                    else:
                        obj = repr(obj)

                    st += f"{INDENT * 2}r{n} = {obj}\n"

                for block_id, block in ebb.blocks.items():
                    st += f"{INDENT * 2}b{block_id}:"

                    for instr in block.body:
                        st += f"\n{INDENT * 3}{instr!s};"

                st += f"\n\n{INDENT}"

            st += "\n\n"

        return st.strip()

    def resolve_into_function(self, obj: ast.Call, scope: Scope) -> Optional[Union[Function, ExternalFunction]]:
        assert isinstance(obj, ast.Call), f"{obj=!r}"

        if isinstance(obj.func, ast.Name):
            results = scope.lookup(obj.func.id)
            return results[0] if results is not None else None

        elif isinstance(obj.func, ast.Attribute):
            def search(needle: ast.Attribute, haystack: Scope) -> Optional[Item]:
                assert isinstance(needle, ast.Attribute), ast.dump(needle)

                lhs: ast.AST = needle.value
                rhs: str = needle.attr

                # a.b.c
                # Attribute(Attribute(value=Name(id="a"), attr="b"), attr="c")

                if isinstance(lhs, ast.Attribute):
                    left = search(lhs, haystack)

                elif isinstance(lhs, ast.Name):
                    left, *_ = haystack.lookup(lhs.id)

                return left.getattr(rhs)

            item = search(obj.func, scope)

            if isinstance(item, ExternalFunction):
                return item
            else:
                assert isinstance(item, Item)
                assert item.function is not None
                return item.function

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
            func_type_id = self.reveal_type(node.func, scope)
            func_type = self.tcx[func_type_id]
            assert isinstance(func_type, Callable), self.tcx.reconstruct(func_type_id)
            return func_type.output

        elif isinstance(
            node, ast.Compare
        ):  # All comparisions produce a boolean value anyway.
            return self.tcx.get_id_or_insert(Primitive.Bool)

        elif isinstance(node, ast.Constant):
            return self.resolve_annotation(scope=Scope(node, unit=self), ann_node=node)

        elif isinstance(node, ast.Attribute):
            def inspect(node: ast.Attribute) -> Item:
                assert isinstance(node, ast.Attribute)

                lhs: ast.AST = node.value
                rhs: str = node.attr

                # a.b.c
                # Attribute(Attribute(value=Name(id="a"), attr="b"), attr="c")

                if isinstance(lhs, ast.Attribute):
                    left = inspect(lhs)

                elif isinstance(lhs, ast.Name):
                    left, *_ = scope.lookup(lhs.id)

                return left.getattr(rhs)

            item = inspect(node)

            if not isinstance(item, Item) and hasattr(item, "into_item"):
                item = item.into_item(self)

            assert isinstance(item, Item)

            if isinstance(item.ty, Primitive):
                return self.tcx.get_id_or_insert(item.ty)
            else:
                return item.ty

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

            results = scope.lookup(target)

            if results is not None:
                item, *_ = results
                assert isinstance(item, Item)
                return self.tcx.get_id_or_insert(item.ty)

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

                if (ty := type(value)) is str:
                    value_ty = self.tcx.get_id_or_insert(Primitive.StrSlice)
                    wrapper_ty = self.tcx.get_id_or_insert(Pointer(ty=value_ty))
                else:
                    kind = Primitive.from_builtin_type(ty) or Primitive.Unknown
                    wrapper_ty = self.tcx.get_id_or_insert(kind)

                return wrapper_ty

            elif isinstance(tree, ast.Name) and (
                builtin_type := getattr(builtins, tree.id)
            ):
                assert isinstance(tree.ctx, ast.Load)

                if builtin_type is str:
                    value_ty = self.tcx.get_id_or_insert(Primitive.StrSlice)
                    ty = Pointer(ty=value_ty)

                elif (ty := Primitive.from_builtin_type(builtin_type)) is None:
                    raise Exception("Unsupported builtin type!")

                return self.tcx.get_id_or_insert(ty)

            else:
                return None

        return check_builtins() or self.tcx.get_id_or_insert(Primitive.Unknown)
