import ast
from dataclasses import dataclass, field
from functools import cached_property
from pathlib import Path
from typing import (
    Callable,
    Iterator,
    List,
    Optional,
    Sequence,
    Tuple,
    Type,
    TypeVar,
    Union,
)

from .comptime import comptime
from .context import Context
from .item import ImportDecl, Item, Module, SpanInfo
from .mir import Ebb, lower_into_mir
from .ty import (
    PRIMITIVE_TYPES,
    BoolType,
    FloatType,
    Function,
    IntegerType,
    KlassType,
    NoneType,
    StringType, TypeRef,
)
from .ty.tychk import infer, typecheck
from .utils import NULL_SCOPE, STDLIB_PATH, ASTNode, collapse_attribute


@dataclass(init=False)
class BuiltinModule(Module):
    """A compiler builtin module."""

    def __init__(self, driver: "MontyDriver", *, name: str):
        root = Item()
        root.span = SpanInfo(module_name=name)
        super().__init__(name=name, root=root)
        self.driver = driver


@dataclass
class MontyDriver:
    ctx: Context = field(repr=False, default_factory=Context)

    @cached_property
    def __monty(self) -> Module:
        return BuiltinModule(driver=self, name="__monty")

    def __post_init__(self):
        self.ctx.modules["__monty"] = phantom_module = self.__monty
        self.ctx.ast_nodes[phantom_module.root] = phantom_module.root

        def bind_lang_impl(node: Union[ast.ClassDef, ast.FunctionDef]):
            assert isinstance(node, (ast.ClassDef, ast.FunctionDef))
            assert node.decorator_list, breakpoint()  # type: ignore

            top = node.decorator_list.pop()

            assert isinstance(top, ast.Call)
            assert isinstance(top.func, ast.Attribute)
            assert collapse_attribute(top.func) == "__monty.lang"

            assert len(top.args) == 1
            assert isinstance(top.args[0], ast.Constant)

            name = top.args[0].value
            assert isinstance(name, str)

            kind = {
                "int": IntegerType,
                "bool": BoolType,
                "float": FloatType,
            }[name]

            self.ctx.builtin_types[kind] = node

        bind_lang = Function(
            name="__monty.lang", args=[StringType], ret=NoneType, impl=bind_lang_impl
        )

        self.ctx.functions.add(bind_lang)
        self.ctx.functions.update(PRIMITIVE_TYPES.values())

    def import_module(self, decl: ImportDecl) -> Optional[Module]:
        paths_to_inspect = [Path("."), STDLIB_PATH]

        assert decl.qualname

        if isinstance(decl.parent, ast.ImportFrom):
            if decl.parent.module is not None:
                prefix = decl.parent.module.split(".")
            else:
                prefix = []

            qualname = prefix + decl.qualname
        else:
            qualname = decl.qualname

        assert qualname, f"{decl=!r}"

        fullname = ".".join(qualname)
        if (module := self.ctx.modules.get(fullname, None)) is not None:
            return module

        def search(curdir: Path, expected: str) -> Optional[Path]:
            for path in curdir.iterdir():
                is_py_file = path.is_file() and path.name.endswith(".py")

                # We cant have "." in module names.
                if not is_py_file and "." in path.name:
                    continue

                name = path.name

                if is_py_file:
                    name = path.name[:-3]

                if name == expected:
                    return path
            else:
                return None

        def compile_module(final_path: Path, qualname: List[str]):
            with open(final_path) as inf:
                return self.compile(
                    st=inf.read(), module_name=".".join(qualname), path=final_path
                )

        final_path: Optional[Path] = None
        final_qualname: List[str]

        for target in paths_to_inspect:
            qualname_iter = enumerate(iter(qualname))
            final_qualname = []
            final_path = None

            # "x.y" <- ("x", "y")
            # "./x/y.py"
            while (_ := next(qualname_iter, None)) is not None:
                if final_path is not None and not final_path.is_dir():
                    assert final_path.is_file
                    assert final_qualname
                    return compile_module(final_path, qualname=final_qualname)
                else:
                    (idx, part) = _
                    is_last = idx == (len(qualname) - 1)

                if (final_path := search(target, part)) is None:
                    break

                final_qualname.append(part)

                assert final_path is not None

                target = final_path

                if final_path.is_dir():
                    # special case?
                    # "./x/y/__init__.py"
                    #
                    # from x.y import z
                    #
                    # or
                    #
                    # from x import y

                    contains_init = (final_path / "__init__.py").exists()

                    if contains_init:
                        if is_last:
                            assert is_last
                            return compile_module(
                                final_path / "__init__.py",
                                qualname=final_qualname + [part],
                            )

                        if not final_path.is_dir():
                            return compile_module(final_path, qualname=final_qualname)

                        peek = qualname[idx + 1]

                        if search(final_path, peek) is not None:
                            continue

                    if is_last and not contains_init:
                        assert False, f"Missing __init__ file {final_path!r}"

        return (
            compile_module(final_path, qualname=qualname)
            if final_path is not None
            else None
        )

    def compile(
        self,
        st: str,
        *,
        module_name: str = "__main__",
        path: Optional[Path] = None,
    ) -> Module:
        if module_name != "std.builtins" and "std.builtins" not in self.ctx.modules:
            result = ast.parse(
                "from std.builtins import int, float, bool", mode="single"
            )
            assert isinstance(result, ast.Interactive)
            imp = result.body[0]
            assert isinstance(imp, ast.ImportFrom)

            for alias in imp.names:
                decl = ImportDecl(node=alias, parent=imp)
                module = self.import_module(decl)
                assert module is not None
                self.ctx.modules["std.builtins"] = module
                del alias, decl, module

            del result, imp

        root_tree = comptime(source=st, module_name=module_name, ctx=self.ctx)

        (root_entry, local_nodes) = Item.from_module(
            root=root_tree,
            module_name=module_name,
            source_ref=st,
        )

        module_obj = Module(name=module_name, root=root_entry, path=path)
        module_obj.local_nodes = local_nodes

        self.ctx.modules[module_name] = module_obj
        self.ctx.ast_nodes[root_tree] = root_entry

        self.ctx.ast_nodes.update(local_nodes)

        if (newly_defined := set(self.ctx.autoimpls) & set(self.ctx.builtin_types)) :
            for primitive in newly_defined:
                prototype = self.ctx.autoimpls.pop(primitive)

                impl_node = self.ctx.builtin_types[primitive]
                impl_item = self.ctx.ast_nodes[impl_node]
                impl_qualname = self.ctx.get_qualname(impl_item)

                for func in prototype:
                    proto = Function(
                        name=f"{impl_qualname}.{func.name}",
                        args=func.args[:],
                        ret=func.ret,
                    )

                    self.ctx.functions.add(proto)
            else:
                del newly_defined

        for (node, entry) in local_nodes.items():
            if not isinstance(node, (ast.Import, ast.ImportFrom)):
                continue

            for alias in node.names:
                decl = ImportDecl(node=alias, parent=node)

                if self.import_module(decl) is None:
                    span = entry.span.fmt(
                        f"Failed to import {decl.realname!r}",
                    )

                    raise ImportError(span)

        docstring = "<missing>"

        for (node, item) in local_nodes.items():
            assert item.scope is not NULL_SCOPE, "item scope was left unassigned!"

            if isinstance(node, (ast.FunctionDef, ast.ClassDef)):
                assert not node.decorator_list, f"decorators are not supported"

                if (docstring := ast.get_docstring(node)) is not None:
                    node.body.pop(0)

            if (inferred := infer(item, self.ctx)) is None:
                assert False, f"Failed to infer type for {ast.dump(item.node)=!r}"

            if isinstance(inferred, Function):
                assert isinstance(node, ast.FunctionDef), node
                item.kind = inferred
                inferred.docstring = docstring

                if isinstance(item.get_direct_parent_node(ctx=self.ctx), ast.ClassDef):
                    func = Function(
                        name=self.ctx.get_qualname(item),
                        args=inferred.args[:],
                        ret=inferred.ret,
                    )
                else:
                    func = inferred

                self.ctx.functions.add(func)

            elif isinstance(inferred, KlassType):
                self.ctx.klass_types.add(inferred)

            item.kind = inferred
            continue

        typecheck(entry=root_entry, ctx=self.ctx)

        # Resolve TypeRef's for funcdef return annotations
        #
        # Technically this should happen for all type-ref instances since at
        # this point all relevant types in the dep tree should be processed.
        for func in self.ctx.functions:
            assert isinstance(func, Function)

            if isinstance(ret := func.ret, TypeRef):
                func.ret = self.ctx.resolve_type(ret)

        return module_obj

    def lower_into_mir(self) -> Iterator[Tuple[Item, Ebb]]:
        """Returns an iterator that yields all 'used' functions."""
        # TODO: Maybe draw a dependency graph and start yielding from the
        # leaves in.

        for (node, item) in self.ctx.ast_nodes.items():
            if not isinstance(node, ast.FunctionDef):
                continue

            assert isinstance(item.kind, Function)
            yield (item, lower_into_mir(item=item, ctx=self.ctx))
