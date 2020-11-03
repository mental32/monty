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

from .ty import (
    BoolType,
    FloatType,
    Function,
    IntegerType,
    KlassType,
    PRIMITIVE_TYPES,
    StringType,
    NoneType,
)
from .item import Item, Module, SpanInfo, ImportDecl
from .utils import ASTNode, Builtin, NULL_SCOPE, STDLIB_PATH, collapse_attribute
from .context import Context
from .ty.tychk import typecheck, infer
from .mirgen import lower_into_mir, Ebb


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

    def _comptime(self, module: ast.Module, source_ref: str) -> ast.Module:
        """Given a module node, perform obvious comptime behaviour.

        Here "obvious" compile time behaviour mainly concerns itself with
        code found in the module, or global, scope. Since, in monty, the
        module scope is intended for static, const, and comptime behaviour.

        This function will mutate the given root tree in-place as it
        decides which nodes to keep or discard, such as branches which are
        proveably false (this is useful if the user wants to variably
        include code depending on the Python version.)

        Stuff that happens:

         * branches get evaluated and the succeeding block will replace the entire `if` node.
         * `assert` nodes turn into the equivelent of a `static_assert` in cxx.

        """
        assert isinstance(module, ast.Module), f"{ast.dump(module)=!r}"

        T = TypeVar("T")

        def search(l: List[T], p: Callable[[T], bool]) -> Optional[int]:
            for (idx, obj) in enumerate(l):
                if p(obj):
                    return idx
            else:
                return None

        def comptime_lookup(
            node: Union[str, ast.Name], seq: Sequence[ASTNode]
        ) -> Optional[ASTNode]:
            if isinstance(node, ast.Name):
                assert isinstance(node.ctx, ast.Load)
                target = node.id
            else:
                assert isinstance(node, str)
                target = node

            # comptime-name-lookup requires a sequence of AST nodes that
            # that came "before" this one. This sequence forms the raw
            # body of what will be used to resolve the name.
            if __debug__:
                _ = f"No history sequence provided in order to search the name: {target!r}"
                assert seq, _
                assert isinstance(seq, list), repr(seq)
                _ = f"Refusing to comptime evaluate a history with branches {seq!r}"
                assert ast.If not in map(type, seq), _

            ALLOWED_TYPES = (ast.Assign, ast.AnnAssign, ast.FunctionDef, ast.Import)
            for rev in reversed(seq):
                if type(rev) not in ALLOWED_TYPES:
                    continue

                if (
                    (
                        isinstance(rev, ast.Import)
                        and any([alias.name == target for alias in rev.names])
                    )
                    or (
                        isinstance(rev, (ast.FunctionDef, ast.AnnAssign))
                        and hasattr(rev, "name")
                        and rev.name == target  # type: ignore
                    )
                    or (
                        isinstance(asn := rev, ast.Assign)
                        and len(asn.targets) == 1
                        and isinstance(lhs := asn.targets[0], ast.Name)
                        and lhs.id == target
                    )
                ):
                    return rev
            else:
                raise NotImplementedError(f"{list(map(ast.dump, seq))}")

        def comptime_bool(n: ASTNode, seq: Optional[Sequence[ASTNode]] = None) -> bool:
            assert isinstance(n, (ast.Constant, ast.Name)), ast.dump(n)

            if isinstance(n, ast.Constant):
                return bool(n.value)

            if isinstance(n, ast.Name) and isinstance(n.ctx, ast.Load):
                assert seq is not None
                result = comptime_lookup(node=n, seq=seq)

                assert result is not None

                if isinstance(result, ast.FunctionDef):
                    return True

                if isinstance(result, ast.AnnAssign):
                    if result.value is not None:
                        return comptime_bool(result.value, seq=seq)

                    # value-less annotated assignments exist:
                    #     x: int
                    #     x = 1
                    return comptime_bool(n, seq=seq[: seq.index(result)])

                if isinstance(result, ast.Assign):
                    return comptime_bool(result.value, seq=seq)

            assert False, "unreachable"

        def yield_all_of(*, ty: Type[T], seq: List[T]) -> Iterator[Tuple[int, T]]:
            while ty in set(map(type, seq)):
                if (idx := search(seq, (lambda n: isinstance(n, ty)))) is not None:
                    yield (idx, seq[idx])
                else:
                    assert False, f"Wait, that's illegal. {seq!r}"

        def fold_branches(m: ast.Module):
            body = m.body

            for (idx, next_if) in yield_all_of(ty=ast.If, seq=body):
                head = next_if
                while True:
                    assert isinstance(head, ast.If)

                    if comptime_bool(head.test):
                        left = body[:idx]
                        right = body[idx + 1 :] if idx + 1 in range(len(body)) else []

                        body = m.body = left + head.body + right
                        break

                    elif head.orelse:
                        alt = head.orelse

                        if alt and isinstance(top := alt[0], ast.If):
                            head = top
                            continue

                        assert alt is not None

                        left = body[:idx]
                        right = body[idx + 1 :] if idx + 1 in range(len(body)) else []

                        body = m.body = left + alt + right
                        break

                    else:
                        alt = head.orelse
                        if len(alt) == 1:
                            head = alt[0]
                            continue
                        else:
                            del body[idx]
                            break

                break

        def eval_assert(m: ast.Module):
            for (idx, asrt) in yield_all_of(ty=ast.Assert, seq=m.body):
                assert isinstance(asrt, ast.Assert)

                if not comptime_bool(asrt.test, seq=m.body[:idx]):
                    assert False, Item.from_node(
                        node=asrt,
                        module_name="...",
                        source_ref=source_ref,
                    ).span.fmt("Failed static assert.")

                del m.body[idx]

        def invoke_decorators(m: ast.Module):
            nonlocal self, comptime_lookup
            for node in m.body:
                if (
                    isinstance(node, (ast.ClassDef, ast.FunctionDef))
                    and node.decorator_list
                ):
                    assert len(node.decorator_list) == 1, "only one decorator allowed."

                    [top] = node.decorator_list

                    assert isinstance(top, ast.Call)
                    assert isinstance(top.func, ast.Attribute)

                    f_name = collapse_attribute(top.func)

                    assert f_name.startswith("__monty.")

                    for kind in {_ for _ in self.ctx.functions if _.name == f_name}:
                        assert kind.impl is not None
                        kind.impl(node)
                        break
                    else:
                        assert (
                            False
                        ), f"No implementation for function {f_name=!r} found!"

        # TODO:
        #   Need to think of a safer way to confirm that no modifications to
        #   the tree has happened in a single comptime pass loop, currently we
        #   just "hash" the tree with `ast.dump` and compare the previous and
        #   latest "digests"

        def apply_exhaustively(module: ast.Module, func: Callable[[ast.Module], None]):
            """Apply a transforming function to the supplied module until there is no difference."""
            previous_digest = ""
            while True:
                current_digest = ast.dump(module)
                if current_digest == previous_digest:
                    break

                func(module)

                previous_digest = ast.dump(module)

            return module

        apply_exhaustively(module, fold_branches)
        apply_exhaustively(module, eval_assert)
        apply_exhaustively(module, invoke_decorators)

        return module

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
        if module_name != "std.builtins":
            st = f"from std.builtins import int\n{st}"

        root_tree = self._comptime(ast.parse(st), source_ref=st)

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
