import ast
from dataclasses import dataclass
from typing import (
    Union,
    Callable,
    Dict,
    Iterable,
    Iterator,
    List,
    Optional,
    Sequence,
    Set,
    Tuple,
    Type,
    TypeVar,
)

from .item import Item
from .utils import ASTNode, collapse_attribute
from .ty import Function

T = TypeVar("T")


def _index_with(it: Iterable[T], pred: Callable[[T], bool]) -> Optional[int]:
    for (idx, obj) in enumerate(it):
        if pred(obj):
            return idx
    else:
        return None


def _yield_all_of(*, ty: Type[T], seq: List[T]) -> Iterator[Tuple[int, T]]:
    while ty in set(map(type, seq)):
        if (idx := _index_with(seq, (lambda n: isinstance(n, ty)))) is not None:
            yield (idx, seq[idx])
        else:
            assert False, f"Wait, that's illegal. {seq!r}"


@dataclass
class CompileTimeTransformer:
    module: ast.Module
    item: Item

    def comptime_lookup(
        self,
        node: Union[str, ast.Name],
        seq: Sequence[ASTNode],
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

    def comptime_bool(
        self,
        n: ASTNode,
        seq: Optional[Sequence[ASTNode]] = None,
    ) -> bool:
        assert isinstance(n, (ast.Constant, ast.Name)), ast.dump(n)

        if isinstance(n, ast.Constant):
            return bool(n.value)

        if isinstance(n, ast.Name) and isinstance(n.ctx, ast.Load):
            assert seq is not None
            result = self.comptime_lookup(node=n, seq=seq)

            assert result is not None

            if isinstance(result, ast.FunctionDef):
                return True

            if isinstance(result, ast.AnnAssign):
                if result.value is not None:
                    return self.comptime_bool(result.value, seq=seq)

                # value-less annotated assignments exist:
                #     x: int
                #     x = 1
                return self.comptime_bool(n, seq=seq[: seq.index(result)])

            if isinstance(result, ast.Assign):
                return self.comptime_bool(result.value, seq=seq)

        assert False, "unreachable"

    # TODO:
    #   Need to think of a safer way to confirm that no modifications to
    #   the tree has happened in a single comptime pass loop, currently we
    #   just "hash" the tree with `ast.dump` and compare the previous and
    #   latest "digests"

    def apply_exhaustively(self, module: ast.Module, func: Callable[[ast.Module], None]):
        """Apply a transforming function to the supplied module until there is no difference."""
        previous_digest = ""
        while True:
            current_digest = ast.dump(module)
            if current_digest == previous_digest:
                break

            func(module)

            previous_digest = ast.dump(module)

        return module

def comptime(source: str, module_name: str, *, ctx) -> ast.Module:
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
    module = ast.parse(source)

    (item, _) = Item.from_module(
        root=module,
        module_name=module_name,
        source_ref=source,
    )

    transformer = CompileTimeTransformer(module=module, item=item)

    def fold_branches(m: ast.Module):
        body = m.body

        for (idx, next_if) in _yield_all_of(ty=ast.If, seq=body):
            head = next_if
            while True:
                assert isinstance(head, ast.If)

                if transformer.comptime_bool(head.test):
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
        for (idx, asrt) in _yield_all_of(ty=ast.Assert, seq=m.body):
            assert isinstance(asrt, ast.Assert)

            if not transformer.comptime_bool(asrt.test, seq=m.body[:idx]):
                assert False, Item.from_node(
                    node=asrt,
                    module_name="...",
                    source_ref=source,
                ).span.fmt("Failed static assert.")

            del m.body[idx]

    def invoke_decorators(m: ast.Module):
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

                for kind in {_ for _ in ctx.functions if _.name == f_name}:
                    assert kind.impl is not None
                    kind.impl(node)
                    break
                else:
                    assert False, f"No implementation for function {f_name=!r} found!"

    transformer.apply_exhaustively(module, fold_branches)
    transformer.apply_exhaustively(module, eval_assert)
    transformer.apply_exhaustively(module, invoke_decorators)

    return module
