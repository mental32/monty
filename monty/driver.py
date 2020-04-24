import ast
from dataclasses import dataclass, field
from typing import Union, TextIO, List, Dict, Optional, Tuple
from io import IOBase

import monty
from monty.language import Scope, Function, Item
from monty.errors import CompilationException
from monty.diagnostic import Diagnostic, Error
from monty.mir import Ebb
from monty.typechecker import InferenceEngine, Primitive, TypeId

SourceInput = Union[TextIO, str]


@dataclass
class CompilationUnit:
    module_name: str
    items: Dict[ast.AST, Item] = field(default_factory=dict)
    type_ctx: InferenceEngine = field(default_factory=InferenceEngine)

    _functions: Dict[str, Function] = field(default_factory=dict)

    def __post_init__(self):
        type_id = self.type_ctx.insert(Primitive.Unknown)
        assert type_id == 0, f"Failed to slot Primitive.Unknown at type_id 0!"

    def resolve_annotation(self, ann_node: Union[ast.Str, ast.Subscript, ast.Name, ast.Attribute]) -> TypeId:
        if isinstance(ann_node, ast.Str):
            tree = ast.parse(ann_node, mode="eval")
            assert isinstance(tree, (ast.Subscript, ast.Name, ast.Attribute, ast.Constant)), ast.dump(tree)
        else:
            tree = ann_node

        print(f"RESOLVE {ast.dump(tree)=!r}")

        return 0

    def get_function(self, name: str) -> Optional[Function]:
        return self._functions.get(name, None)

    def set_function(self, name: str, func: Function):
        self._functions[name] = func


def compile_source(
    source_input: SourceInput, *, module_name: str = "__main__"
) -> CompilationUnit:
    if isinstance(source_input, IOBase):
        root = ast.parse(source_input.read())

    elif isinstance(source_input, str):
        root = ast.parse(source_input)

    else:
        raise TypeError(
            f"Expected `source_input` to be a string or file-like object, instead got {type(source_input)!r}"
        )

    assert isinstance(root, ast.Module), f"Can only process modules!"

    root_item = Item(ty=Primitive.Module, node=root, scope=Scope.from_node(root))

    if issues := root_item.validate():
        raise CompilationException(issues)

    unit = CompilationUnit(module_name)

    if issues := monty.typechecker.typecheck(item=root_item, unit=unit):
        raise CompilationException(issues)

    # TODO: Lowering AST/Items (root_items) into HIR/Items (lowered_root)

    # TODO: Lowering HIR/Items (lowered_root) into MIR/Items (compiled_root)

    return unit
