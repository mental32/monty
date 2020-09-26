import ast
import subprocess
from typing import Union, TextIO, List, Dict, Optional, Tuple, Any
from io import IOBase
from pathlib import Path

import monty
from monty.language import Item, Module
from monty.errors import CompilationException

from monty.mir import Ebb, Module as ModuleBuilder
from monty.typing import Primitive
from monty.unit import CompilationUnit

__all__ = ("compile",)

_STDLIB_PATH = (Path(".").parent / "stdlib").resolve().absolute()


def compile(
    source: Union[TextIO, str],
    module_name: str = "__main__",
    unit: Optional[CompilationUnit] = None,
    arch: Optional[str] = None,
    path_to_stdlib: Optional[Path] = None,
):
    """Attempt to compile some `source` code into a compiled module.

    The steps should be taken as following:

    * `.read` out the string from the file-like object (if its a file)
    otherwise use the provided string.
    * `ast.parse` the source input.
    * recursively validate the root `ast.Module` item.
    * recursively typecheck the root item.
    * lower the module into MIR.

    Args:
        source (Union[TextIO, str]): the source input to compile.

            May be either a string or a file-like object
            (providing that it inherits from `IOBase`)

        module_name (str): the name of the initial module default: `__main__`

        unit (Optional[CompilationUnit]): compile and typecheck the source with
            the provided unit, otherwise just make a new one and use that.

            Note that this should not typically be supplied by the user, it is
            an argument that the library can use to recursively compile modules.

        arch: (Optional[str]): The target architecture to compile for.

            Defaults to the native architecture this code is running on.

    Returns:
        The compiled module(s) as a `CompilationUnit`.
    """
    source_input = source.read() if isinstance(source, IOBase) else source

    if not isinstance(source_input, str):
        ty = type(source_input)
        raise TypeError(
            f"Expected `source` to be a string or file-like object, instead got {ty!r}"
        )

    if arch is not None:
        raise NotImplementedError("Support for custom targets is not implemented yet.")
    else:
        arch = subprocess.check_output("gcc -dumpmachine", shell=True).decode("utf-8").strip()

    path_to_stdlib = path_to_stdlib or _STDLIB_PATH

    assert isinstance(path_to_stdlib, Path), type(path_to_stdlib)
    assert path_to_stdlib.exists(), f"provided path to stdlib does not exist! {path_to_stdlib=!r}"

    root_node = ast.parse(source_input)

    assert isinstance(root_node, ast.Module), "Can only process ast.Modules as roots!"

    if unit is None:
        unit = CompilationUnit(path_to_stdlib=path_to_stdlib)

    root_item = Item(node=root_node, ty=Primitive.Module, unit=unit)

    if issues := root_item.recursively_validate():
        raise CompilationException(issues)

    if issues := monty.typing.typecheck(item=root_item, unit=unit):
        raise CompilationException(issues)

    if module_name not in unit.modules:
        unit.modules[module_name] = module = Module(name=module_name, path=None)
    else:
        module = unit.modules[module_name]

    module.builder = builder = ModuleBuilder(unit=unit, root=root_item)
    builder.output = builder.lower_into_mir()

    return unit
