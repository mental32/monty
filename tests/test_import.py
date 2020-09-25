import monty

SOURCE = """
import a
import b
import c,  d,   e
from f import g
from h import i, j
"""

# def test_parse_import():
#     code = monty.compile(SOURCE)

#     main = code.modules["__main__"]

#     assert set(map(str, main.imports)) == {*"abcdegij"}


def test_parse_import_module_path():
    code = monty.compile("import builtins")

    main = code.modules["__main__"]

    assert set(map(str, main.imports)) == {"builtins"}

    builtins_module_decl = main.imports.pop()

    module_object = code.import_module(builtins_module_decl)

    assert module_object is not None
    assert isinstance(module_object, monty.language.Module)
    assert module_object.name == "builtins"
    assert module_object.path.exists()
    assert module_object.builder is None
