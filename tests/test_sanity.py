import pytest

import monty
from monty.errors import CompilationException

SOURCE = """
def main() -> int:
    return 0
"""

BAD_RETURN_VALUE = """
def x() -> int:
    return "foo"
"""

class TestSanity:

    def test_unit_main(self):
        unit = monty.driver.compile_source(SOURCE)

        # assert (func := unit.get_function("__main__.main")) is not None, "No function named \"__main__.main\" was found!"
        # assert func.return_type == "int"
        # assert not func.arguments

    def test_basic_inference_engine(self):
        from monty.typechecker import InferenceEngine, Primitive, Callable, List

        engine = InferenceEngine()

        i = engine.insert(Primitive.Unknown)
        o = engine.insert(Primitive.Number)
        f0 = engine.insert(Callable(i, o))

        i = engine.insert(Primitive.Bool)
        o = engine.insert(Primitive.Unknown)
        f1 = engine.insert(Callable(i, o))

        engine.unify(f0, f1)

        l = engine.insert(List(kind=f1))

        assert engine.reconstruct(l) == "List[Callable[Bool, Number]]"

    def test_bad_return_value(self):
        with pytest.raises(CompilationException):
            monty.driver.compile_source(BAD_RETURN_VALUE)
