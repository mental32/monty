import monty

s = """
def oof(y: int, x: str) -> bool:
    return True

def foo(x: str, y: int) -> bool:
    return False
"""

def test_simple_function_types():
    code = monty.compile(s)

    oof = code.functions["oof"]
    assert code.tcx.reconstruct(oof.type_id) == "Callable[Tuple[Int, Pointer(StrSlice)], Bool]"

    foo = code.functions["foo"]
    assert code.tcx.reconstruct(foo.type_id) == "Callable[Tuple[Pointer(StrSlice), Int], Bool]"
