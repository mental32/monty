import monty

SOURCE = """
import a.z
import b
import c,  d,   e
from f import g
from h import i, j
"""

def test_parse_import():
    code = monty.compile(SOURCE)

    main = code.modules["__main__"]

    assert set(map(str, main.imports)) ^ {*"abcdegij"}
