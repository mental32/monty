<h1 align="center">Monty</h1>

<h1 align="center">A Strongly Typed Python Dialect</h1>

## Index

- [Index](#index)
- [Brief](#brief)
- [Examples](#examples)
- [Related projects](#related-projects)
  - ["prior art"](#prior-art)

## Brief

Monty `(/ˈmɒntɪ/)` is an attempt to provide a completely organic dialect of
Python equipped with a stronger, safer, and smarter type system.

At a high level monty can be closely compared with what TypeScript does for the
JavaScript community. The core contrast between Monty and TypeScript however is
that TS is a strict syntactical superset of JS, Monty is a strict syntactical
subset of Python. Meaning that TS adds backwards incompatible syntax to JS
where Monty disallows existing Python syntax in a backwards compatible manner.

## Examples

```py
import monty

HELLO_WORLD = """
from builtins import print

def main():
    print("Hello, World!")
"""

code = monty.compile(HELLO_WORLD)

print(code.disassemble())
```

## Related projects

### ["prior art"](https://github.com/rust-lang/rfcs/blob/master/text/2333-prior-art.md)

- [Cython](https://github.com/cython/cython)
- [Numba](https://github.com/numba/numba)
- [Nuitka](https://github.com/Nuitka/Nuitka)
- [Peggen](https://github.com/gvanrossum/pegen)
- [MyPy](https://github.com/python/mypy)
- [PyPy](https://foss.heptapod.net/pypy/pypy)

<hr>

[![Join the chat at https://gitter.im/mental32-monty/community](https://badges.gitter.im/mental32-monty/community.svg)](https://gitter.im/mental32-monty/community?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
