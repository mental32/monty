<h1 align="center">Monty</h1>

<h1 align="center">The elegant Python compiler</h1>

> **WARNING! USE AT YOUR OWN PERIL**

## Index

 - [Index](#Index)
 - [Brief](#Brief)
 - [Examples](#Examples)
 - [Related projects](#Related-projects)


## Brief

Monty (/ˈmɒntɪ/) is an attempt to provide a completely organic dialect of
Python that is capable of being AOT compiled in the same manner as C,
Haskell or Rust are.

At a high level monty can be closely compared with what TypeScript does for the
JavaScript community. The core contrast between Monty and TypeScript however is
that TS is a strict syntactical superset of JS, Monty is a strict syntactical
subset of Python. Meaning that TS adds backwards incompatible syntax to JS
where Monty disallows existing Python syntax in a backwards compatible manner.

## Examples

```py
import monty

FIBONACCI = """
def fib(n: int) -> int:
    a = 0
    b = 1

    for _ in range(n):
        a, b, = b, b + a

    return b

def main():
    print(fib(10))
"""

compilation_unit = monty.driver.compile_source(FIBONACCI)

from monty.ext import miri

miri.exec(compilation_unit)  # "89"
```

## Related projects

> In other words ["prior art"](https://github.com/rust-lang/rfcs/blob/master/text/2333-prior-art.md)

 - [Cython](https://github.com/cython/cython)
 - [Numba](https://github.com/numba/numba)
 - [Nuitka](https://github.com/Nuitka/Nuitka)
 - [Peggen](https://github.com/gvanrossum/pegen)
 - [MyPy](https://github.com/python/mypy)
 - [PyPy](https://foss.heptapod.net/pypy/pypy)

<hr>

[![Join the chat at https://gitter.im/mental32-monty/community](https://badges.gitter.im/mental32-monty/community.svg)](https://gitter.im/mental32-monty/community?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
