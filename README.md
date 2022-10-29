<h1 align="center">Monty</h1>

<h1 align="center">A compiler for strongly typed Python.</h1>

## Index

- [Index](#index)
- [Brief](#brief)
- [Architecture](#architecture)
- [Related projects](#related-projects)
  - ["prior art"](#prior-art)

## Brief

Monty `(/ˈmɒntɪ/)` is a gradually typed, statically compilable Python.
With some baked in tricks to make it feel as dynamic as regular Python.

## Architecture

This workspace is made up of the following crates:

- `montyc_lexer` - strings to tokens
- `montyc_parser` - tokens to ast types
- `montyc_ast` - ast types
- `montyc_flatcode` - ast to flatcode
- `montyc_interpreter` - flatcode to object graphs
- `montyc_typing` - type inference and checking on object graphs
- `montyc_codegen` - machine code generation from typing information and code
- `montyc_driver` - used to tie all the passes together into one pipeline


## Related projects

### ["prior art"](https://github.com/rust-lang/rfcs/blob/master/text/2333-prior-art.md)

- [Cython](https://github.com/cython/cython)
- [Numba](https://github.com/numba/numba)
- [Nuitka](https://github.com/Nuitka/Nuitka)
- [Peggen](https://github.com/gvanrossum/pegen)
- [MyPy](https://github.com/python/mypy)
- [PyPy](https://foss.heptapod.net/pypy/pypy)
- [RPython](https://foss.heptapod.net/pypy/pypy/-/tree/branch/default/rpython)
- [RustPython](https://github.com/RustPython/RustPython)
- [Pyston](https://github.com/pyston/pyston)
- [Pyjion](https://github.com/tonybaloney/Pyjion)
- [ShedSkin](https://github.com/shedskin/shedskin)
- [IronPython](https://github.com/IronLanguages/ironpython3)
- [CPython]

[cranelift]: https://github.com/bytecodealliance/wasmtime/tree/main/cranelift
[llvm]: https://llvm.org/

[PEP604]: https://www.python.org/dev/peps/pep-0604/

[pyre]: https://pyre-check.org/
[rustdoc]: https://doc.rust-lang.org/rust-by-example/meta/doc.html

[CPython]: https://github.com/python/cpython/

[rpython-instances]: https://rpython.readthedocs.io/en/latest/translation.html#user-defined-classes-and-instances
[type-narrowing]: https://www.python.org/dev/peps/pep-0647/#id3
