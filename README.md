<h1 align="center">Monty</h1>

<h1 align="center">A language toolchain for explicitly typed annotated Python.</h1>

## Index

- [Index](#index)
- [Abstract](#abstract)
- [Building the compiler](#building-the-compiler)
  - [Crate/Repository Layout](#craterepository-layout)
- [Related projects](#related-projects)
  - ["prior art"](#prior-art)

## Abstract

Monty `(/ˈmɒntɪ/)` is a language toolchain for a subset of Python 3.0+
it is designed from the ground up to be an LSP-first compiler, emphasizing
high responsiveness and extraordinarily fast compile times.

Similar alternatives are already mature and pretty impressive, namely:

- [Codon](https://github.com/exaloop/codon)
- [LPython](https://github.com/lcompilers/lpython)

The compiler aims to maintain the inherent dynamism of Python while
incorporating advanced type inference and type-checking capabilities.

Key Differentiators:

1.  Compile-Time Execution of Global Scope: Monty evaluates code in the global
    scope at compile-time, thereby making the import process static. This
    architectural decision is intended to encourage the segregation of business
    logic from initialization code.

2.  Advanced Typing Semantics: Monty's type system is influenced by prominent
    type checkers such as pytype, pyright, and pyre. It employs advanced type
    inference algorithms and supports a variety of type-narrowing techniques.

Monty is not intended to ever be a production grade language, it is a
project I enjoy working on in my spare time and most recently I have
decided to transition away from total coverage of Python and pick out a,
strict, toy, subset of the language.

## Building the compiler

You will need a fairly recent version of rustc, I am building locally with `1.72.0`

Then specify that you would like to build the `montyc` binary, dropping the
`--release` flag as needed.

```
cargo build --release --bin montyc
```

### Crate/Repository Layout

* `/montyc` is the compiler binary, it is a thin wrapper around `montyc_driver`
* `/montyc_driver` is where all the magic happens, type checking/inference,
    calls into codegen, etc...
* `/montyc_codegen` is where codegen providers are,
    currently only Cranelift is supported but I'd like to support both LLVM and GCC
    in the future.
* `/montyc_hlirt` is a High Level Interpreter Runtime (HLIRT)
    and is a minimal but geniune Python interpreter used mainly for compile time
    evaluation.
* `/montyc_query` is where the query interface is defined for the
    driver.
* `/montyc_flatcode` is where AST -> FlatCode lowering happens.
* `/montyc_parser` is the parser implementation.
* `/montyc_core` is where all
    fundamental types used in this project go to live.

## Related projects

### ["prior art"](#prior-art)

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

[prior-art]: https://github.com/rust-lang/rfcs/blob/master/text/2333-prior-art.md
[cranelift]: https://github.com/bytecodealliance/wasmtime/tree/main/cranelift
[llvm]: https://llvm.org/
[PEP604]: https://www.python.org/dev/peps/pep-0604/
[rpython-instances]: https://rpython.readthedocs.io/en/latest/translation.html#user-defined-classes-and-instances
[type-narrowing]: https://www.python.org/dev/peps/pep-0647/#id3
