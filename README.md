<h1 align="center">Monty</h1>

<h1 align="center">A novel compiler for strongly typed Python.</h1>

## Index

- [Index](#index)
- [Brief](#brief)
  - ["Is it really Python?"](#is-it-really-python)
  - [Differences from regular Python](#differences-from-regular-python)
    - [Code in the global scope gets run at compile time instead of at program startup.](#code-in-the-global-scope-gets-run-at-compile-time-instead-of-at-program-startup)
    - [Monty by default will accept (parse, eval) any Python code. It may not, however, compile it all.](#monty-by-default-will-accept-parse-eval-any-python-code-it-may-not-however-compile-it-all)
  - ["Why is this compiler 'novel'"](#why-is-this-compiler-novel)
- [Related projects](#related-projects)
  - ["prior art"](#prior-art)

## Brief

Monty `(/ˈmɒntɪ/)` is a gradually typed, statically compilable Python.
With some baked in tricks to make it feel as dynamic as regular Python.

### "Is it really Python?"

On a technical level monty is a compiler for a Python _dialect_ but the implementation
strives to remove semantic and any syntactic differences between this particular dialect
and the one presented by [CPython]. It's not, strictly speaking, the same language but
you can pretend as if it is 99% the same.

### Differences from regular Python

#### Code in the global scope gets run at compile time instead of at program startup.

This is done for several reasons however the most important one is generally to make importing a static (at-compile-time) process rather than a lazy (at-program-startup-time) task.

The attitude monty has towards code is that all the business logic should be tucked away neatly organized behind classes and functions, anything that is in the module/global scope should only be there to initialize and define those classes and functions.

The compile-time runtime is bounded so that programs may not hog time or infinitely execute. I/O is also restricted and sandboxed by default prompting the terminal when code attempts to open files, bind or connect sockets, and read input.

#### Monty by default will accept (parse, eval) any Python code. It may not, however, compile it all.

In the interest of making it easy to gradually port existing Python code so that monty can compile it: the compiler will parse all modern Python (3.8+) code and it will submit the code through compile-time evaluation. this means that it is completely legal to have Python code which monty cant compile (e.g. async/await, macros, etc...) alongside code that monty can compile.

It is a compilation error if monty discovers a call into code that it can not compile. All the extra code that isn't compilable is still parsed, evaluated, and managed internally to make it usable for compile-time evaluation, or third-party analysis

### "Why is this compiler 'novel'"

This compiler was designed in a, somewhat, unorthodox way.

Traditional compilers are pass-oriented, operating on some files, directory, or package as input and spitting out binaries as output.
Along the way they will try some things to improve build time performance like: caching artifacts / figuring out how to do incremental compilation.

montyc is a wild mix of things:

1. it is not a simple pass-oriented architecture.
2. it is [query based](https://ollef.github.io/blog/posts/query-based-compilers.html) to place emphasis on incremental compilation first.
3. it is also a [nanopass](https://dl.acm.org/doi/10.1145/1016848.1016878) compiler, focusing on transforming the intermediate representation in steps instead of running large or small passes onto a global state object.
4. it is designed to run as a service, you:
   1. spin it up
   2. point it at some code, perhaps not even to compile but simply type check!
   3. access session artifacts, this is every class definition, declaration, every function call, access to static analysis of variables live
5. it is also a capable interpreter, not a fast one however since this isn't what it's primary intended purpose is.
6. it is a language server provider for whatever your favorite editor is
7. it can automatically generate documentation akin to [rustdoc]! ([example](https://docs.rs/tokio/latest/tokio/))

it does not include all these features with the intention of replacing already
existing tools that do this, but instead includes these things to make it as easy
as possible to say yes to when deciding what to use.

and most importantly because it is built to run as a service. the primary usage of
compilers-as-analytics is to be used as a source of truth for more specialized static analysis
tools, like how [pyre] lets you perform [taint analysis](https://pyre-check.org/docs/pysa-explore/) for security purposes.


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
