<h1 align="center">Monty</h1>

<h1 align="center">A compiler for strongly typed Python.</h1>

## Index

- [Index](#index)
- [Brief](#brief)
  - [How does Monty differ from regular Python or the typing semantics of other type checkers?](#how-does-monty-differ-from-regular-python-or-the-typing-semantics-of-other-type-checkers)
- [Building the compiler](#building-the-compiler)
  - [Crate/Repository Layout](#craterepository-layout)
- [Related projects](#related-projects)
  - ["prior art"](#prior-art)

## Brief

Monty `(/ˈmɒntɪ/)` is a gradually typed, statically compilable Python.
With some baked in tricks to make it feel as dynamic as regular Python.

<!-- ### How does Monty differ from regular Python or the typing semantics of other type checkers?

1. The most notable difference is that code in the global scope gets run at compile time instead of at program startup.

This is done for several reasons however the most important one is generally to make importing a static (at-compile-time)
process rather than a lazy (at-program-startup-time) task.

The attitude monty has towards code is that all the business logic should be tucked away neatly organized behind
classes and functions, anything that is in the module/global scope should only be there to initialize and define
those classes and functions.

The compile-time runtime is bounded so that programs may not hog time or infinitely execute. I/O is also restricted
and sandboxed by default prompting the terminal when code attempts to open files, bind or connect sockets, and read
input.

1. Monty by default will accept (parse, comptime eval) **any** Python code. It may not, however, compile it all.

In the interest of making it easy to gradually port existing Python code so that monty can compile it: the compiler will parse
all modern Python (3.8+) code and it will submit the code through compile-time evaluation. this means that it is completely legal
to have Python code which monty cant compile (e.g. async/await, macros, etc...) alongside code that monty can compile.

It is a compilation error if monty discovers a call into code that it can not compile. All the extra code that isn't compilable is
still parsed, evaluated, and managed internally to make it usable for compile-time evaluation, or third-party analysis.

3. Monty typing takes after pytype, pyright, and pyre.

Monty takes direct inspiration from the three mainstream checkers: [pytype] (google), [pyright] (microsoft), [pyre] (facebook).

Like pytype: monty relies [heavily on inference and is lenient instead of strict](https://github.com/google/pytype#how-is-pytype-different-from-other-type-checkers)

Monty learnt about narrowing and guards from pyright [and supports many of the same guard and narrowing patterns](https://github.com/microsoft/pyright/blob/main/docs/type-concepts.md#type-narrowing)

A lot of existing code in monty is designed to be embeddable and query-able [similar to pyre](https://pyre-check.org/docs/querying-pyre/). -->

## Building the compiler

You will need a fairly recent version of rustc, I am building locally with 1.57.
After that it's as simple as running: `cargo run --bin montyc -- --help`

### Crate/Repository Layout

* `/montyc` is the compiler binary, it is a thin wrapper around `montyc_driver`
* `/montyc_driver` is where all the magic happens, type checking/inference, calls into codegen, etc...
* `/montyc_codegen` is where codegen providers are, currently only Cranelift is supported but I'd like to support both LLVM and GCC in the future.
* `/montyc_hlirt` is a High Level Interpreter Runtime (HLIRT) and is a minimal but geniune Python interpreter used mainly for compile time evaluation.
* `/montyc_query` is where the query interface is defined for the driver.
* `/montyc_flatcode` is where AST -> FlatCode lowering happens.
* `/montyc_parser` is the parser implementation.
* `/montyc_core` is where all fundamental types used in this project go to live.

<!-- 
## What Monty can do to feel dynamic.

This section is a work in progress and it documents a few ideas
that I'm exploring to see if I can remove the typical hassle of
working with a strongly-typed, compiled language.

### "automatic unionization"

In Monty variables may only have one type per scope.
you may not re-assign a value to a variable with a different type.

```py
def badly_typed():
    this = 1
    this = "foo"
```

You may however have a union of types, which is internally represented like a tagged
union in C or an enum in Rust.

`typing.Union[T, ...]` is the traditional way to annotate a union explicitly but in
Monty you may use the newer literal syntax `T | U` from [PEP604]:

```py
def foo():
    this: int | str = 1
    this = "foo"
```
```py
def bar() -> int | bool:
    if random.randrange(0, 2):
        return 1
    else:
        return False
```
```py
def baz(qux: str | list[str]) -> int | bool:
    ...
```

And it even works with inference:

```py
def foo() -> int:
    return 1

def bar() -> str:
    return "foo"

def baz(control: bool):
    x = foo() if control else bar()
```

Here the type of `x` in `baz` is inferred to be `Union[int, str]` depending on
the value of `control`.

### "Type narrowing"

Type narrowing [is not a new concept][type-narrowing] and its been around for a while in typecheckers.

The idea is, roughly, that you can take a union type and dissasemble it into one of its
variants through a type guard like:

```py
x: int | str | list[str]


if isinstance(x, int):
    # x is now considered an integer in this branch of the if statement
elif isinstance(x, str):
    # x is now considered a string here.
else:
    # exhaustive-ness checks will allow `x` to be treated as a list of strings here.
```

### Staged computation of module-level code (aka "comptime"/"consteval")

The biggest difference between regular Python and Monty is how the module-level
is evaluated.

Python is lazy and everything gets run when its accessed, a
modules scope is still a big block of executable code after all and can be treated
as a function that operates on an implicit module object.

Monty treats a module's global scope as a big pool of constant declarations.
but this doesn't translate well for obvious reasons with already existing code
and semantics. To bridge this gap montyc has within itself a small AST-based
interpreter that is used to execute the code within a modules global scope.

Assuming most global-scope level logic is there to act as a sort of 
"initializing glue routine" then the user can do whatever they like as long as:

  * The execution finishes within a known amount of "ticks" (so that we don't accidentally run off into an infinite loop that never finishes.)

  * The state of the module's global scope is semantically correct (the typechecker will verify the module after comptime execution has finished for a module.)


Of course in a completely dynamic environment we don't have to restrict the user
like we would when compiling the code regularly, so in that case most things that
would be rejected normally are perfectly fine such as: `exec`, `eval`, 
`globals`, `locals`, dynamic class creation, and functions with untyped arguments. -->

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

[cranelift]: https://github.com/bytecodealliance/wasmtime/tree/main/cranelift
[llvm]: https://llvm.org/

[PEP604]: https://www.python.org/dev/peps/pep-0604/

[rpython-instances]: https://rpython.readthedocs.io/en/latest/translation.html#user-defined-classes-and-instances
[type-narrowing]: https://www.python.org/dev/peps/pep-0647/#id3
