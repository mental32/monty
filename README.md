# Monty

[![Join the chat at https://gitter.im/mental32-monty/community](https://badges.gitter.im/mental32-monty/community.svg)](https://gitter.im/mental32-monty/community?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

## The elegant Python compiler

## Index

 - [Index](#Index)
 - [Brief](#Brief)
 - [Motivation](#Motivation)
 - [Approach](#Approach)
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

## Motivation

> *Warning:* This section is mostly me ranting and raving about why **I** think
> this is needed, it doesn't actually (currently) address real world scenarios
> and motivations about why tools like monty need to exist.
>
> **you have been warned!! :D**

### Preface

Python is a programming language. A tool just like any other of its kind.
Ultimately meant to assist developers in expressing some intent to the machine
in charge of executing the resultant behaviour.

Languages are tools that developers the same way a carpetor has a set of tools.
Tools are designed to suit very specific sets of problems and languages are
also held in this regard. You wouldn't use sledge hammer to assemble a cabinet
where a pin hammer could do just fine the same way you wouldn't use Ruby for
low level systems programming such as writing drivers or linux kernel modules.

Python is now a very mature language, concieved in December of 1989 now making
it 30 years old! its old enough to be the dad of two annoying teen aged kids
regularly barbecuing and producing god awful and cringe worthy dad jokes.

Python had its roots in scripting like many great languages. It focused on
getting the developer home in time for dinner instead of wrestling with them
in an attempt to figure out nasty memory bugs or establish the type of every
identifier ahead of time, which for some languages resulted in a lot of error
prone `search and replace`'ing and angry phone calls during the weekend when
a server spectacularly fails due to some obscure error.

In todays world Python is still a fantastic scripting language! (so great in
fact I've ditched all shell scripts for single file Python alternatives and
[I suggest you do too!](https://www.youtube.com/watch?v=6OY1xFYJVxQ)) but
Python gets used for sooo much more: Data Science, Data Visualization,
Machine Learning, Artificial intelligence, GUIs and DevOps like infrastructure!

### The issue (or "My" issue)

The most common criticism/complaint that I've heard from other users apart from
syntax based ones (e.g. indentation is weird) is that Python is just too slow.
And that is a fair argument to an extent, Python is just a language it has no
concept of speed or performance; only semantics and syntax.

The leading reference implementation of Python is the infamous CPython
interpreter and yes *that* implementation is not as fast as it should be I'll
admit but there are plenty of tricks to "just get the code faster".

> All of this goes without saying that before you should have to resort to easy
> performance tricks like the ones I mention below, the implementations
> algorithmic efficiency should've been assessed first tackling the root of the
> issue there.

Using PyPy (Like CPython interpreter but no C and 100% Python features a JIT!)
or making/using C bindings but if that gets tiresome Cython always exists and
is awesome to use or even by using opt-in like JITters like the well known Numba
library.

These are all perfectly acceptable and widely used solutions, but they all come
at the cost of extra infrastructure or incompatibility with the latest
Python 3.x features. You're limited to CPython syntax 3.6 with PyPy and have to
write extra logic into a build script to deal with C extensions or Cython
codegen, even with Numba you've got extra docs to read.

> *"There must be a better way!"* ~ Raymond Hettinger

Enjoying the performance boost out of the box with 0 extra tooling overhead
(and thus 0 extra non-language documentation to read) allowing Python users
to enjoy the speed of JS on a hot v8 instance or Rust with all the advanced
dark magic tricks used would be brilliant and something I definitely want to
see! therefore I will attempt to make it a reality :D

## Approach

Currently (since this is such a huge project) I'm using step-by-step
development tactic, based on Abdulaziz Ghuloum's
[paper](https://github.com/namin/inc/blob/master/docs/paper.pdf?raw=true)
(*An Incremental Approach to Compiler Construction*.)

In terms of the architecture, the compiler is divided into at least two pieces:

 - Frontend (This repo!)
 - Backend ([montyc](https://github.com/mental32/montyc))

I specified "at least" because the design of the architecture holds a core
focus on increasing API surface wherever possible; hacking, hooking into and
or extending the capabilities of this dialect of Python is very much
encouraged!

### The frontend

The frontend, or in other words this repository, is written in Python intended
to be run on the latest released version of CPython (`3.8>=`) and its designed
and distributed as bog standard a zero dependency typical Python package that
concerns itself purely with the translating and dealing with the language
dialect semantics.

The frontend is divided into four main phases:

 1) CPython `ast.parse`
 2) Monty typed-ast transform
 3) Complex/Heavy semantic checking
 4) typed-ast to lower level SSA form IR (MIR - Monty IR) 

### The backend(s)

The backend element is where Monty likes to show off a little, The dialect is
only ever enforced by the frontend; this includes the various semantics and
nitty gritty implementation details of the more magical aspects of typical
Python.

The dialect is ultimately a guided style of Python. It's no more real than type
annotations without a static type checker to use them and no more enforced than
PEP8 without a linter/formatter to constantly correct the source style.

The style only becomes enforced once the frontend gets used and the frontend
holds a very particular law that it must always be as backend agnostic as
possible. As long as this rule holds true a tremendous amount of freedom and
flexibility is then gained automatically, if users want to use LLVM or
Cranelift or compile to WASM or even use a runtime interpreter to run MIR
straight off the bat, they should be able to do so!

Currently the reference backend is [montyc](https://github.com/mental32/montyc)
and it's a MIR to machine code compiler that uses the
[Cranelift](https://github.com/bytecodealliance/wasmtime/tree/master/cranelift)
code generator backend and its written in Rust!

## Examples

### Bottles on the wall
#### *NOTE: WIP, this example currently will not compile.*

Lets take the classic [bottles of beer on the wall](https://github.com/python/cpython/blob/master/Tools/demo/beer.py).
Save the following code under `bottles.py`:

```py
"""Bottle of beer on the wall, a recreation of the classic by Guido."""
from sys import argv, exit
from typing import Tuple

if len(argv) >= 2:
    upper = int(argv[1])
else:
    upper = 100

if upper <= 0:
    exit("The number of bottles must be a positive integer!")

def to_message(n: int) -> Tuple[str, str]:
    assert n >= 1
    return (f"{n} bottles of beer", f"{n - 1} bottles of beer")

others = map(to_message, range(upper, 0, -2))
final = [("One bottle of beer", "No more bottles of beer")]

for (currently, following) in chain(others, final):
    print(f"{currently} on the wall,")
    print(f"{currently}.")
    print("Take one down, pass it around,")
    print(f"{following} on the wall.")
```

Compiling it is as simple as:

 - `monty bottle.py`

Now an executable file will be emitted with the same name `bottle`.

 - `./bottle`

## Related projects

> In other words ["prior art"](https://github.com/rust-lang/rfcs/blob/master/text/2333-prior-art.md)

 - [Cython](https://github.com/cython/cython)
 - [Numba](https://github.com/numba/numba)
 - [Nuitka](https://github.com/Nuitka/Nuitka)
 - [Peggen](https://github.com/gvanrossum/pegen)
 - [MyPy](https://github.com/python/mypy)
 - [PyPy](https://foss.heptapod.net/pypy/pypy)
