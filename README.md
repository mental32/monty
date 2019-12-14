# Monty
## The elegant Python compiler

## Index

 - [Index](#Index)
 - [Brief](#Brief)
 - [Examples](#Examples)

## Brief

Monty is a Python compiler supporting the cranelift and LLVM backends.

### The pitch

 1) Take the Python language as it is today (Py3 not Py2)
 2) Enforce static typing via type annotations.
 3) Move or remove functions from the stdlib that would otherwise make compiling a bitch.
   3a) Remove functions that would be otherwise impossible to compile.
   3b) Mark or move functions whose return value or behaviour could be computed at compile time.   
 4) Implement a lightweight runtime that handles memory management and exception tracebacks.

## Examples

### Basic demo

Lets take the classic [bottles of beer on the wall](https://github.com/python/cpython/blob/master/Tools/demo/beer.py).
Save the following code under `bottles.py`:

```py
"""Bottle of beer on the wall, a recreation of the classic by Guido."""
from sys import argv

if len(argv) >= 2:
    upper = int(argv[1])
else:
    upper = 100

bottles = {
    0: "No more bottles of beer",
    1: "One bottle of beer",
}

for i in range(upper, 0, -1):
    bottle: str = bottles.get(i, f"{i} bottles of beer")
    print(f"{bottle} on the wall,")
    print(f"{bottle}.")
    print("Take one down, pass it around,")
    bottle = bottles.get(i - 1, f"{i - 1} bottles of beer")
    print(f"{bottle} on the wall.")
```

Compiling it is as simple as:

 - `monty bottle.py`

Now an executable file will be emitted with the same name `bottle`.

 - `./bottle`
