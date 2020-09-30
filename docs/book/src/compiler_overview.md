# Overview of the Compiler

The main entry point of the compiler is defined in `monty/driver.py` as
the `compile` function.

All unprocessed source code or input is intended to go through a call to:

```py
monty.compile(source)
```

For instance your typical "hello world" implementation might look like this:

```py
import monty

source = """
from builtins import print

def main():
    print("Hello, World!")
"""

code = monty.compile(source)
print(code.disassembly())
```

## A more detailed overview

The underlying `compile` implementation will then take the appropriate steps to
produce a `CompilationUnit` object, which is a big glob of state about the
compiled code.

Currently the call to compile will go through the following steps:

1. pass your source to the CPython `ast.parse()` function

    * This is actually one of the main reasons we lean on CPython, they provide
      a parser free of charge!

2. recursivly traverse and visit the produced `Module` node and validate that
   the initial semantics make sense

    * This includes behaviour like making sure there are no two parameters in a
      function definition's parameter list with the same name or it'll check if
      all arguments and even the return value are properly type annotated.

    * It even does some extra work during the initial traversal, like
      transforming all AST nodes into `monty.language.Item`s with the
      appropriate type field set.

    * Currently it's also being used to reject features of the language that
      are not supported, like async/await, classdef's, non-type annotated
      assingments

3. then the typechecker kicks in and it'll traverse the produced `Item` tree
   and inspect function definitions and value types and make sure everything
   makes sense.

    * At the moment this is where a lot of the abstraction starts to mix in
      together, for instance; the module import & compile will kick in here
      when the typechecker encounters an `Primitive.Import` type `Item`

    * This is also where we perform some primitive inference on the function
      definitions signature and assign it a concrete type, typically a type_id
      that resolves to `Callable[[args...], return_type]`.

4. finally we lower the AST/`Item` tree into `MIR` (Monty IR) using the
   `ModuleBuilder` API and bind the output blocks to the current `Module` item.

At this point we have a `CompilationUnit` object that contains helper methods
to access and walk all compiled modules, functions, expressions and figure out
the types for stuff!

The next steps are up to the user of the library (the IR consumer) which maybe
an interpreter for the IR implemented in your favourite language of choice that
performs the occasional JITing :)
