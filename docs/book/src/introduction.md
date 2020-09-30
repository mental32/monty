# Introduction

Monty is a strongly typed dialect of traditional Python. It aims to provide the
programmer with additional layer of safety by leveraging a strong type system
and, additionally, the capability of being compiled by IR-backends like
[montyc](https://github.com/mental32/montyc) into traditional machine
executable ELF binaries.

Monty is primarily implemented through this pure Python language frontend.
This package is intended to be run using CPython 3.8.x and it's responsible
for parsing, validating, typechecking and lowering the provided source code
into a form of IR called "Monty IR"

Monty IR is intended to be the only real product of this frontend, it's
intended to be consumed further by additional backends such as:

* an IR interpreter (MIRI)
* or a traditional compiler (montyc)

This development guide is being written as a documentation effort to
communicate the high-level design and reasoning behind the frontend and
language implementation.
