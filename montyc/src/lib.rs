//! Compiler implementation of the monty dialect of Python.
//!
//! * I wanted type safety built into Python.
//! * I wanted to make C-FII pracatice easier.
//! * I want to separate Python (the language) from CPython (the interpreter)
//!

#![feature(const_generics)]
#![allow(incomplete_features)]
#![feature(bool_to_option)]

pub mod parser;
