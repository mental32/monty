//! A parsing library capable of parsing Python 3.8+ syntax.
//!
//! ```
//! use montyc_parser2::prelude::*;
//!
//! let module = Module::parse_from("import x.y.z\nclass Foo:\n    def bar(self) -> Self:\n        return self\n\n").expect("this is valid code.");
//! ```
//!
//! There are several important top level types to know about:
//!
//! * All the AST types (present in `montyc_parser2::ast`)
//! * High-level parsing traits (allows `Module::parse_from` and `Expr::parse_from`)
//! * AST walking traits for walking the AST types.
//! * `Spanned<T>` to wrap AST types in span information.
//!
#![forbid(unsafe_code)]

pub mod ast;
pub mod parser;
pub mod span_interner;
pub mod token;
pub mod token_iter;

pub mod prelude {
    use super::*;

    pub use ast::*;
    pub use parser::*;
    pub use token::*;
}
