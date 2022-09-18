//! `montyc_query` is an implementation for a query-focused library for incremental compilation
//!
//! [Salsa](https://github.com/salsa-rs/salsa) is another framework focused for incremental query-based compilation;
//! it is also the main source of inspiration and reference used for this library.
//!
//! This crate is however obviously not Salsa; there are differnces mainly:
//!
//!  * salsa is full of sugar macros to hide ugly boilerplate
//!  * we need to be able to have `async fn`s work as query functions and this is not possible in Salsa, also it is difficult to write about of a function that may be a future from the perspective of a trait.
//!  * in Salsa properties of queries, interned structs and entity structs are declared at the place of definition, here we take an opt-in model treat regular functions and structs specially per-callsite.
//!
//! ### The magic happens in the database not anywhere else
//!
//! The design motto for this library is that "the magic happens in the database"
//!
//! I dislike the approach of tagging structs, enums, and function items with a magical macro
//! that will handle generating more code or automatically wiring queries together to providers.
//!
//! There should be basically no hidden complexity or control flow, everything is visible from
//! the call site. query functions are just functions, there is nothing extra that gets generated
//! inline with the function definition that extends the interface magically.
//!

pub(crate) mod and_then;
pub mod any;

pub mod query;
pub mod storage;

pub trait QueryMeta {
    type InputTy;
    type OutputTy;
}

pub trait QueryDatabase {
    fn get_cached<Q>(&self, key: &<Q as QueryMeta>::InputTy) -> Option<<Q as QueryMeta>::OutputTy>
    where
        Q: QueryMeta;

    fn set_cached<Q>(
        &self,
        key: <Q as QueryMeta>::InputTy,
        value: <Q as QueryMeta>::OutputTy,
    ) -> <Q as QueryMeta>::OutputTy
    where
        Q: QueryMeta;
}

impl QueryDatabase for () {
    fn get_cached<Q>(&self, _: &<Q as QueryMeta>::InputTy) -> Option<<Q as QueryMeta>::OutputTy>
    where
        Q: QueryMeta,
    {
        None
    }

    fn set_cached<Q>(
        &self,
        _: <Q as QueryMeta>::InputTy,
        value: <Q as QueryMeta>::OutputTy,
    ) -> <Q as QueryMeta>::OutputTy
    where
        Q: QueryMeta,
    {
        value
    }
}

pub mod prelude {
    pub use super::QueryDatabase;
    pub use crate::storage::Storage;
}
