//! An implementation of [Queries] via a [query trait](crate::Query)
//!
//! Herein lies the montyc query system... it is an implementation of an incremental
//! query interface driven internally by a lightweight actor system since the query
//! implementations are free to be written like actors or services, within a larger runtime.
//!
//! Worry not about unnecessary runtime overhead since if your query actors are not _really_
//! actors you can pass in futures and async fns to be called directly, eliding the requirement
//! for a runtime.
//!
//! ## Gardens and seeds
//!
//! We call generic any "runtime" a garden and types that act as an initial configuration for an actor a seed.
//!
//! A garden is a box where seeds are planted, and the seeds once planted produce their associated actor kind.
//!
//! [Queries]: https://ollef.github.io/blog/posts/query-based-compilers.html
//!

use std::{future::Future, marker::PhantomData, pin::Pin};

pub(crate) use montyc_hlist as hlist;
pub use montyc_hlist;

pub mod actor;

/// The fundamental query type.
pub trait Query {
    type Input;
    type Output;
}

// mod sealed {
//     use super::*;

//     /// Provider channels act as a buffer between the caller interacting with a [super::QueryProvider] and the underlying query actor handler ([QueryHandler])
//     ///
//     /// This allows montyc_query to be generic over a tokio channel or elide
//     /// the runtime requirement entirely by calling an async fn directly.
//     pub trait QueryProviderChannel<ActorT>
//     where
//         ActorT: QueryActor,
//     {
//         fn do_handle<Q>(&mut self, query_message: Q) -> <ActorT as QueryHandler<Q>>::Fut
//         where
//             <ActorT as QueryActor>::MessageAggregate: From<Q>,
//             ActorT: QueryHandler<Q>;
//     }
// }

/// A marker trait necessary for all query actors.
pub trait QueryActor {
    /// This type is the enum of all your underlying supported messages for which a handler is implemented for.
    type MessageAggregate;
}

pub trait HasQuery<Q>
where
    Q: Query,
{
}

pub struct QueryTrampoline<A, Q>(PhantomData<(A, Q)>)
where
    Q: Query,
    A: QueryActorDetails + HasQuery<Q>;

pub trait QueryActorDetails {
    type Input;
    type Output;
}

#[macro_export]
macro_rules! query_input_type {
    ($t:ty) => {
        $crate::montyc_hlist::HCons!($t)
    };
    ($t:ty, $(t2:ty),+) => {
        $crate::montyc_hlist::HCons!($t, $($t2),+)
    };
}

#[macro_export]
macro_rules! query {
    (
        @internal $name:ident;
    ) => {};
    (
        @internal $name:ident;
        fn [$f_name:ident / $c_name:ident ] (&self, $( $param:ident : $param_t:ty ),+ ) -> $( @intern )? $ret:ty $b:block
        $($tail:tt)*
    ) =>
    {
        pub struct $c_name;

        impl $crate::Query for $name < $c_name >
        {
            type Input = $crate::query_input_type!($( $param_t ),+);
            type Output = $ret;
        }

        impl $name < $c_name > {
            pub fn $f_name (&self, $( $param : $param_t ),+ ) -> $ret $b
        }
    };

    {
        impl $actor_name:ident {
            let $actor_input:ident = <InputTy>;
            let $actor_output:ident = <OutputTy>;

            $( $tail:tt )*
        }
    } =>
    {
        #[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
        pub struct $actor_name<T>(::std::marker::PhantomData<T>);

        pub struct $actor_input;

        pub struct $actor_output;

        impl<T> $crate::QueryActorDetails for $actor_name<T> {
            type Input = $actor_input;
            type Output = $actor_output;
        }

        $crate::query!(
            @internal $actor_name;
            $( $tail )*
        );
    };
}
