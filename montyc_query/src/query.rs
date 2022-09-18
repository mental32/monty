use montyc_hlist::{Func, Tuple};

use crate::and_then::AndThen;

pub trait QueryBase {
    type Extract: Tuple;
    type Future: std::future::Future<Output = Self::Extract>;

    fn make_future(&self) -> Self::Future;
}

pub trait QueryExt: QueryBase {
    fn and_then<F>(self, fun: F) -> AndThen<Self, F>
    where
        Self: Sized,
        F: Func<Self::Extract> + Clone,
        F::Output: std::future::Future + Send,
    {
        AndThen {
            filter: self,
            callback: fun,
        }
    }
}

impl<T: QueryBase> QueryExt for T {}
