use std::pin::Pin;
use std::task::{Context, Poll};

use futures::Future;

use crate::query::{QueryBase, QueryExt};

pub fn any() -> impl QueryExt<Extract = ()> + Copy {
    Any
}

#[derive(Copy, Clone)]
#[allow(missing_debug_implementations)]
struct Any;

impl QueryBase for Any {
    type Extract = ();
    type Future = AnyFut;

    #[inline]
    fn make_future(&self) -> Self::Future {
        AnyFut
    }
}

#[allow(missing_debug_implementations)]
struct AnyFut;

impl Future for AnyFut {
    type Output = ();

    #[inline]
    fn poll(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<Self::Output> {
        Poll::Ready(())
    }
}
