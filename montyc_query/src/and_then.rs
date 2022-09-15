use std::future::Future;
use std::pin::Pin;
use std::task::{Context, Poll};

use montyc_hlist::Func;
use pin_project::pin_project;

use crate::query::{QueryBase, QueryExt};

#[derive(Clone, Copy, Debug)]
pub struct AndThen<T, F> {
    pub(super) filter: T,
    pub(super) callback: F,
}

impl<T, F> QueryBase for AndThen<T, F>
where
    T: QueryExt,
    F: Func<T::Extract> + Clone + Send,
    F::Output: std::future::Future + Send,
{
    type Extract = (<F::Output as std::future::Future>::Output,);
    type Future = AndThenFuture<T, F>;

    #[inline]
    fn make_future(&self) -> Self::Future {
        AndThenFuture {
            state: State::First(self.filter.make_future(), self.callback.clone()),
        }
    }
}

#[allow(missing_debug_implementations)]
#[pin_project]
pub struct AndThenFuture<T, F>
where
    T: QueryExt,
    F: Func<T::Extract>,
    F::Output: std::future::Future + Send,
{
    #[pin]
    state: State<T::Future, F>,
}

#[pin_project(project = StateProj)]
enum State<T, F>
where
    T: std::future::Future,
    F: Func<T::Output>,
    F::Output: std::future::Future + Send,
{
    First(#[pin] T, F),
    Second(#[pin] F::Output),
    Done,
}

impl<T, F> Future for AndThenFuture<T, F>
where
    T: QueryExt,
    F: Func<T::Extract>,
    F::Output: std::future::Future + Send,
{
    type Output = (<F::Output as std::future::Future>::Output,);

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        self.project().state.poll(cx)
    }
}

impl<T, F> Future for State<T, F>
where
    T: std::future::Future,
    F: Func<T::Output>,
    F::Output: std::future::Future + Send,
{
    type Output = (<F::Output as std::future::Future>::Output,);

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        loop {
            match self.as_mut().project() {
                StateProj::First(first, second) => {
                    let ex1 = futures::ready!(first.poll(cx));
                    let fut2 = second.call(ex1);
                    self.set(State::Second(fut2));
                }
                StateProj::Second(second) => {
                    let ex2 = futures::ready!(second.poll(cx));
                    self.set(State::Done);
                    return Poll::Ready((ex2,));
                }
                StateProj::Done => panic!("polled after complete"),
            }
        }
    }
}
