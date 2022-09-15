use std::marker::PhantomData;
use std::pin::Pin;
use std::time::Duration;

use futures::Future;
use montyc_hlist::Func;
use montyc_hlist::ReflectFunc;
use montyc_query::prelude::*;
use montyc_query::query::QueryBase;
use montyc_query::query::QueryExt;
use montyc_query::QueryDatabase;

#[derive(Clone, Copy)]
struct Db;

// impl QueryDatabase for Db {
//     fn storage(&self) -> &montyc_query::storage::Storage {
//         &Storage::EMPTY
//     }
// }

async fn sleep(_: &Db, (): ()) {
    tokio::time::sleep(Duration::from_secs(1)).await;
}

fn add_one(db: &Db, a: usize) -> Pin<Box<dyn Future<Output = usize>>> {
    let db = *db;
    Box::pin(async move {
        sleep(&db, ()).await;
        return a + 1;
    })
}

#[tokio::test]
async fn async_works() {
    let db = Db;

    let add_one = montyc_query::any::any()
        .and_then(|| async move {})
        .make_future()
        .await;

    // fn args<F: ReflectFunc>(q: &Query<F>) -> PhantomData<<F as ReflectFunc>::ReflectIn> {
    //     PhantomData
    // }

    // let args = args(add_one);

    // let b = db.prepare(add_one).call(1).await;

    // assert_eq!(b, 2)
}
