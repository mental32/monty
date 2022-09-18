use std::pin::Pin;
use std::time::Duration;

use futures::Future;

use montyc_query::prelude::*;
use montyc_query::QueryDatabase;

struct Db(Storage);

impl QueryDatabase for Db {
    fn get_cached<Q>(
        &self,
        key: &<Q as montyc_query::QueryMeta>::InputTy,
    ) -> Option<<Q as montyc_query::QueryMeta>::OutputTy>
    where
        Q: montyc_query::QueryMeta,
    {
        None
    }

    fn set_cached<Q>(
        &self,
        key: <Q as montyc_query::QueryMeta>::InputTy,
        value: <Q as montyc_query::QueryMeta>::OutputTy,
    ) -> <Q as montyc_query::QueryMeta>::OutputTy
    where
        Q: montyc_query::QueryMeta,
    {
        value
    }
}

async fn sleep(_: &Db, (): ()) {
    tokio::time::sleep(Duration::from_secs(1)).await;
}

fn add_one<'a>(db: &'a Db, a: usize) -> Pin<Box<dyn Future<Output = usize> + 'a>> {
    Box::pin(async move {
        sleep(&db, ()).await;
        return a + 1;
    })
}

#[tokio::test]
async fn async_works() {
    let db = Db(Storage::EMPTY);

    db;
}
