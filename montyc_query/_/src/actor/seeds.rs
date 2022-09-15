use crate::hlist;
use crate::hlist::{Func, HList, Tuple};

pub struct ActorSeeder {}

impl<S> Func<(S,)> for ActorSeeder
where
    S: ActorSeed,
{
    type Output = <S as ActorSeed>::ActorKind;

    fn call(&self, args: (S,)) -> Self::Output {
        todo!()
    }
}

pub trait ActorSeed {
    type ActorKind: crate::QueryActor;
}

impl ActorSeed for () {
    type ActorKind = ();
}

impl crate::QueryActor for () {
    type MessageAggregate = ();
}

pub struct QueryActorSeeds<G, A> {
    garden: G,
    seeds: A,
}

impl<G> QueryActorSeeds<G, ()> {
    pub fn new(garden: G) -> Self {
        QueryActorSeeds { garden, seeds: () }
    }

    pub fn and<A>(self, actor: A) -> QueryActorSeeds<G, hlist::HCons<A, ()>>
    where
        A: ActorSeed,
    {
        QueryActorSeeds {
            garden: self.garden,
            seeds: hlist::one(actor),
        }
    }
}

impl<H, T, G> QueryActorSeeds<G, hlist::HCons<H, T>>
where
    T: hlist::HList,
    hlist::HCons<H, T>: hlist::HList,
{
    pub fn and<A>(
        self,
        actor: A,
    ) -> QueryActorSeeds<G, <hlist::HCons<H, T> as hlist::Combine<hlist::HCons<A, ()>>>::Output>
    where
        A: ActorSeed,
        hlist::HCons<H, T>: hlist::Combine<<(A,) as Tuple>::HList>,
        hlist::HCons<A, ()>: hlist::Combine<hlist::HCons<H, T>>,
    {
        use hlist::Combine as _;

        let Self { garden, seeds } = self;

        QueryActorSeeds {
            garden,
            seeds: seeds.combine(hlist::one(actor)),
        }
    }

    pub fn finish(self) -> <hlist::HCons<H, T> as HList>::Tuple {
        self.seeds.flatten()
    }
}

pub fn foo() {
    impl ActorSeed for Foo {
        type ActorKind = FooActor;
    }

    struct FooActor;

    impl crate::QueryActor for FooActor {
        type MessageAggregate = ();
    }

    struct Foo;

    let (left, right) = QueryActorSeeds::new(ActorSeeder {})
        .and(Foo)
        .and(Foo)
        .finish();
}
