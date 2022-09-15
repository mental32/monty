#[derive(Debug)]
pub struct HCons<H, T: HList>(pub(crate) H, pub(crate) T);

#[inline]
pub fn one<T>(val: T) -> HCons<T, ()> {
    HCons(val, ())
}

impl<H, T> Default for HCons<H, T>
where
    H: Default,
    T: Default + HList,
{
    fn default() -> Self {
        Self(Default::default(), Default::default())
    }
}

// Converts HCons (and ()) into tuples.
pub trait HList: Sized {
    type Tuple: Tuple<HList = Self>;

    fn flatten(self) -> Self::Tuple;
}

// Typeclass that tuples can be converted into a HCons (or unit ()).
pub trait Tuple: Sized {
    type HList: HList<Tuple = Self>;

    fn hlist(self) -> Self::HList;

    #[inline]
    fn combine<T>(self, other: T) -> CombinedTuples<Self, T>
    where
        Self: Sized,
        T: Tuple,
        Self::HList: Combine<T::HList>,
    {
        self.hlist().combine(other.hlist()).flatten()
    }
}

pub type CombinedTuples<T, U> =
    <<<T as Tuple>::HList as Combine<<U as Tuple>::HList>>::Output as HList>::Tuple;

// Combines HCons together.
pub trait Combine<T: HList> {
    type Output: HList;

    fn combine(self, other: T) -> Self::Output;
}

pub trait Func<Args> {
    type Output;

    fn call(&self, args: Args) -> Self::Output;

    fn as_dyn<'a>(&'a self) -> &'a dyn Func<Args, Output = Self::Output>
    where
        Self: Sized,
    {
        self
    }

    fn as_reflect(
        &self,
    ) -> &dyn ReflectFunc<ReflectIn = Self::ReflectIn, ReflectOut = Self::ReflectOut>
    where
        Self: ReflectFunc + Sized,
    {
        self
    }
}

impl<A, O> ReflectFunc for dyn Func<A, Output = O> {
    type ReflectIn = A;

    type ReflectOut = O;

    fn call_reflect(&self, args: Self::ReflectIn) -> Self::ReflectOut {
        self.call(args)
    }
}

pub trait ReflectFunc {
    type ReflectIn;
    type ReflectOut;

    fn call_reflect(&self, args: Self::ReflectIn) -> Self::ReflectOut;
}

// ===== impl Combine =====

impl<T: HList> Combine<T> for () {
    type Output = T;
    #[inline]
    fn combine(self, other: T) -> Self::Output {
        other
    }
}

impl<H, T: HList, U: HList> Combine<U> for HCons<H, T>
where
    T: Combine<U>,
    HCons<H, <T as Combine<U>>::Output>: HList,
{
    type Output = HCons<H, <T as Combine<U>>::Output>;

    #[inline]
    fn combine(self, other: U) -> Self::Output {
        HCons(self.0, self.1.combine(other))
    }
}

impl HList for () {
    type Tuple = ();
    #[inline]
    fn flatten(self) -> Self::Tuple {}
}

impl Tuple for () {
    type HList = ();

    #[inline]
    fn hlist(self) -> Self::HList {}
}

impl<F, R> Func<()> for F
where
    F: Fn() -> R,
{
    type Output = R;

    #[inline]
    fn call(&self, _args: ()) -> Self::Output {
        (*self)()
    }
}

#[macro_export]
macro_rules! hcons {
    ($H:expr) => { HCons($H, ()) };
    ($H:expr, $($T:expr),*) => { HCons($H, hcons!($($T),*)) };
}

#[macro_export]
macro_rules! HCons {
    ($H:ty) => { $crate::HCons<$H, ()> };
    ($H:ty, $($T:ty),*) => { $crate::HCons<$H, HCons!($($T),*)> };
}

macro_rules! hcons_pat {
    ($H:pat) => { HCons($H, ()) };
    ($H:pat, $($T:pat),*) => { HCons($H, hcons_pat!($($T),*)) };
}

macro_rules! generics {
    ($type:ident) => {
        impl<$type> HList for HCons!($type) {
            type Tuple = ($type,);

            #[inline]
            fn flatten(self) -> Self::Tuple {
                (self.0,)
            }
        }

        impl<$type> Tuple for ($type,) {
            type HList = HCons!($type);
            #[inline]
            fn hlist(self) -> Self::HList {
                hcons!(self.0)
            }
        }

        impl<F, R, $type> Func<HCons!($type)> for F
        where
            F: Fn($type) -> R,
        {
            type Output = R;

            #[inline]
            fn call(&self, args: HCons!($type)) -> Self::Output {
                (*self)(args.0)
            }

        }

        impl<F, R, $type> Func<($type,)> for F
        where
            F: Fn($type) -> R,
        {
            type Output = R;

            #[inline]
            fn call(&self, args: ($type,)) -> Self::Output {
                (*self)(args.0)
            }
        }


    };

    ($type1:ident, $( $type:ident ),*) => {
        generics!($( $type ),*);

        impl<$type1, $( $type ),*> HList for HCons!($type1, $($type),*) {
            type Tuple = ($type1, $( $type ),*);

            #[inline]
            fn flatten(self) -> Self::Tuple {
                #[allow(non_snake_case)]
                let hcons_pat!($type1, $( $type ),*) = self;
                ($type1, $( $type ),*)
            }
        }

        impl<$type1, $( $type ),*> Tuple for ($type1, $($type),*) {
            type HList = HCons!($type1, $( $type ),*);

            #[inline]
            fn hlist(self) -> Self::HList {
                #[allow(non_snake_case)]
                let ($type1, $( $type ),*) = self;
                hcons!($type1, $( $type ),*)
            }
        }

        impl<F, R, $type1, $( $type ),*> Func<HCons!($type1, $($type),*)> for F
        where
            F: Fn($type1, $( $type ),*) -> R,
        {
            type Output = R;

            #[inline]
            fn call(&self, args: HCons!($type1, $($type),*)) -> Self::Output {
                #[allow(non_snake_case)]
                let hcons_pat!($type1, $( $type ),*) = args;
                (*self)($type1, $( $type ),*)
            }
        }

        impl<F, R, $type1, $( $type ),*> Func<($type1, $($type),*)> for F
        where
            F: Fn($type1, $( $type ),*) -> R,
        {
            type Output = R;

            #[inline]
            fn call(&self, args: ($type1, $($type),*)) -> Self::Output {
                #[allow(non_snake_case)]
                let ($type1, $( $type ),*) = args;
                (*self)($type1, $( $type ),*)
            }
        }

    };
}

generics! {
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
    T11,
    T12,
    T13,
    T14,
    T15,
    T16
}
