use crate::object::PyValue;
use crate::{ctx::CallCx, PyResult};
use crate::{ObjectId, ObjectSpace, PyException};

mod argspec;
mod argument;
pub(self) mod generic;
mod then;

use self::generic::{Combine, Tuple};

pub use self::{
    argspec::ArgSpec,
    argument::{arg, Argument},
    then::Then,
};

pub trait ParserBase {
    type Extract: generic::Tuple;

    fn parse_from<'this, 'ecx, 'args, 'a, 'b>(
        &'this self,
        cx: CallCx<'ecx, 'args>,
        args: &'a [ObjectId],
    ) -> Result<(CallCx<'ecx, 'args>, &'b [ObjectId], Self::Extract), ()>
    where
        'a: 'b;
}

pub trait Parser: ParserBase {
    fn parse_with_cx<'ecx, 'args>(
        self,
        cx: CallCx<'ecx, 'args>,
    ) -> PyResult<(CallCx<'ecx, 'args>, Self::Extract)>
    where
        Self: Sized,
    {
        cx.parse_args_v2(self)
    }

    fn then_arg<A>(self, other: A) -> Then<Self, Argument<A>>
    where
        Self: Sized,
        A: ArgSpec,
        Argument<A>: Parser + Clone,
        <Self::Extract as Tuple>::HList:
            Combine<<<Argument<A> as ParserBase>::Extract as Tuple>::HList>,
    {
        self.then(Argument(other))
    }

    fn then<P>(self, other: P) -> Then<Self, P>
    where
        Self: Sized,
        P: Parser + Clone,
        <Self::Extract as Tuple>::HList: Combine<<P::Extract as Tuple>::HList>,
    {
        Then {
            first: self,
            second: other,
        }
    }
}

impl<T> Parser for T where T: ParserBase {}

// Extension traits and types for parsers that extract ObjectIds

pub struct ObjectMap<T, F>(pub T, pub F);

impl<T, U, F> ParserBase for ObjectMap<T, F>
where
    T: Parser<Extract = (ObjectId,)>,
    F: Fn(ObjectId, &PyValue) -> U,
{
    type Extract = (U,);

    #[allow(warnings)]
    fn parse_from<'this, 'ecx, 'args, 'a, 'b>(
        &'this self,
        cx: CallCx<'ecx, 'args>,
        args: &'a [ObjectId],
    ) -> Result<(CallCx<'ecx, 'args>, &'b [ObjectId], Self::Extract), ()>
    where
        'a: 'b,
    {
        let Self(parser, mapper) = self;

        let (cx, rem, (object,)) = parser.parse_from(cx, args)?;

        let ret = cx
            .ecx
            .runtime()
            .objects
            .with_object(object, |v| mapper(object, v));

        Ok((cx, rem, (ret,)))
    }
}

pub trait ObjectParserExt: Parser<Extract = (ObjectId,)> {
    fn map_value<T, F>(self, f: F) -> ObjectMap<Self, F>
    where
        Self: Sized,
        F: Fn(ObjectId, &PyValue) -> T,
    {
        ObjectMap(self, f)
    }
}

impl<T> ObjectParserExt for T where T: Parser<Extract = (ObjectId,)> {}

// Old combinator parsers

pub type Objects<'a> = &'a [ObjectId];

pub fn args_unboxed<T: ArgSpec, const N: usize>(
    positional: [T; N],
) -> impl for<'a> FnOnce(Objects<'a>) -> PyResult<(Objects<'a>, [ObjectId; N])> {
    move |input| {
        let (remaining, head) = and(positional.map(arg_))(input)?;
        let head: Box<[_; N]> = head.into_boxed_slice().try_into().unwrap();
        Ok((remaining, *head))
    }
}

pub fn args_opt_unboxed<T: ArgSpec, U: ArgSpec, const N: usize, const M: usize>(
    positional: [T; N],
    optional: [U; M],
) -> impl for<'a> FnOnce(Objects<'a>) -> PyResult<(Objects<'a>, ([ObjectId; N], [Option<ObjectId>; M]))>
{
    move |input| {
        let (mut remaining, head) = and(positional.map(arg_))(input)?;
        let head: Box<[_; N]> = head.into_boxed_slice().try_into().unwrap();

        let mut tail = [None; M];

        for (n, opt) in optional.iter().enumerate() {
            let (rem, opt) = match arg_(opt.name())(remaining) {
                Ok(ro) => ro,
                Err(_) => break,
            };

            tail[n].replace(opt);

            remaining = rem;
        }

        let parsed = (head.as_ref().clone(), tail);

        Ok((remaining, parsed))
    }
}

// pub fn args_opt<const N: usize, const M: usize>(
//     positional: [&'static str; N],
//     optional: [&'static str; M],
// ) -> impl for<'a> Fn(
//     Objects<'a>,
// ) -> PyResult<(
//     Objects<'a>,
//     (Box<[ObjectId; N]>, Box<[Option<ObjectId>; M]>),
// )> {
//     move |input| {
//         let (mut remaining, head) = and(positional.map(arg))(input)?;
//         let head: Box<[_; N]> = head.into_boxed_slice().try_into().unwrap();

//         let mut tail =
//             <Box<[Option<ObjectId>; M]>>::try_from(vec![None; M].into_boxed_slice()).unwrap();

//         for (n, opt) in optional.iter().enumerate() {
//             let (rem, opt) = match arg(opt.name())(remaining) {
//                 Ok(ro) => ro,
//                 Err(_) => break,
//             };

//             tail[n].replace(opt);

//             remaining = rem;
//         }

//         let parsed = (head, tail);

//         Ok((remaining, parsed))
//     }
// }

pub fn and<T, F, Input, const N: usize>(
    parsers: [F; N],
) -> impl FnOnce(Input) -> PyResult<(Input, Vec<T>)>
where
    F: Fn(Input) -> PyResult<(Input, T)>,
{
    move |input| {
        let mut last = input;
        let mut results = Vec::with_capacity(N);

        for parser in parsers {
            let (next, result) = parser(last)?;
            last = next;
            results.push(result);
        }

        Ok((last, results))
    }
}

fn arg_<I>(name: impl ArgSpec) -> impl Fn(&[I]) -> PyResult<(&[I], I)>
where
    I: Clone,
{
    let error_message = format!("missing required argument {}", name.name());

    move |args| match args {
        [] => PyException::type_error().set_message(&error_message).into(),
        [arg, rest @ ..] => Ok((rest, arg.clone())),
    }
}
