use crate::{
    exception::{PyException, PyResult},
    ObjectId,
};

type Objects<'a> = &'a [ObjectId];

pub fn args_opt_unboxed<const N: usize, const M: usize>(
    positional: [&'static str; N],
    optional: [&'static str; M],
) -> impl for<'a> Fn(Objects<'a>) -> PyResult<(Objects<'a>, ([ObjectId; N], [Option<ObjectId>; M]))>
{
    move |input| {
        let (mut remaining, head) = and(positional.map(arg))(input)?;
        let head: Box<[_; N]> = head.into_boxed_slice().try_into().unwrap();

        let mut tail = [None; M];

        for (n, opt) in optional.iter().enumerate() {
            let (rem, opt) = match arg(opt)(remaining) {
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

pub fn args_opt<const N: usize, const M: usize>(
    positional: [&'static str; N],
    optional: [&'static str; M],
) -> impl for<'a> Fn(
    Objects<'a>,
) -> PyResult<(
    Objects<'a>,
    (Box<[ObjectId; N]>, Box<[Option<ObjectId>; M]>),
)> {
    move |input| {
        let (mut remaining, head) = and(positional.map(arg))(input)?;
        let head: Box<[_; N]> = head.into_boxed_slice().try_into().unwrap();

        let mut tail =
            <Box<[Option<ObjectId>; M]>>::try_from(vec![None; M].into_boxed_slice()).unwrap();

        for (n, opt) in optional.iter().enumerate() {
            let (rem, opt) = match arg(opt)(remaining) {
                Ok(ro) => ro,
                Err(_) => break,
            };

            tail[n].replace(opt);

            remaining = rem;
        }

        let parsed = (head, tail);

        Ok((remaining, parsed))
    }
}

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

pub fn arg<I>(name: &str) -> impl Fn(&[I]) -> PyResult<(&[I], I)>
where
    I: Clone,
{
    let error_message = format!("missing required argument {}", name);

    move |args| match args {
        [] => PyException::type_error().set_message(&error_message).into(),
        [arg, rest @ ..] => Ok((rest, arg.clone())),
    }
}
