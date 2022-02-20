use crate::{ctx::CallCx, ObjectId};

use super::{ArgSpec, ParserBase};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Argument<T>(pub T)
where
    T: ArgSpec;

impl<T> ParserBase for Argument<T>
where
    T: ArgSpec,
{
    type Extract = (ObjectId,);

    fn parse_from<'this, 'ecx, 'args, 'a, 'b>(
        &'this self,
        cx: CallCx<'ecx, 'args>,
        args: &'a [ObjectId],
    ) -> Result<(CallCx<'ecx, 'args>, &'b [ObjectId], Self::Extract), ()>
    where
        'a: 'b,
    {
        match args {
            [] => Err(()),
            [head, rest @ ..] => Ok((cx, rest, (*head,))),
        }
    }
}

pub fn arg<T>(spec: T) -> Argument<T>
where
    T: ArgSpec,
{
    Argument(spec)
}
