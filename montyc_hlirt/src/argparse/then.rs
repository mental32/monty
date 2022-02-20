use crate::ctx::CallCx;
use crate::ObjectId;

use super::generic::{Combine, CombinedTuples, Tuple};
use super::{Parser, ParserBase};

#[derive(Clone, Copy, Debug)]
pub struct Then<T, U> {
    pub(super) first: T,
    pub(super) second: U,
}

impl<T, U> ParserBase for Then<T, U>
where
    T: Parser,
    U: Parser,
    <T::Extract as Tuple>::HList: Combine<<U::Extract as Tuple>::HList>,
{
    type Extract = CombinedTuples<T::Extract, U::Extract>;

    fn parse_from<'this, 'ecx, 'args, 'a, 'b>(
        &'this self,
        cx: CallCx<'ecx, 'args>,
        args: &'a [ObjectId],
    ) -> Result<(CallCx<'ecx, 'args>, &'b [ObjectId], Self::Extract), ()>
    where
        'a: 'b,
    {
        let Self { first, second } = self;

        let (cx, args, first) = first.parse_from(cx, args)?;
        let (cx, args, second) = second.parse_from(cx, args)?;

        Ok((cx, args, first.combine(second)))
    }
}
