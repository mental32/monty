pub mod tyeval;

use montyc_core::MontyResult;

pub(in crate) trait Typecheck<C, O = ()> {
    fn typecheck(&self, cx: C) -> MontyResult<O>;
}
