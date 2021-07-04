use montyc_core::MontyResult;

pub(in crate) trait Typecheck<C> {
    fn typecheck(&self, cx: C) -> MontyResult<()>;
}
