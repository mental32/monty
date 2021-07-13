pub(crate) mod tyeval;

use montyc_core::MontyResult;

/// A generic type checking trait.
///
/// It is generic over some given `Context` type to allow for more
/// fine-grained checking of types in specific contexts; and it is
/// generic over `Output` which is the type of the result of the evaluation.
///
/// It is used by the `tyeval` module to perform type checking, and
/// inference, of function bodies.
///
pub(in crate) trait Typecheck<Context, Output = ()> {
    fn typecheck(&self, cx: Context) -> MontyResult<Output>;
}
