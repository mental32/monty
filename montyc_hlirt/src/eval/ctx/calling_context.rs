use crate::exception::{PyException, PyResult};
use crate::ObjectId;

use super::EvalGlue;

#[non_exhaustive]
pub struct CallCx<'a, 'b> {
    pub ecx: &'a mut dyn EvalGlue,
    pub args: &'b [ObjectId],
    pub globals: ObjectId,
}

mod sealed {
    pub trait ParamSpec {
        fn n_positional_arguments(&self) -> usize;

        fn n_optional_arguments(&self) -> usize;
    }

    impl<T> ParamSpec for &[T] {
        fn n_positional_arguments(&self) -> usize {
            self.len()
        }

        fn n_optional_arguments(&self) -> usize {
            0
        }
    }

    impl<T, U> ParamSpec for (&[T], &[U]) {
        fn n_positional_arguments(&self) -> usize {
            self.0.len()
        }

        fn n_optional_arguments(&self) -> usize {
            self.1.len()
        }
    }

    impl<T, const N: usize> ParamSpec for [T; N] {
        fn n_positional_arguments(&self) -> usize {
            N
        }

        fn n_optional_arguments(&self) -> usize {
            0
        }
    }

    impl<T, U, const N: usize, const M: usize> ParamSpec for ([T; N], [U; M]) {
        fn n_positional_arguments(&self) -> usize {
            N
        }

        fn n_optional_arguments(&self) -> usize {
            M
        }
    }

    impl<T, const N: usize> ParamSpec for Box<[T; N]> {
        fn n_positional_arguments(&self) -> usize {
            N
        }

        fn n_optional_arguments(&self) -> usize {
            0
        }
    }

    impl<T, U, const N: usize, const M: usize> ParamSpec for (Box<[T; N]>, Box<[U; M]>) {
        fn n_positional_arguments(&self) -> usize {
            N
        }

        fn n_optional_arguments(&self) -> usize {
            M
        }
    }
}

impl<'a, 'b> CallCx<'a, 'b> {
    pub fn new(ecx: &'a mut dyn EvalGlue, args: &'b [ObjectId], globals: ObjectId) -> Self {
        Self { ecx, args, globals }
    }

    pub fn parse_args_with<T, P>(&self, parser: P) -> PyResult<T>
    where
        P: FnOnce(&'b [ObjectId]) -> PyResult<(&'b [ObjectId], T)>,
        T: sealed::ParamSpec,
    {
        let (rem, args) = parser(self.args)?;

        match rem {
            [] => Ok(args),
            _rem => {
                let err = format!(
                    "takes {} positional arguments, but {} were given",
                    args.n_positional_arguments() + args.n_optional_arguments(),
                    self.args.len()
                );

                PyException::type_error().set_message(&err).into()
            }
        }
    }
}
