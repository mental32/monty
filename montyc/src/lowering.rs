pub trait Lower {
    type Output;

    fn lower(&self) -> Self::Output;
}
