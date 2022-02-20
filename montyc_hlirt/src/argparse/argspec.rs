pub trait ArgSpec {
    fn name<'a>(&'a self) -> &'a str;
}

impl<'a> ArgSpec for &str {
    fn name(&self) -> &str {
        self
    }
}
