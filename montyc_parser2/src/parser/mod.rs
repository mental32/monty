mod comb;

pub enum ParsingError {}

pub type ParsingResult<T> = Result<T, ParsingError>;

pub trait Parseable {
    fn parse_from(string: &str) -> ParsingResult<Self>
    where
        Self: Sized;
}

impl Parseable for crate::ast::Module {
    fn parse_from(string: &str) -> ParsingResult<Self>
    where
        Self: Sized,
    {
        todo!()
    }
}
