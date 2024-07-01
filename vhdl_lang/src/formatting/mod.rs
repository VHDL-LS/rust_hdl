use crate::Token;

mod context;
mod declaration;
mod design;
mod entity;
mod statement;
mod token;

struct Formatter {}

struct DesignUnitFormatter<'a, 'b> {
    formatter: &'a Formatter,
    tokens: &'b Vec<Token>,
}
