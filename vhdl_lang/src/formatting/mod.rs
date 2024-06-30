use crate::Token;

mod context;
mod design;
mod entity;
mod token;

struct Formatter {}

struct DesignUnitFormatter<'a, 'b> {
    formatter: &'a Formatter,
    tokens: &'b Vec<Token>,
}
