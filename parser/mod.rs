mod expression;
mod whitespace;

use rupert::{Diagnostic, parse};

use crate::parser::expression::{Expression, parse_expression};

pub fn parse(code: &str) -> (Option<Expression>, Vec<Diagnostic>) {
	parse!(code, parse_expression)
}
