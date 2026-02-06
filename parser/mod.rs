mod lexer;
mod stream;

pub use crate::parser::lexer::{lex, Token};

#[derive(Debug)]
pub enum Expression {}

#[derive(Debug)]
pub struct Diagnostic {
	pub message: String,
}

pub fn parse(_code: &str) -> (Option<Expression>, Vec<Diagnostic>) {
	(None, vec![])
}
