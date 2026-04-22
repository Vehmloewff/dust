mod ast;
mod lexer;
mod parse;
mod token_stream;

pub use crate::parser::ast::Node;
pub use crate::parser::lexer::{Token, lex};
pub use crate::parser::token_stream::TokenStream;

use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
pub struct Diagnostic {
	pub message: String,
}

/// Lex the source and parse the AST. Always returns a file node (possibly with no items)
/// and any diagnostics from lenient error recovery.
pub fn parse(code: &str) -> (Node, Vec<Diagnostic>) {
	let tokens = lex(code);
	let stream = TokenStream::new(tokens);
	parse::parse_file(stream)
}
