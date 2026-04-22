//! LR-style parser with lenient error recovery. Builds syntax tree nodes by consuming from
//! a token stream and pushing tokens into a per-node buffer; structure fields hold
//! ChildRef indices into that buffer.

mod blocks;
mod expressions;
mod items;
mod lexer;
mod stack;
mod syntax_tree;
#[cfg(test)]
mod tests;
mod token_stream;

pub use crate::parser::lexer::{Token, TokenKind, lex};
pub use crate::parser::syntax_tree::Node;
pub use crate::parser::token_stream::TokenStream;

use serde::{Deserialize, Serialize};
use stack::Stack;

#[derive(Debug, Serialize, Deserialize)]
pub struct Diagnostic {
	pub message: String,
}

/// Parse a full file from a token stream. Returns the file node and any diagnostics.
pub fn parse_file(stream: TokenStream) -> (Node, Vec<Diagnostic>) {
	let mut stack = Stack::new(stream);
	let node = items::parse_file(&mut stack);
	let diags = stack.finish();
	(node, diags)
}

/// Lex the source and parse the syntax tree. Always returns a file node (possibly with no items)
/// and any diagnostics from lenient error recovery.
pub fn parse(code: &str) -> (Node, Vec<Diagnostic>) {
	let tokens = lex(code);
	let stream = TokenStream::new(tokens);
	parse_file(stream)
}
