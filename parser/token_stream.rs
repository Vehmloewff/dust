//! Token stream over a slice of tokens with peek, pop, and error-recovery helpers.

use crate::parser::Token;
use crate::parser::lexer::Mode;

/// Stream over a token list. Tracks position and provides peek/advance and
/// content-only (skip trivia) views for LR-style parsing.
#[derive(Debug)]
pub struct TokenStream {
	tokens: Vec<Token>,
	pos: usize,
}

impl TokenStream {
	pub fn new(tokens: Vec<Token>) -> Self {
		Self { tokens, pos: 0 }
	}

	/// Current index into the token vector (for building syntax tree refs).
	pub fn position(&self) -> usize {
		self.pos
	}

	/// True if we are past the last token or positioned at EOF.
	pub fn is_at_end(&self) -> bool {
		self.pos >= self.tokens.len() || matches!(self.tokens.get(self.pos), Some(Token::Eof))
	}

	/// Peek at the token at the current position without advancing.
	pub fn peek(&self) -> Option<&Token> {
		self.tokens.get(self.pos)
	}

	/// Peek at the token at `self.position() + n`. Does not skip trivia.
	pub fn peek_n(&self, n: usize) -> Option<&Token> {
		self.tokens.get(self.pos + n)
	}

	/// Advance by one token and return it. Returns `None` at end.
	pub fn advance(&mut self) -> Option<Token> {
		let t = self.tokens.get(self.pos).cloned()?;
		self.pos += 1;
		Some(t)
	}

	/// Advance while the current token is trivia (whitespace, comment). Does not advance past end.
	pub fn skip_trivia(&mut self) {
		while let Some(t) = self.peek() {
			if !Self::is_trivia(t) {
				break;
			}
			self.pos += 1;
		}
	}

	/// Peek at the next *content* (non-trivia) token. Does not advance.
	pub fn peek_content(&mut self) -> Option<&Token> {
		let mut p = self.pos;
		while p < self.tokens.len() {
			let t = &self.tokens[p];
			if !Self::is_trivia(t) {
				if matches!(t, Token::Eof) {
					return None;
				}
				return Some(t);
			}
			p += 1;
		}
		None
	}

	/// Advance past trivia, then advance one content token and return it.
	/// Use when you want to consume the next semantic token and skip trivia.
	pub fn advance_content(&mut self) -> Option<Token> {
		self.skip_trivia();
		self.advance()
	}

	/// True if this token is whitespace or comment (does not affect grammar).
	pub fn is_trivia(t: &Token) -> bool {
		matches!(t, Token::Whitespace(_) | Token::Comment(_))
	}

	/// Advance until the stream is at end or the next content token satisfies `pred`.
	/// Returns the number of tokens consumed (including trivia). Use for error recovery
	/// to skip to a recovery point (e.g. next `;`, `}`, or `fn`).
	pub fn skip_until_content<P>(&mut self, pred: P) -> usize
	where
		P: Fn(&Token) -> bool,
	{
		let start = self.pos;
		loop {
			self.skip_trivia();
			if self.is_at_end() {
				break;
			}
			if pred(self.peek().unwrap()) {
				break;
			}
			self.advance();
		}
		self.pos - start
	}

	/// Skip to the next token that is one of: `;` `}` or `fn` (statement/block boundary or next item).
	/// Returns whether we hit a recovery token (vs end of input).
	pub fn skip_until_statement_or_block_end(&mut self) -> bool {
		loop {
			self.skip_trivia();
			match self.peek() {
				None => return false,
				Some(Token::Semi) | Some(Token::Brace(..)) => return true,
				Some(Token::Fn) => return true,
				_ => {
					self.advance();
				}
			}
		}
	}

	/// Skip to the next `}` (block end) or end of input. Consumes the `}` if found.
	pub fn skip_until_brace_close(&mut self) -> bool {
		let mut depth: i32 = 0;
		loop {
			self.skip_trivia();
			match self.peek() {
				None => return false,
				Some(Token::Brace(Mode::Open)) => {
					depth += 1;
					self.advance();
				}
				Some(Token::Brace(Mode::Close)) => {
					if depth == 0 {
						self.advance();
						return true;
					}
					depth -= 1;
					self.advance();
				}
				_ => {
					self.advance();
				}
			}
		}
	}

	/// Number of tokens in the underlying slice (for slicing when building node token lists).
	pub fn len(&self) -> usize {
		self.tokens.len()
	}

	/// Slice of tokens from `start` to current position (excluding current). Useful when
	/// building a node's token list from a range of the original stream.
	pub fn slice_from(&self, start: usize) -> &[Token] {
		let end = self.pos.min(self.tokens.len());
		if start >= end {
			return &[];
		}
		&self.tokens[start..end]
	}
}
