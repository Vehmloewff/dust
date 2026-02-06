use serde::{Deserialize, Serialize};

use crate::utils::sub_usize;

#[derive(Debug, Clone)]
pub enum WrapResult<T> {
	Reject(InputStream, T),
	Built(InputStream, T),
}

#[derive(Debug, Clone)]
pub enum ParseResult<T> {
	Reject(InputStream),
	Built(InputStream, T),
}

#[derive(Debug, Clone)]
pub struct SpanBuilder {
	start: usize,
}

impl SpanBuilder {
	pub fn build(&self, stream: &InputStream) -> Span {
		Span {
			start: self.start,
			end: stream.get_index(),
		}
	}

	pub fn register_error<S: Into<String>>(&self, stream: &mut InputStream, message: S) {
		let diagnostic = Diagnostic::new_error(self.build(&stream), message);

		stream.add_diagnostic(diagnostic)
	}

	pub fn register_warn<S: Into<String>>(&self, stream: &mut InputStream, message: S) {
		let diagnostic = Diagnostic::new_warn(self.build(&stream), message);

		stream.add_diagnostic(diagnostic)
	}

	pub fn build_notice<S: Into<String>>(&self, stream: &mut InputStream, message: S) {
		let diagnostic = Diagnostic::new_notice(self.build(&stream), message);

		stream.add_diagnostic(diagnostic)
	}
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Span {
	start: usize,
	end: usize,
}

impl Span {
	pub fn new(start: usize, end: usize) -> Span {
		Span { start, end }
	}

	pub fn get_start_index(&self) -> usize {
		self.start
	}

	pub fn get_end_index(&self) -> usize {
		self.end
	}
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum DiagnosticLevel {
	Notice,
	Warn,
	Error,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Diagnostic {
	pub span: Span,
	pub message: String,
	pub level: DiagnosticLevel,
}

impl Diagnostic {
	pub fn new_error(span: Span, message: impl Into<String>) -> Diagnostic {
		Diagnostic {
			span,
			message: message.into(),
			level: DiagnosticLevel::Error,
		}
	}

	pub fn new_warn(span: Span, message: impl Into<String>) -> Diagnostic {
		Diagnostic {
			span,
			message: message.into(),
			level: DiagnosticLevel::Warn,
		}
	}

	pub fn new_notice(span: Span, message: impl Into<String>) -> Diagnostic {
		Diagnostic {
			span,
			message: message.into(),
			level: DiagnosticLevel::Notice,
		}
	}
}

#[derive(Debug, Clone)]
pub struct InputStream {
	index: usize,
	reverse_chars: Vec<char>,
	diagnostics: Vec<Diagnostic>,
}

impl InputStream {
	pub fn new(text: &str) -> InputStream {
		InputStream {
			index: 0,
			reverse_chars: text.chars().rev().collect::<Vec<char>>(),
			diagnostics: Vec::new(),
		}
	}

	pub fn span_instant(&self) -> Span {
		Span {
			start: self.index,
			end: self.index,
		}
	}

	pub fn get_index(&self) -> usize {
		self.index
	}

	pub fn pin(&self) -> SpanBuilder {
		SpanBuilder { start: self.get_index() }
	}

	pub fn instant_error(&mut self, message: impl Into<String>) {
		self.add_diagnostic(Diagnostic::new_error(self.span_instant(), message))
	}

	pub fn add_diagnostic(&mut self, diagnostic: Diagnostic) {
		self.diagnostics.push(diagnostic)
	}

	pub fn finish(mut self) -> (Vec<char>, Vec<Diagnostic>) {
		self.reverse_chars.reverse();

		(self.reverse_chars, self.diagnostics)
	}

	pub fn lookahead(&self, length: usize) -> String {
		let chars_length = self.reverse_chars.len();

		self.reverse_chars[sub_usize(chars_length, length)..chars_length]
			.iter()
			.rev()
			.collect::<String>()
	}

	pub fn peek(&self) -> Option<char> {
		self.reverse_chars.last().cloned()
	}

	pub fn peek_at(&self, index: usize) -> Option<char> {
		self.reverse_chars.get(self.reverse_chars.len() - index - 1).cloned()
	}

	pub fn pop(&mut self) -> Option<char> {
		self.index += 1;
		self.reverse_chars.pop()
	}

	pub fn consume(&mut self, count: usize) -> String {
		let mut string = String::with_capacity(count);

		for _ in 0..count {
			let character = match self.pop() {
				Some(character) => character,
				None => break,
			};

			string.push(character)
		}

		string
	}

	pub fn consume_char(&mut self, expected_char: char) -> Option<char> {
		self.consume_any_char(&[expected_char])
	}

	pub fn consume_any_char(&mut self, expected_chars: &[char]) -> Option<char> {
		match self.reverse_chars.last() {
			Some(character) => {
				if expected_chars.contains(character) {
					Some(self.pop().unwrap())
				} else {
					None
				}
			}
			None => None,
		}
	}

	pub fn consume_single_whitespace(&mut self) -> Option<char> {
		match self.reverse_chars.last() {
			Some(character) => {
				if character.is_whitespace() {
					Some(self.pop().unwrap())
				} else {
					None
				}
			}
			None => None,
		}
	}

	pub fn consume_single_digit(&mut self) -> Option<char> {
		match self.reverse_chars.last() {
			Some(character) => {
				if character.is_ascii_digit() {
					Some(self.pop().unwrap())
				} else {
					None
				}
			}
			None => None,
		}
	}

	/// Gets the next word, which is define by [a-zA-Z0-9_]
	pub fn peek_word(&mut self) -> Option<String> {
		let mut word = String::new();

		loop {
			let char = match self.peek_at(word.len()) {
				Some(char) => char,
				None => break,
			};

			if !char.is_alphanumeric() && char != '_' {
				break;
			}

			word.push(char)
		}

		if word.is_empty() { None } else { Some(word) }
	}

	/// Consumes `text` as long as it is in a word form.
	pub fn consume_word(&mut self, name: &str) -> Option<Span> {
		let next_word = self.peek_word()?;

		if next_word == name {
			let index = self.index;
			self.consume(name.len());

			Some(Span {
				start: index,
				end: index + name.len(),
			})
		} else {
			None
		}
	}

	pub fn consume_text(&mut self, text: &str) -> Option<String> {
		let compare = self.lookahead(text.len());

		if &compare == text { Some(self.consume(text.len())) } else { None }
	}

	pub fn consume_whitespace(&mut self) -> Option<String> {
		let mut string = match self.consume_single_whitespace() {
			Some(character) => String::from(character),
			None => return None,
		};

		loop {
			string.push(match self.consume_single_whitespace() {
				Some(character) => character,
				None => break,
			})
		}

		Some(string)
	}

	pub fn consume_digits(&mut self) -> Option<String> {
		let mut string = match self.consume_single_digit() {
			Some(character) => String::from(character),
			None => return None,
		};

		loop {
			string.push(match self.consume_single_digit() {
				Some(character) => character,
				None => break,
			})
		}

		Some(string)
	}
}
