use crate::parser::syntax_tree::{ChildRef, Excess, Missing, Node, NodeChild, Structure};
use crate::parser::token_stream::TokenStream;
use crate::parser::{Token, TokenKind};
use std::mem;

pub type NodeScope = usize;

/// Parser state: stream and token buffers.
pub struct Stack {
	stream: TokenStream,
	buffers: Vec<Vec<NodeChild>>,
}

impl Stack {
	pub fn new(stream: TokenStream) -> Self {
		Self {
			stream,
			buffers: vec![Vec::new()],
		}
	}

	pub fn is_at_end(&self) -> bool {
		self.peek().is_none()
	}

	pub fn peek(&self) -> Option<&Token> {
		self.raw_peek_content()
	}

	pub fn peek_kind(&self) -> Option<TokenKind> {
		self.peek().map(TokenKind::from)
	}

	pub fn advance(&mut self) -> ChildRef {
		self.flush_trivia_into_buffer();
		match self.stream.advance() {
			Some(token) => self.push_token(token),
			None => ChildRef::Missing(Missing::default()),
		}
	}

	pub fn expect(&mut self, want: &Token, msg: &str) -> ChildRef {
		match self.peek() {
			Some(token) if self.tokens_match(token, want) => self.advance(),
			_ => self.missing(msg),
		}
	}

	pub fn expect_where<F>(&mut self, pred: F, msg: &str) -> ChildRef
	where
		F: FnOnce(&Token) -> bool,
	{
		match self.peek() {
			Some(token) if pred(token) => self.advance(),
			_ => self.missing(msg),
		}
	}

	pub fn missing(&self, msg: impl Into<String>) -> ChildRef {
		ChildRef::Missing(Missing::with_message(msg))
	}

	pub fn start_node(&mut self) -> NodeScope {
		let scope = self.buffers.len();
		self.buffers.push(Vec::new());
		scope
	}

	pub fn finish_node<T>(&mut self, scope: NodeScope, structure: T) -> ChildRef
	where
		T: Into<Structure>,
	{
		let node = self.finish_node_detached(scope, structure);
		self.push_node(node)
	}

	pub fn finish_node_detached<T>(&mut self, scope: NodeScope, structure: T) -> Node
	where
		T: Into<Structure>,
	{
		debug_assert_eq!(scope + 1, self.buffers.len());
		let children = self.buffers.pop().expect("node scope buffer must exist");
		let content_length = children.len();
		Node {
			children,
			structure: structure.into(),
			content_length,
		}
	}

	pub fn push_node(&mut self, node: Node) -> ChildRef {
		let idx = self.active_buffer().len();
		self.active_buffer_mut().push(NodeChild::Node(node));
		ChildRef::Index(idx)
	}

	pub fn abandon_node(&mut self, scope: NodeScope) {
		debug_assert_eq!(scope + 1, self.buffers.len());
		let _ = self.buffers.pop();
	}

	pub fn subparse<F>(&mut self, boundaries: &[TokenKind], mut parser: F) -> Option<ChildRef>
	where
		F: FnMut(&mut Stack) -> Option<ChildRef>,
	{
		loop {
			match self.peek_kind() {
				None => return None,
				Some(kind) if Self::is_boundary(kind, boundaries) => return None,
				Some(_) => {}
			}

			if let Some(child) = parser(self) {
				return Some(child);
			}

			match self.peek_kind() {
				None => return None,
				Some(kind) if Self::is_boundary(kind, boundaries) => return None,
				Some(_) => {
					if !self.advance_excess(None::<String>) {
						return None;
					}
				}
			}
		}
	}

	pub(crate) fn finish_root<T>(&mut self, structure: T) -> Node
	where
		T: Into<Structure>,
	{
		debug_assert_eq!(self.buffers.len(), 1);
		let children = mem::take(self.active_buffer_mut());
		let content_length = children.len();
		Node {
			children,
			structure: structure.into(),
			content_length,
		}
	}

	pub(crate) fn skip_until(&mut self, boundaries: &[TokenKind], msg: Option<&str>) -> bool {
		let mut attached = false;
		loop {
			match self.peek_kind() {
				None => return false,
				Some(kind) if Self::is_boundary(kind, boundaries) => return true,
				Some(_) => {
					let diagnostic = if attached { None } else { msg.map(str::to_owned) };
					if !self.advance_excess(diagnostic) {
						return false;
					}
					attached = true;
				}
			}
		}
	}

	fn active_buffer(&self) -> &Vec<NodeChild> {
		self.buffers.last().expect("stack must always have a buffer")
	}

	fn active_buffer_mut(&mut self) -> &mut Vec<NodeChild> {
		self.buffers.last_mut().expect("stack must always have a buffer")
	}

	fn push_token(&mut self, token: Token) -> ChildRef {
		let idx = self.active_buffer().len();
		self.active_buffer_mut().push(NodeChild::Token(token));
		ChildRef::Index(idx)
	}

	fn flush_trivia_into_buffer(&mut self) {
		while let Some(token) = self.stream.peek() {
			if !TokenStream::is_trivia(token) {
				break;
			}
			if let Some(token) = self.stream.advance() {
				self.active_buffer_mut().push(NodeChild::Token(token));
			}
		}
	}

	fn raw_peek_content(&self) -> Option<&Token> {
		let mut offset = 0;
		loop {
			let token = self.stream.peek_n(offset)?;
			if !TokenStream::is_trivia(token) {
				if matches!(token, Token::Eof) {
					return None;
				}
				return Some(token);
			}
			offset += 1;
		}
	}

	fn advance_excess<T>(&mut self, msg: Option<T>) -> bool
	where
		T: Into<String>,
	{
		self.flush_trivia_into_buffer();
		let Some(token) = self.stream.advance() else {
			return false;
		};
		let excess = match msg {
			Some(msg) => Excess::with_message(token, msg),
			None => Excess::new(token),
		};
		self.active_buffer_mut().push(NodeChild::Excess(excess));
		true
	}

	fn tokens_match(&self, a: &Token, b: &Token) -> bool {
		match (a, b) {
			(Token::Ident(s1), Token::Ident(s2)) => s1 == s2,
			(Token::Number(n1), Token::Number(n2)) => n1 == n2,
			(Token::Operator(o1), Token::Operator(o2)) => o1 == o2,
			(Token::Comparison(c1), Token::Comparison(c2)) => c1 == c2,
			(Token::Paren(m1), Token::Paren(m2)) => m1 == m2,
			(Token::Brace(m1), Token::Brace(m2)) => m1 == m2,
			_ => mem::discriminant(a) == mem::discriminant(b),
		}
	}

	fn is_boundary(kind: TokenKind, boundaries: &[TokenKind]) -> bool {
		boundaries.contains(&kind)
	}
}
