//! LR-style parser with lenient error recovery. Builds AST nodes by consuming from
//! a token stream and pushing tokens into a per-node buffer; structure fields hold
//! ChildRef indices into that buffer.

use crate::parser::Token;
use crate::parser::ast::*;
use crate::parser::lexer::{ComparisonOperator, Mode, Operator};
use crate::parser::token_stream::TokenStream;
use std::mem;

/// Parse a full file from a token stream. Returns the file node and any diagnostics.
pub fn parse_file(stream: TokenStream) -> (Node, Vec<super::Diagnostic>) {
	Parser::parse_file(stream)
}

/// Parser state: stream, token buffer for the current node, and diagnostics.
struct Parser {
	stream: TokenStream,
	buffer: Vec<NodeChild>,
	diags: Vec<super::Diagnostic>,
}

impl Parser {
	fn new(stream: TokenStream) -> Self {
		Self {
			stream,
			buffer: Vec::new(),
			diags: Vec::new(),
		}
	}

	fn push(&mut self, token: Token) -> ChildRef {
		let idx = self.buffer.len();
		self.buffer.push(NodeChild::Token(token));
		ChildRef::Index(idx)
	}

	fn push_trivia(&mut self) {
		while let Some(t) = self.stream.peek() {
			if !TokenStream::is_trivia(t) {
				break;
			}
			if let Some(t) = self.stream.advance() {
				self.buffer.push(NodeChild::Token(t));
			}
		}
	}

	/// Advance stream and push the current token as semantic. Returns Missing if at end.
	fn advance_and_push(&mut self) -> ChildRef {
		self.push_trivia();
		match self.stream.advance() {
			Some(t) => self.push(t),
			None => ChildRef::Missing,
		}
	}

	/// If the next content token matches `pred`, advance (and push trivia + token) and return Index; else return Missing.
	fn expect_content<F>(&mut self, pred: F, msg: &str) -> ChildRef
	where
		F: FnOnce(&Token) -> bool,
	{
		self.push_trivia();
		match self.stream.peek() {
			Some(t) if pred(t) => {
				let tok = self.stream.advance().unwrap();
				self.push(tok)
			}
			_ => {
				self.diags.push(super::Diagnostic { message: msg.to_string() });
				ChildRef::Missing
			}
		}
	}

	/// Expect exact token (content only). On match: advance+push, return Index. Else: add diagnostic, return Missing.
	fn expect(&mut self, want: &Token, msg: &str) -> ChildRef {
		self.push_trivia();
		match self.stream.peek() {
			Some(t) if mem::discriminant(t) == mem::discriminant(want) => {
				// Simple equality for concrete tokens; for variants with data we'd compare.
				if self.tokens_match(t, want) {
					let tok = self.stream.advance().unwrap();
					return self.push(tok);
				}
			}
			_ => {}
		}
		self.diags.push(super::Diagnostic { message: msg.to_string() });
		ChildRef::Missing
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

	/// Parse top-level file: items (function defs) with recovery.
	pub fn parse_file(stream: TokenStream) -> (Node, Vec<super::Diagnostic>) {
		let mut p = Parser::new(stream);
		let mut items = Vec::new();

		loop {
			p.push_trivia();
			if p.stream.is_at_end() {
				break;
			}
			match p.stream.peek_content() {
				Some(Token::Fn) => {
					// Parse function into its own buffer, then push as child node.
					let mut fn_buf = Vec::new();
					mem::swap(&mut p.buffer, &mut fn_buf);
					let opt_def = p.parse_function_def_inner();
					mem::swap(&mut p.buffer, &mut fn_buf);
					if let Some(def) = opt_def {
						let content_length = fn_buf.len();
						let node = Node {
							tokens: fn_buf,
							structure: Structure::FunctionDef(def),
							content_length,
						};
						let idx = p.buffer.len();
						p.buffer.push(NodeChild::Node(node));
						items.push(ChildRef::Index(idx));
					}
				}
				_ => {
					p.diags.push(super::Diagnostic {
						message: "expected function definition".to_string(),
					});
					let recovered = p.stream.skip_until_statement_or_block_end();
					if !recovered && p.stream.is_at_end() {
						break;
					}
					if !matches!(p.stream.peek_content(), Some(Token::Fn) | None) {
						let _ = p.stream.advance_content();
					}
				}
			}
		}

		let content_length = p.buffer.len();
		let node = Node {
			tokens: mem::take(&mut p.buffer),
			structure: Structure::File(FileStructure { items }),
			content_length,
		};
		(node, p.diags)
	}

	/// Parse one function definition into current buffer. Returns FunctionDef on success.
	fn parse_function_def_inner(&mut self) -> Option<FunctionDef> {
		let fn_kw = self.expect(&Token::Fn, "expected 'fn'");
		let name = self.expect_content(|t| matches!(t, Token::Ident(_)), "expected function name");
		let open_paren = self.expect(&Token::Paren(Mode::Open), "expected '('");
		let mut params = Vec::new();
		loop {
			self.push_trivia();
			if let Some(Token::Paren(Mode::Close)) = self.stream.peek_content() {
				break;
			}
			let p_name = self.expect_content(|t| matches!(t, Token::Ident(_)), "expected parameter name");
			let p_colon = self.expect(&Token::Colon, "expected ':' after parameter name");
			let p_type = self.expect_content(|t| matches!(t, Token::Ident(_)), "expected parameter type");
			params.push(Param {
				name: p_name,
				colon: p_colon,
				type_ident: p_type,
			});
			self.push_trivia();
			match self.stream.peek_content() {
				Some(Token::Comma) => {
					self.advance_and_push();
				}
				Some(Token::Paren(Mode::Close)) => break,
				_ => {
					self.diags.push(super::Diagnostic {
						message: "expected ',' or ')' in parameter list".to_string(),
					});
					self.stream
						.skip_until_content(|t| matches!(t, Token::Paren(Mode::Close) | Token::Semi));
				}
			}
		}
		let close_paren = self.expect(&Token::Paren(Mode::Close), "expected ')'");
		let arrow = self.expect(&Token::Arrow, "expected '->'");
		let return_type = self.expect_content(|t| matches!(t, Token::Ident(_)), "expected return type");
		let block = self.parse_block();
		Some(FunctionDef {
			fn_keyword: fn_kw,
			name,
			open_paren,
			params,
			close_paren,
			arrow,
			return_type,
			block,
		})
	}

	/// Parse block `{ ... }`. Pushes all tokens to current buffer. Returns BlockExpr with refs into current buffer.
	fn parse_block(&mut self) -> BlockExpr {
		let open_brace = self.expect(&Token::Brace(Mode::Open), "expected '{'");
		let mut statements = Vec::new();
		let mut final_expression = None;

		loop {
			self.push_trivia();
			if self.stream.is_at_end() {
				self.diags.push(super::Diagnostic {
					message: "unclosed block".to_string(),
				});
				break;
			}
			match self.stream.peek_content() {
				Some(Token::Brace(Mode::Close)) => break,
				Some(Token::Let) | Some(Token::Return) => {
					let start = self.buffer.len();
					self.parse_statement();
					statements.push(ChildRef::Index(start));
				}
				Some(Token::Semi) => {
					// Empty statement
					self.advance_and_push();
				}
				_ => {
					// Expression statement or final expression: try to parse expr then semicolon or }
					let start = self.buffer.len();
					if self.parse_expression().is_some() {
						self.push_trivia();
						match self.stream.peek_content() {
							Some(Token::Semi) => {
								self.advance_and_push();
								statements.push(ChildRef::Index(start));
							}
							Some(Token::Brace(Mode::Close)) => {
								final_expression = Some(ChildRef::Index(start));
								break;
							}
							_ => {
								self.diags.push(super::Diagnostic {
									message: "expected ';' or '}'".to_string(),
								});
								statements.push(ChildRef::Index(start));
								self.stream
									.skip_until_content(|t| matches!(t, Token::Semi | Token::Brace(Mode::Close)));
							}
						}
					} else {
						// Could not parse expression: skip to recovery
						self.stream
							.skip_until_content(|t| matches!(t, Token::Semi | Token::Brace(Mode::Close) | Token::Let | Token::Return));
					}
				}
			}
		}

		let close_brace = self.expect(&Token::Brace(Mode::Close), "expected '}'");
		BlockExpr {
			open_brace,
			statements,
			final_expression,
			close_brace,
		}
	}

	fn parse_statement(&mut self) {
		self.push_trivia();
		match self.stream.peek_content() {
			Some(Token::Let) => {
				self.parse_let_stmt();
			}
			Some(Token::Return) => {
				self.parse_return_stmt();
			}
			_ => {}
		}
	}

	fn parse_let_stmt(&mut self) {
		let _let_kw = self.advance_and_push();
		let _name = self.expect_content(|t| matches!(t, Token::Ident(_)), "expected variable name after 'let'");
		let _type_annotation = self.parse_optional_let_type();
		let _initializer = self.parse_optional_let_value();
		let _semi = self.expect(&Token::Semi, "expected ';' after let");
	}

	fn parse_optional_let_type(&mut self) -> Option<LetType> {
		self.push_trivia();
		match self.stream.peek_content() {
			Some(Token::Colon) => {
				let colon = self.advance_and_push();
				let type_ = self.expect_content(|t| matches!(t, Token::Ident(_)), "expected type after ':'");
				Some(LetType { colon, type_ })
			}
			_ => None,
		}
	}

	fn parse_optional_let_value(&mut self) -> Option<LetValue> {
		self.push_trivia();
		match self.stream.peek_content() {
			Some(Token::Equals) => {
				let equals = self.advance_and_push();
				let start = self.buffer.len();
				let expr = if self.parse_expression().is_some() {
					ChildRef::Index(start)
				} else {
					ChildRef::Missing
				};
				Some(LetValue { equals, expr })
			}
			_ => None,
		}
	}

	fn parse_return_stmt(&mut self) {
		let _return_kw = self.advance_and_push();
		let start = self.buffer.len();
		let _expr = if self.parse_expression().is_some() {
			ChildRef::Index(start)
		} else {
			ChildRef::Missing
		};
		let _semi = self.expect(&Token::Semi, "expected ';' after return");
	}

	/// Parse an expression with infix precedence. Returns Some(()) if we parsed something.
	fn parse_expression(&mut self) -> Option<()> {
		self.parse_expression_bp(0)
	}

	fn parse_expression_bp(&mut self, min_bp: u8) -> Option<()> {
		self.push_trivia();
		self.parse_prefix_expression()?;
		self.parse_postfix_expression();

		loop {
			self.push_trivia();
			let Some((left_bp, right_bp)) = self.peek_infix_binding_power() else {
				break;
			};
			if left_bp < min_bp {
				break;
			}

			self.advance_and_push();
			if self.parse_expression_bp(right_bp).is_none() {
				self.diags.push(super::Diagnostic {
					message: "expected expression after operator".to_string(),
				});
				break;
			}
		}

		Some(())
	}

	fn parse_prefix_expression(&mut self) -> Option<()> {
		self.push_trivia();
		match self.stream.peek_content()? {
			Token::Ident(_) => {
				self.advance_and_push();
				Some(())
			}
			Token::Number(_) | Token::String(..) | Token::RawString(..) => {
				self.advance_and_push();
				Some(())
			}
			Token::Paren(Mode::Open) => {
				self.advance_and_push();
				if self.parse_expression().is_none() {
					self.diags.push(super::Diagnostic {
						message: "expected expression after '('".to_string(),
					});
				}
				self.expect(&Token::Paren(Mode::Close), "expected ')'");
				Some(())
			}
			Token::Negate | Token::Operator(Operator::Sub) => {
				self.advance_and_push();
				if self.parse_expression_bp(14).is_none() {
					self.diags.push(super::Diagnostic {
						message: "expected expression after unary operator".to_string(),
					});
				}
				Some(())
			}
			Token::If => {
				self.advance_and_push(); // if
				if self.parse_expression().is_none() {
					self.diags.push(super::Diagnostic {
						message: "expected condition after 'if'".to_string(),
					});
				}
				let _block = self.parse_block();
				Some(())
			}
			Token::Brace(Mode::Open) => {
				let _block = self.parse_block();
				Some(())
			}
			_ => None,
		}
	}

	fn parse_postfix_expression(&mut self) {
		loop {
			self.push_trivia();
			if !matches!(self.stream.peek_content(), Some(Token::Paren(Mode::Open))) {
				break;
			}

			self.advance_and_push(); // (
			loop {
				self.push_trivia();
				if matches!(self.stream.peek_content(), Some(Token::Paren(Mode::Close))) {
					break;
				}
				if self.parse_expression().is_none() {
					self.diags.push(super::Diagnostic {
						message: "expected expression in argument list".to_string(),
					});
					self.stream
						.skip_until_content(|t| matches!(t, Token::Comma | Token::Paren(Mode::Close)));
					if matches!(self.stream.peek_content(), Some(Token::Paren(Mode::Close))) {
						break;
					}
				}
				self.push_trivia();
				match self.stream.peek_content() {
					Some(Token::Comma) => {
						self.advance_and_push();
					}
					Some(Token::Paren(Mode::Close)) => break,
					_ => {
						self.diags.push(super::Diagnostic {
							message: "expected ',' or ')' in argument list".to_string(),
						});
						self.stream
							.skip_until_content(|t| matches!(t, Token::Comma | Token::Paren(Mode::Close)));
						if matches!(self.stream.peek_content(), Some(Token::Comma)) {
							self.advance_and_push();
						}
					}
				}
			}
			self.expect(&Token::Paren(Mode::Close), "expected ')'");
		}
	}

	fn peek_infix_binding_power(&mut self) -> Option<(u8, u8)> {
		match self.stream.peek_content()? {
			Token::Comparison(ComparisonOperator::Or) => Some((1, 2)),
			Token::Comparison(ComparisonOperator::And) => Some((3, 4)),
			Token::Comparison(ComparisonOperator::Equality | ComparisonOperator::NotEqual) => Some((5, 6)),
			Token::Comparison(
				ComparisonOperator::LessThan
				| ComparisonOperator::LessThanOrEqual
				| ComparisonOperator::GreaterThan
				| ComparisonOperator::GreaterThanOrEqual,
			) => Some((7, 8)),
			Token::Operator(Operator::Add | Operator::Sub) => Some((9, 10)),
			Token::Operator(Operator::Mul | Operator::Div | Operator::Mod) => Some((11, 12)),
			Token::Operator(Operator::Pow) => Some((13, 13)),
			_ => None,
		}
	}
}

#[cfg(test)]
mod tests {
	use super::parse_file;
	use crate::parser::ast::{ChildRef, Structure};
	use crate::parser::{TokenStream, lex};

	fn parse(code: &str) -> (crate::parser::Node, Vec<crate::parser::Diagnostic>) {
		let tokens = lex(code);
		parse_file(TokenStream::new(tokens))
	}

	#[test]
	fn recovers_from_invalid_top_level_and_continues() {
		let (node, diags) = parse("x;\nfn main() -> i32 {}\n");
		assert!(!diags.is_empty());
		match node.structure {
			Structure::File(file) => {
				assert_eq!(file.items.len(), 1);
				assert!(matches!(file.items[0], ChildRef::Index(_)));
			}
			_ => panic!("expected file node"),
		}
	}

	#[test]
	fn parses_arithmetic_comparison_and_logical_expressions() {
		let (_node, diags) = parse(
			"fn main(x: i32, y: i32) -> i32 {\n\
			     let z = 1 + 2 * 3 ^ 4;\n\
			     return x < y && y != 0 || z == 1;\n\
			 }",
		);
		assert!(diags.is_empty(), "unexpected diagnostics: {:?}", diags);
	}

	#[test]
	fn parses_calls_mixed_with_infix_expressions() {
		let (_node, diags) = parse(
			"fn main() -> i32 {\n\
			     let x = foo(1, 2 + 3) + bar(4);\n\
			     return x;\n\
			 }",
		);
		assert!(diags.is_empty(), "unexpected diagnostics: {:?}", diags);
	}
}
