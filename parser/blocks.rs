use super::expressions;
use super::stack::Stack;
use crate::parser::Token;
use crate::parser::TokenKind;
use crate::parser::lexer::Mode;
use crate::parser::syntax_tree::{BlockExpr, ChildRef, LetType, LetValue};

/// Parse block `{ ... }`. Pushes all tokens to current buffer. Returns BlockExpr with refs into current buffer.
pub fn parse_block(stack: &mut Stack) -> BlockExpr {
	let open_brace = stack.expect(&Token::Brace(Mode::Open), "expected '{'");
	let mut statements = Vec::new();
	let mut final_expression = None;

	loop {
		if stack.is_at_end() {
			stack.error("unclosed block");
			break;
		}
		match stack.peek() {
			Some(Token::Brace(Mode::Close)) => break,
			Some(Token::Let) | Some(Token::Return) => {
				if let Some(statement) = parse_statement(stack) {
					statements.push(statement);
				}
			}
			Some(Token::Semi) => {
				let _ = stack.advance();
			}
			_ => {
				if let Some(expr) = expressions::parse_expression(stack) {
					match stack.peek() {
						Some(Token::Semi) => {
							let _ = stack.advance();
							statements.push(expr);
						}
						Some(Token::Brace(Mode::Close)) => {
							final_expression = Some(expr);
							break;
						}
						_ => {
							stack.error("expected ';' or '}'");
							statements.push(expr);
							stack.skip_until(&[TokenKind::Semi, TokenKind::CloseBrace]);
						}
					}
				} else {
					stack.skip_until(&[TokenKind::Semi, TokenKind::CloseBrace, TokenKind::Let, TokenKind::Return]);
				}
			}
		}
	}

	let close_brace = stack.expect(&Token::Brace(Mode::Close), "expected '}'");
	BlockExpr {
		open_brace,
		statements,
		final_expression,
		close_brace,
	}
}

fn parse_statement(stack: &mut Stack) -> Option<ChildRef> {
	let start = stack.mark();
	match stack.peek() {
		Some(Token::Let) => {
			parse_let_stmt(stack);
			Some(stack.child(start))
		}
		Some(Token::Return) => {
			parse_return_stmt(stack);
			Some(stack.child(start))
		}
		_ => None,
	}
}

fn parse_let_stmt(stack: &mut Stack) {
	let _let_kw = stack.advance();
	let _name = stack.expect_where(|t| matches!(t, Token::Ident(_)), "expected variable name after 'let'");
	let _type_annotation = parse_optional_let_type(stack);
	let _initializer = parse_optional_let_value(stack);
	let _semi = stack.expect(&Token::Semi, "expected ';' after let");
}

fn parse_optional_let_type(stack: &mut Stack) -> Option<LetType> {
	match stack.peek() {
		Some(Token::Colon) => {
			let colon = stack.advance();
			let type_ = stack.expect_where(|t| matches!(t, Token::Ident(_)), "expected type after ':'");
			Some(LetType { colon, type_ })
		}
		_ => None,
	}
}

fn parse_optional_let_value(stack: &mut Stack) -> Option<LetValue> {
	match stack.peek() {
		Some(Token::Equals) => {
			let equals = stack.advance();
			let expr = match stack.subparse(&[TokenKind::Semi, TokenKind::CloseBrace], expressions::parse_expression) {
				Some(expr) => expr,
				None => {
					stack.error("expected expression after '='");
					ChildRef::Missing
				}
			};
			Some(LetValue { equals, expr })
		}
		_ => None,
	}
}

fn parse_return_stmt(stack: &mut Stack) {
	let _return_kw = stack.advance();
	let _expr = match stack.subparse(&[TokenKind::Semi, TokenKind::CloseBrace], expressions::parse_expression) {
		Some(expr) => expr,
		None => {
			stack.error("expected expression after 'return'");
			ChildRef::Missing
		}
	};
	let _semi = stack.expect(&Token::Semi, "expected ';' after return");
}
