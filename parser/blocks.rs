use super::expressions;
use super::stack::Stack;
use crate::parser::Token;
use crate::parser::TokenKind;
use crate::parser::lexer::Mode;
use crate::parser::syntax_tree::{BlockExpr, ChildRef, ExprStmt, LetStmt, LetType, LetValue, ReturnStmt};

pub fn parse_block_node(stack: &mut Stack) -> ChildRef {
	let scope = stack.start_node();
	let block = parse_block_inner(stack);
	stack.finish_node(scope, block)
}

pub(crate) fn parse_block_detached(stack: &mut Stack) -> crate::parser::Node {
	let scope = stack.start_node();
	let block = parse_block_inner(stack);
	stack.finish_node_detached(scope, block)
}

fn parse_block_inner(stack: &mut Stack) -> BlockExpr {
	let open_brace = stack.expect(&Token::Brace(Mode::Open), "expected '{'");
	let mut statements = Vec::new();
	let mut final_expression = None;

	loop {
		if stack.is_at_end() {
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
				if let Some(expr) = expressions::parse_expression_detached(stack) {
					match stack.peek() {
						Some(Token::Semi) => statements.push(parse_expr_stmt_node(stack, expr)),
						Some(Token::Brace(Mode::Close)) => {
							final_expression = Some(stack.push_node(expr));
							break;
						}
						_ => {
							statements.push(stack.push_node(expr));
							stack.skip_until(&[TokenKind::Semi, TokenKind::CloseBrace], Some("expected ';' or '}'"));
						}
					}
				} else {
					stack.skip_until(&[TokenKind::Semi, TokenKind::CloseBrace, TokenKind::Let, TokenKind::Return], None);
				}
			}
		}
	}

	let close_brace = if stack.is_at_end() {
		stack.missing("Unterminated block. Expected '}'")
	} else {
		stack.expect(&Token::Brace(Mode::Close), "expected '}'")
	};
	BlockExpr {
		open_brace,
		statements,
		final_expression,
		close_brace,
	}
}

fn parse_statement(stack: &mut Stack) -> Option<ChildRef> {
	match stack.peek() {
		Some(Token::Let) => parse_let_stmt_node(stack),
		Some(Token::Return) => parse_return_stmt_node(stack),
		_ => None,
	}
}

fn parse_let_stmt_node(stack: &mut Stack) -> Option<ChildRef> {
	if !matches!(stack.peek(), Some(Token::Let)) {
		return None;
	}

	let scope = stack.start_node();
	let let_keyword = stack.advance();
	let name = stack.expect_where(|t| matches!(t, Token::Ident(_)), "expected variable name after 'let'");
	let type_annotation = parse_optional_let_type(stack);
	let initializer = parse_optional_let_value(stack);
	let semi = stack.expect(&Token::Semi, "expected ';' after let");

	Some(stack.finish_node(
		scope,
		LetStmt {
			let_keyword,
			name,
			type_annotation,
			initializer,
			semi,
		},
	))
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
				None => stack.missing("expected expression after '='"),
			};
			Some(LetValue { equals, expr })
		}
		_ => None,
	}
}

fn parse_return_stmt_node(stack: &mut Stack) -> Option<ChildRef> {
	if !matches!(stack.peek(), Some(Token::Return)) {
		return None;
	}

	let scope = stack.start_node();
	let return_keyword = stack.advance();
	let expr = match stack.subparse(&[TokenKind::Semi, TokenKind::CloseBrace], expressions::parse_expression) {
		Some(expr) => expr,
		None => stack.missing("expected expression after 'return'"),
	};
	let semi = stack.expect(&Token::Semi, "expected ';' after return");

	Some(stack.finish_node(
		scope,
		ReturnStmt {
			return_keyword,
			expr,
			semi,
		},
	))
}

fn parse_expr_stmt_node(stack: &mut Stack, expr: crate::parser::Node) -> ChildRef {
	let scope = stack.start_node();
	let expr = stack.push_node(expr);
	let semi = stack.expect(&Token::Semi, "expected ';' after expression");
	stack.finish_node(scope, ExprStmt { expr, semi })
}
