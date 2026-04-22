use super::blocks;
use super::stack::Stack;
use crate::parser::Token;
use crate::parser::TokenKind;
use crate::parser::lexer::{ComparisonOperator, Mode, Operator};
use crate::parser::syntax_tree::{BinaryExpr, CallExpr, ChildRef, IfExpr, ParenExpr, Structure, UnaryExpr};

pub fn parse_expression(stack: &mut Stack) -> Option<ChildRef> {
	let node = parse_expression_detached(stack)?;
	Some(stack.push_node(node))
}

pub(crate) fn parse_expression_detached(stack: &mut Stack) -> Option<crate::parser::Node> {
	parse_expression_node_bp(stack, 0)
}

fn parse_expression_node_bp(stack: &mut Stack, min_bp: u8) -> Option<crate::parser::Node> {
	let mut lhs = parse_prefix_expression_node(stack)?;
	lhs = parse_postfix_expression_node(stack, lhs);

	loop {
		let Some((left_bp, right_bp)) = peek_infix_binding_power(stack) else {
			break;
		};
		if left_bp < min_bp {
			break;
		}

		let scope = stack.start_node();
		let left = stack.push_node(lhs);
		let op = stack.advance();
		let right = match parse_expression_node_bp(stack, right_bp) {
			Some(rhs) => stack.push_node(rhs),
			None => stack.missing("expected expression after operator"),
		};
		lhs = stack.finish_node_detached(scope, BinaryExpr { left, op, right });
	}

	Some(lhs)
}

fn parse_prefix_expression_node(stack: &mut Stack) -> Option<crate::parser::Node> {
	match stack.peek()? {
		Token::Ident(_) => {
			let scope = stack.start_node();
			let ident = stack.advance();
			Some(stack.finish_node_detached(scope, Structure::Ident(ident)))
		}
		Token::Number(_) => {
			let scope = stack.start_node();
			let number = stack.advance();
			Some(stack.finish_node_detached(scope, Structure::NumberLit(number)))
		}
		Token::String(..) | Token::RawString(..) => {
			let scope = stack.start_node();
			let string = stack.advance();
			Some(stack.finish_node_detached(scope, Structure::StringLit(string)))
		}
		Token::Paren(Mode::Open) => {
			let scope = stack.start_node();
			let open_paren = stack.advance();
			let expr = match stack.subparse(&[TokenKind::CloseParen], parse_expression) {
				Some(expr) => expr,
				None => stack.missing("expected expression after '('"),
			};
			let close_paren = stack.expect(&Token::Paren(Mode::Close), "expected ')'");
			Some(stack.finish_node_detached(
				scope,
				ParenExpr {
					open_paren,
					expr,
					close_paren,
				},
			))
		}
		Token::Negate | Token::Operator(Operator::Sub) => {
			let scope = stack.start_node();
			let op = stack.advance();
			let operand = match parse_expression_node_bp(stack, 14) {
				Some(operand) => stack.push_node(operand),
				None => stack.missing("expected expression after unary operator"),
			};
			Some(stack.finish_node_detached(scope, UnaryExpr { op, operand }))
		}
		Token::If => {
			let scope = stack.start_node();
			let keyword = stack.advance();
			let condition = match stack.subparse(&[TokenKind::OpenBrace], parse_expression) {
				Some(condition) => condition,
				None => stack.missing("expected condition after 'if'"),
			};
			let block = blocks::parse_block_node(stack);
			Some(stack.finish_node_detached(scope, IfExpr { keyword, condition, block }))
		}
		Token::Brace(Mode::Open) => Some(blocks::parse_block_detached(stack)),
		_ => None,
	}
}

fn parse_postfix_expression_node(stack: &mut Stack, mut lhs: crate::parser::Node) -> crate::parser::Node {
	loop {
		if !matches!(stack.peek(), Some(Token::Paren(Mode::Open))) {
			break;
		}

		let scope = stack.start_node();
		let callee = stack.push_node(lhs);
		let open_paren = stack.advance();
		let mut args = Vec::new();
		loop {
			if matches!(stack.peek(), Some(Token::Paren(Mode::Close))) {
				break;
			}
			if stack.is_at_end() {
				break;
			}

			if let Some(arg) = stack.subparse(&[TokenKind::Comma, TokenKind::CloseParen], parse_expression) {
				args.push(arg);
			} else {
				args.push(stack.missing("expected expression in argument list"));
			}

			match stack.peek() {
				Some(Token::Comma) => {
					let _ = stack.advance();
				}
				Some(Token::Paren(Mode::Close)) => break,
				_ => {
					stack.skip_until(
						&[TokenKind::Comma, TokenKind::CloseParen],
						Some("expected ',' or ')' in argument list"),
					);
					if matches!(stack.peek(), Some(Token::Comma)) {
						let _ = stack.advance();
					}
				}
			}
		}
		let close_paren = stack.expect(&Token::Paren(Mode::Close), "expected ')'");
		lhs = stack.finish_node_detached(
			scope,
			CallExpr {
				callee,
				open_paren,
				args,
				close_paren,
			},
		);
	}

	lhs
}

fn peek_infix_binding_power(stack: &mut Stack) -> Option<(u8, u8)> {
	match stack.peek()? {
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
