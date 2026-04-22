use super::blocks;
use super::stack::Stack;
use crate::parser::Token;
use crate::parser::TokenKind;
use crate::parser::lexer::{ComparisonOperator, Mode, Operator};
use crate::parser::syntax_tree::ChildRef;

/// Parse an expression with infix precedence. Returns the starting child when we parsed something.
pub fn parse_expression(stack: &mut Stack) -> Option<ChildRef> {
	parse_expression_bp(stack, 0)
}

fn parse_expression_bp(stack: &mut Stack, min_bp: u8) -> Option<ChildRef> {
	let start = stack.mark();
	parse_prefix_expression(stack)?;
	parse_postfix_expression(stack);

	loop {
		let Some((left_bp, right_bp)) = peek_infix_binding_power(stack) else {
			break;
		};
		if left_bp < min_bp {
			break;
		}

		let _op = stack.advance();
		if parse_expression_bp(stack, right_bp).is_none() {
			stack.error("expected expression after operator");
			break;
		}
	}

	Some(stack.child(start))
}

fn parse_prefix_expression(stack: &mut Stack) -> Option<()> {
	match stack.peek()? {
		Token::Ident(_) => {
			let _ = stack.advance();
			Some(())
		}
		Token::Number(_) | Token::String(..) | Token::RawString(..) => {
			let _ = stack.advance();
			Some(())
		}
		Token::Paren(Mode::Open) => {
			let _ = stack.advance();
			if stack.subparse(&[TokenKind::CloseParen], parse_expression).is_none() {
				stack.error("expected expression after '('");
			}
			let _ = stack.expect(&Token::Paren(Mode::Close), "expected ')'");
			Some(())
		}
		Token::Negate | Token::Operator(Operator::Sub) => {
			let _ = stack.advance();
			if parse_expression_bp(stack, 14).is_none() {
				stack.error("expected expression after unary operator");
			}
			Some(())
		}
		Token::If => {
			let _ = stack.advance();
			if stack.subparse(&[TokenKind::OpenBrace], parse_expression).is_none() {
				stack.error("expected condition after 'if'");
			}
			let _block = blocks::parse_block(stack);
			Some(())
		}
		Token::Brace(Mode::Open) => {
			let _block = blocks::parse_block(stack);
			Some(())
		}
		_ => None,
	}
}

fn parse_postfix_expression(stack: &mut Stack) {
	loop {
		if !matches!(stack.peek(), Some(Token::Paren(Mode::Open))) {
			break;
		}

		let _ = stack.advance();
		loop {
			if matches!(stack.peek(), Some(Token::Paren(Mode::Close))) {
				break;
			}
			if stack.is_at_end() {
				break;
			}

			if stack
				.subparse(&[TokenKind::Comma, TokenKind::CloseParen], parse_expression)
				.is_none()
			{
				stack.error("expected expression in argument list");
			}

			match stack.peek() {
				Some(Token::Comma) => {
					let _ = stack.advance();
				}
				Some(Token::Paren(Mode::Close)) => break,
				_ => {
					stack.error("expected ',' or ')' in argument list");
					stack.skip_until(&[TokenKind::Comma, TokenKind::CloseParen]);
					if matches!(stack.peek(), Some(Token::Comma)) {
						let _ = stack.advance();
					}
				}
			}
		}
		let _ = stack.expect(&Token::Paren(Mode::Close), "expected ')'");
	}
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
