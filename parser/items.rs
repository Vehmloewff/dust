use super::blocks;
use super::stack::Stack;
use crate::parser::Token;
use crate::parser::TokenKind;
use crate::parser::lexer::Mode;
use crate::parser::syntax_tree::{FileStructure, FunctionDef, Node, Param};

/// Parse top-level file: items (function defs) with recovery.
pub fn parse_file(stack: &mut Stack) -> Node {
	let mut items = Vec::new();

	loop {
		if stack.is_at_end() {
			break;
		}

		match stack.peek() {
			Some(Token::Fn) => {
				let scope = stack.start_node();
				let opt_def = parse_function_def_inner(stack);
				if let Some(def) = opt_def {
					items.push(stack.finish_node(scope, def));
				} else {
					stack.abandon_node(scope);
				}
			}
			_ => {
				let recovered = stack.skip_until(
					&[TokenKind::Semi, TokenKind::CloseBrace, TokenKind::Fn],
					Some("expected function definition"),
				);
				if !recovered && stack.is_at_end() {
					break;
				}
				if !matches!(stack.peek(), Some(Token::Fn) | None) {
					let _ = stack.advance();
				}
			}
		}
	}

	stack.finish_root(FileStructure { items })
}

/// Parse one function definition into the current node scope. Returns FunctionDef on success.
fn parse_function_def_inner(stack: &mut Stack) -> Option<FunctionDef> {
	let fn_kw = stack.expect(&Token::Fn, "expected 'fn'");
	let name = stack.expect_where(|t| matches!(t, Token::Ident(_)), "expected function name");
	let open_paren = stack.expect(&Token::Paren(Mode::Open), "expected '('");
	let mut params = Vec::new();
	loop {
		if let Some(Token::Paren(Mode::Close)) = stack.peek() {
			break;
		}
		if stack.is_at_end() {
			break;
		}

		let p_name = stack.expect_where(|t| matches!(t, Token::Ident(_)), "expected parameter name");
		let p_colon = stack.expect(&Token::Colon, "expected ':' after parameter name");
		let p_type = stack.expect_where(|t| matches!(t, Token::Ident(_)), "expected parameter type");
		params.push(Param {
			name: p_name,
			colon: p_colon,
			type_ident: p_type,
		});

		match stack.peek() {
			Some(Token::Comma) => {
				let _ = stack.advance();
			}
			Some(Token::Paren(Mode::Close)) => break,
			_ => {
				stack.skip_until(
					&[TokenKind::CloseParen, TokenKind::Semi],
					Some("expected ',' or ')' in parameter list"),
				);
			}
		}
	}
	let close_paren = stack.expect(&Token::Paren(Mode::Close), "expected ')'");
	let arrow = stack.expect(&Token::Arrow, "expected '->'");
	let return_type = stack.expect_where(|t| matches!(t, Token::Ident(_)), "expected return type");
	let block = blocks::parse_block_node(stack);
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
