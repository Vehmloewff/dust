use super::parse_file;
use crate::parser::syntax_tree::{ChildRef, Structure};
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

#[test]
fn trivia_is_hidden_from_parser_operations() {
	let (_node, diags) = parse(
		"fn main // name
		 ( // open
		 ) -> i32 {
		     return // value
		     1;
		 }",
	);
	assert!(diags.is_empty(), "unexpected diagnostics: {:?}", diags);
}

#[test]
fn recovers_invalid_argument_with_subparse_and_continues() {
	let (_node, diags) = parse(
		"fn main() -> i32 {\n\
		     let x = foo(, 2);\n\
		     return x;\n\
		 }",
	);
	assert_eq!(diags.len(), 1, "unexpected diagnostics: {:?}", diags);
	assert_eq!(diags[0].message, "expected expression in argument list");
}
