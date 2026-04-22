use crate::parser::syntax_tree::{ChildRef, NodeChild, Structure};
use crate::parser::{Node, TokenStream, lex};

fn parse(code: &str) -> (crate::parser::Node, Vec<crate::parser::Diagnostic>) {
	let tokens = lex(code);
	crate::parser::parse_file(TokenStream::new(tokens))
}

fn node_child<'a>(node: &'a Node, child_ref: &ChildRef) -> &'a NodeChild {
	match child_ref {
		ChildRef::Index(index) => &node.children[*index],
		ChildRef::Missing(_) => panic!("expected child node, found missing"),
	}
}

fn child_node<'a>(node: &'a Node, child_ref: &ChildRef) -> &'a Node {
	match node_child(node, child_ref) {
		NodeChild::Node(node) => node,
		other => panic!("expected node child, found {:?}", other),
	}
}

fn missing<'a>(child_ref: &'a ChildRef) -> &'a crate::parser::syntax_tree::Missing {
	match child_ref {
		ChildRef::Missing(missing) => missing,
		ChildRef::Index(_) => panic!("expected missing child ref"),
	}
}

fn file_item(node: &Node, index: usize) -> &Node {
	let Structure::File(file) = &node.structure else {
		panic!("expected file node");
	};
	child_node(node, &file.items[index])
}

fn function_block(function: &Node) -> &Node {
	let Structure::FunctionDef(function_def) = &function.structure else {
		panic!("expected function node");
	};
	child_node(function, &function_def.block)
}

fn first_return_expr(block: &Node) -> &Node {
	let Structure::BlockExpr(block_expr) = &block.structure else {
		panic!("expected block expr");
	};
	let return_stmt_node = child_node(block, &block_expr.statements[0]);
	let Structure::ReturnStmt(return_stmt) = &return_stmt_node.structure else {
		panic!("expected return stmt");
	};
	child_node(return_stmt_node, &return_stmt.expr)
}

#[test]
fn recovers_from_invalid_top_level_and_continues() {
	let (node, diags) = parse("x;\nfn main() -> i32 {}\n");
	assert!(!diags.is_empty());
	match &node.structure {
		Structure::File(file) => {
			assert_eq!(file.items.len(), 1);
			assert!(matches!(file.items[0], ChildRef::Index(_)));
		}
		_ => panic!("expected file node"),
	}
}

#[test]
fn function_body_statements_become_nodes() {
	let (node, diags) = parse(
		"fn main() -> i32 {\n\
		     let x = 1;\n\
		     foo(x);\n\
		     return x;\n\
		 }",
	);
	assert!(diags.is_empty(), "unexpected diagnostics: {:?}", diags);

	let function = file_item(&node, 0);
	let block = function_block(function);
	let Structure::BlockExpr(block_expr) = &block.structure else {
		panic!("expected block expr");
	};

	assert_eq!(block_expr.statements.len(), 3);
	assert!(matches!(
		child_node(block, &block_expr.statements[0]).structure,
		Structure::LetStmt(_)
	));
	assert!(matches!(
		child_node(block, &block_expr.statements[1]).structure,
		Structure::ExprStmt(_)
	));
	assert!(matches!(
		child_node(block, &block_expr.statements[2]).structure,
		Structure::ReturnStmt(_)
	));
}

#[test]
fn final_expression_is_a_node() {
	let (node, diags) = parse("fn main() -> i32 { foo() }");
	assert!(diags.is_empty(), "unexpected diagnostics: {:?}", diags);

	let block = function_block(file_item(&node, 0));
	let Structure::BlockExpr(block_expr) = &block.structure else {
		panic!("expected block expr");
	};
	let final_expr = block_expr.final_expression.as_ref().expect("expected final expression");
	assert!(matches!(child_node(block, final_expr).structure, Structure::CallExpr(_)));
}

#[test]
fn binary_precedence_is_explicit() {
	let (node, diags) = parse("fn main() -> i32 { return 1 + 2 * 3; }");
	assert!(diags.is_empty(), "unexpected diagnostics: {:?}", diags);

	let expr = first_return_expr(function_block(file_item(&node, 0)));
	let Structure::BinaryExpr(binary) = &expr.structure else {
		panic!("expected top-level binary expr");
	};
	assert!(matches!(child_node(expr, &binary.left).structure, Structure::NumberLit(_)));
	assert!(matches!(child_node(expr, &binary.right).structure, Structure::BinaryExpr(_)));
}

#[test]
fn call_expressions_are_explicit() {
	let (node, diags) = parse("fn main() -> i32 { return foo(1, 2 + 3); }");
	assert!(diags.is_empty(), "unexpected diagnostics: {:?}", diags);

	let expr = first_return_expr(function_block(file_item(&node, 0)));
	let Structure::CallExpr(call) = &expr.structure else {
		panic!("expected call expr");
	};
	assert!(matches!(child_node(expr, &call.callee).structure, Structure::Ident(_)));
	assert_eq!(call.args.len(), 2);
	assert!(matches!(child_node(expr, &call.args[0]).structure, Structure::NumberLit(_)));
	assert!(matches!(child_node(expr, &call.args[1]).structure, Structure::BinaryExpr(_)));
}

#[test]
fn unary_and_parenthesized_expressions_are_explicit() {
	let (node, diags) = parse("fn main(x: i32) -> i32 { return -(x + 1); }");
	assert!(diags.is_empty(), "unexpected diagnostics: {:?}", diags);

	let expr = first_return_expr(function_block(file_item(&node, 0)));
	let Structure::UnaryExpr(unary) = &expr.structure else {
		panic!("expected unary expr");
	};
	let operand = child_node(expr, &unary.operand);
	let Structure::ParenExpr(paren) = &operand.structure else {
		panic!("expected paren expr");
	};
	assert!(matches!(child_node(operand, &paren.expr).structure, Structure::BinaryExpr(_)));
}

#[test]
fn if_expressions_are_explicit() {
	let (node, diags) = parse("fn main() -> i32 { return if x { y }; }");
	assert!(diags.is_empty(), "unexpected diagnostics: {:?}", diags);

	let expr = first_return_expr(function_block(file_item(&node, 0)));
	let Structure::IfExpr(if_expr) = &expr.structure else {
		panic!("expected if expr");
	};
	assert!(matches!(child_node(expr, &if_expr.condition).structure, Structure::Ident(_)));
	let if_block = child_node(expr, &if_expr.block);
	let Structure::BlockExpr(if_block_expr) = &if_block.structure else {
		panic!("expected if block");
	};
	let final_expr = if_block_expr
		.final_expression
		.as_ref()
		.expect("expected final expression in if block");
	assert!(matches!(child_node(if_block, final_expr).structure, Structure::Ident(_)));
}

#[test]
fn recovery_preserves_structure() {
	let (node, diags) = parse("fn main() -> i32 { return 1 + ; }");
	assert!(!diags.is_empty(), "expected diagnostics");

	let expr = first_return_expr(function_block(file_item(&node, 0)));
	let Structure::BinaryExpr(binary) = &expr.structure else {
		panic!("expected binary expr");
	};
	assert!(matches!(binary.right, ChildRef::Missing(_)));
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
	let (node, diags) = parse(
		"fn main() -> i32 {\n\
		     let x = foo(, 2);\n\
		     return x;\n\
		 }",
	);
	assert_eq!(diags.len(), 1, "unexpected diagnostics: {:?}", diags);
	assert_eq!(diags[0].message, "expected expression in argument list");

	let block = function_block(file_item(&node, 0));
	let Structure::BlockExpr(block_expr) = &block.structure else {
		panic!("expected block expr");
	};
	let let_stmt_node = child_node(block, &block_expr.statements[0]);
	let Structure::LetStmt(let_stmt) = &let_stmt_node.structure else {
		panic!("expected let stmt");
	};
	let initializer = let_stmt.initializer.as_ref().expect("expected initializer");
	let call = child_node(let_stmt_node, &initializer.expr);
	let Structure::CallExpr(call) = &call.structure else {
		panic!("expected call expr");
	};
	assert_eq!(
		missing(&call.args[0]).diagnostics[0].message,
		"expected expression in argument list"
	);
	assert!(matches!(
		child_node(block, &block_expr.statements[1]).structure,
		Structure::ReturnStmt(_)
	));
}

#[test]
fn unterminated_block_diagnostic_lives_on_missing_close_brace() {
	let (node, diags) = parse("fn main() -> i32 { return 1;");
	assert_eq!(diags.len(), 1, "unexpected diagnostics: {:?}", diags);
	assert_eq!(diags[0].message, "Unterminated block. Expected '}'");

	let block = function_block(file_item(&node, 0));
	let Structure::BlockExpr(block_expr) = &block.structure else {
		panic!("expected block expr");
	};
	assert_eq!(
		missing(&block_expr.close_brace).diagnostics[0].message,
		"Unterminated block. Expected '}'"
	);
}

#[test]
fn excess_tokens_carry_their_own_diagnostics() {
	let (node, diags) = parse("x;\nfn main() -> i32 {}\n");
	assert_eq!(diags.len(), 1, "unexpected diagnostics: {:?}", diags);
	assert_eq!(diags[0].message, "expected function definition");
	match &node.children[0] {
		NodeChild::Excess(excess) => {
			assert_eq!(excess.diagnostics[0].message, "expected function definition");
		}
		other => panic!("expected excess child, found {:?}", other),
	}
}
