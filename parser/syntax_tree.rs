use crate::parser::{Diagnostic, Token};
use serde::{Deserialize, Serialize};

/// Reference to a token in a node's `children` array. Either an index into the array
/// ([`Index`](ChildRef::Index)) or [`Missing`](ChildRef::Missing) when the parser
/// recovered from an error and the token was not present (e.g. omitted or invalid).
///
/// **Option vs ChildRef:** Use [`Option`]<[`ChildRef`]> (or `Option<SomeStruct>`) only when
/// the element is *legally* optional in the grammar. Use plain [`ChildRef`] for required
/// elements; [`Missing`](ChildRef::Missing) there always indicates a syntax error and
/// error recovery.
#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ChildRef {
	Index(usize),
	Missing(Missing),
}

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct Missing {
	pub diagnostics: Vec<Diagnostic>,
}

impl Missing {
	pub fn with_message(message: impl Into<String>) -> Self {
		Self {
			diagnostics: vec![Diagnostic { message: message.into() }],
		}
	}
}

/// One entry in a [`Node`]'s `children` array.
///
/// Normal tokens are stored as [`Token`](NodeChild::Token). Rare recovery artifacts can be
/// preserved as [`Excess`](NodeChild::Excess) when the parser wants to keep unexpected source
/// text without treating it as valid syntax for the node. Nested syntax is stored as
/// [`Node`](NodeChild::Node).
#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum NodeChild {
	Token(Token),
	Excess(Excess),
	Node(Node),
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Excess {
	pub token: Token,
	pub diagnostics: Vec<Diagnostic>,
}

impl Excess {
	pub fn new(token: Token) -> Self {
		Self {
			token,
			diagnostics: Vec::new(),
		}
	}

	pub fn with_message(token: Token, message: impl Into<String>) -> Self {
		Self {
			token,
			diagnostics: vec![Diagnostic { message: message.into() }],
		}
	}
}

/// A parse node: the unit of syntax that owns its child stream and structure.
///
/// **Child storage model:** Every token, excess token, or nested node for this node lives in
/// `children`. The fields in `structure` (and its variants like [`BlockExpr`], [`IfExpr`]) are
/// *[`ChildRef`]s* into `children`—e.g. `BlockExpr::open_brace` is the `{` token in this
/// array, not the token itself. This keeps a single source of truth for child data and lets
/// structure types stay small (just indices).
#[derive(Debug, Serialize, Deserialize)]
pub struct Node {
	pub children: Vec<NodeChild>,
	pub structure: Structure,
	pub content_length: usize,
}

impl Node {
	pub fn diagnostics(&self) -> Vec<Diagnostic> {
		let mut out = Vec::new();
		self.collect_diagnostics_into(&mut out);
		out
	}

	fn collect_diagnostics_into(&self, out: &mut Vec<Diagnostic>) {
		let mut visited = vec![false; self.children.len()];
		self.collect_structure_diagnostics(out, &mut visited);
		for index in 0..self.children.len() {
			if !visited[index] {
				self.collect_child_diagnostics(index, out, &mut visited);
			}
		}
	}

	fn collect_structure_diagnostics(&self, out: &mut Vec<Diagnostic>, visited: &mut [bool]) {
		match &self.structure {
			Structure::File(file) => {
				for item in &file.items {
					self.collect_child_ref_diagnostics(item, out, visited);
				}
			}
			Structure::FunctionDef(function) => {
				self.collect_child_ref_diagnostics(&function.fn_keyword, out, visited);
				self.collect_child_ref_diagnostics(&function.name, out, visited);
				self.collect_child_ref_diagnostics(&function.open_paren, out, visited);
				for param in &function.params {
					self.collect_child_ref_diagnostics(&param.name, out, visited);
					self.collect_child_ref_diagnostics(&param.colon, out, visited);
					self.collect_child_ref_diagnostics(&param.type_ident, out, visited);
				}
				self.collect_child_ref_diagnostics(&function.close_paren, out, visited);
				self.collect_child_ref_diagnostics(&function.arrow, out, visited);
				self.collect_child_ref_diagnostics(&function.return_type, out, visited);
				self.collect_child_ref_diagnostics(&function.block, out, visited);
			}
			Structure::BlockExpr(block) => {
				self.collect_child_ref_diagnostics(&block.open_brace, out, visited);
				for statement in &block.statements {
					self.collect_child_ref_diagnostics(statement, out, visited);
				}
				if let Some(final_expression) = &block.final_expression {
					self.collect_child_ref_diagnostics(final_expression, out, visited);
				}
				self.collect_child_ref_diagnostics(&block.close_brace, out, visited);
			}
			Structure::IfExpr(if_expr) => {
				self.collect_child_ref_diagnostics(&if_expr.keyword, out, visited);
				self.collect_child_ref_diagnostics(&if_expr.condition, out, visited);
				self.collect_child_ref_diagnostics(&if_expr.block, out, visited);
			}
			Structure::LetStmt(let_stmt) => {
				self.collect_child_ref_diagnostics(&let_stmt.let_keyword, out, visited);
				self.collect_child_ref_diagnostics(&let_stmt.name, out, visited);
				if let Some(type_annotation) = &let_stmt.type_annotation {
					self.collect_child_ref_diagnostics(&type_annotation.colon, out, visited);
					self.collect_child_ref_diagnostics(&type_annotation.type_, out, visited);
				}
				if let Some(initializer) = &let_stmt.initializer {
					self.collect_child_ref_diagnostics(&initializer.equals, out, visited);
					self.collect_child_ref_diagnostics(&initializer.expr, out, visited);
				}
				self.collect_child_ref_diagnostics(&let_stmt.semi, out, visited);
			}
			Structure::ReturnStmt(return_stmt) => {
				self.collect_child_ref_diagnostics(&return_stmt.return_keyword, out, visited);
				self.collect_child_ref_diagnostics(&return_stmt.expr, out, visited);
				self.collect_child_ref_diagnostics(&return_stmt.semi, out, visited);
			}
			Structure::ExprStmt(expr_stmt) => {
				self.collect_child_ref_diagnostics(&expr_stmt.expr, out, visited);
				self.collect_child_ref_diagnostics(&expr_stmt.semi, out, visited);
			}
			Structure::BinaryExpr(binary) => {
				self.collect_child_ref_diagnostics(&binary.left, out, visited);
				self.collect_child_ref_diagnostics(&binary.op, out, visited);
				self.collect_child_ref_diagnostics(&binary.right, out, visited);
			}
			Structure::CallExpr(call) => {
				self.collect_child_ref_diagnostics(&call.callee, out, visited);
				self.collect_child_ref_diagnostics(&call.open_paren, out, visited);
				for arg in &call.args {
					self.collect_child_ref_diagnostics(arg, out, visited);
				}
				self.collect_child_ref_diagnostics(&call.close_paren, out, visited);
			}
			Structure::NumberLit(number) | Structure::StringLit(number) | Structure::Ident(number) => {
				self.collect_child_ref_diagnostics(number, out, visited);
			}
			Structure::ParenExpr(paren) => {
				self.collect_child_ref_diagnostics(&paren.open_paren, out, visited);
				self.collect_child_ref_diagnostics(&paren.expr, out, visited);
				self.collect_child_ref_diagnostics(&paren.close_paren, out, visited);
			}
			Structure::UnaryExpr(unary) => {
				self.collect_child_ref_diagnostics(&unary.op, out, visited);
				self.collect_child_ref_diagnostics(&unary.operand, out, visited);
			}
		}
	}

	fn collect_child_ref_diagnostics(&self, child_ref: &ChildRef, out: &mut Vec<Diagnostic>, visited: &mut [bool]) {
		match child_ref {
			ChildRef::Index(index) => self.collect_child_diagnostics(*index, out, visited),
			ChildRef::Missing(missing) => out.extend(missing.diagnostics.iter().cloned()),
		}
	}

	fn collect_child_diagnostics(&self, index: usize, out: &mut Vec<Diagnostic>, visited: &mut [bool]) {
		if visited[index] {
			return;
		}
		visited[index] = true;
		match &self.children[index] {
			NodeChild::Token(_) => {}
			NodeChild::Excess(excess) => out.extend(excess.diagnostics.iter().cloned()),
			NodeChild::Node(node) => node.collect_diagnostics_into(out),
		}
	}
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Structure {
	/// Top-level file: sequence of function definitions (and possibly other items later).
	File(FileStructure),
	/// Function definition: `fn name(params) -> return_type { body }`.
	FunctionDef(FunctionDef),
	/// Block used as expression or function body: `{ stmt; stmt; expr? }`.
	BlockExpr(BlockExpr),
	/// If expression: `if condition { block }` (condition may optionally be parenthesized).
	IfExpr(IfExpr),
	/// Let binding: `let name = expr;`
	LetStmt(LetStmt),
	/// Return statement: `return expr;`
	ReturnStmt(ReturnStmt),
	/// Expression used as statement: `expr;`
	ExprStmt(ExprStmt),
	/// Binary operation: left op right (arithmetic or comparison).
	BinaryExpr(BinaryExpr),
	/// Function or method call: callee(args).
	CallExpr(CallExpr),
	/// Number literal; reference to the number token.
	NumberLit(ChildRef),
	/// String (or raw string) literal; reference to the string token.
	StringLit(ChildRef),
	/// Identifier reference; reference to the ident token.
	Ident(ChildRef),
	/// Parenthesized expression: ( expr ).
	ParenExpr(ParenExpr),
	/// Unary operation: op operand (e.g. `-x`).
	UnaryExpr(UnaryExpr),
}

impl From<FileStructure> for Structure {
	fn from(value: FileStructure) -> Self {
		Self::File(value)
	}
}

impl From<FunctionDef> for Structure {
	fn from(value: FunctionDef) -> Self {
		Self::FunctionDef(value)
	}
}

impl From<BlockExpr> for Structure {
	fn from(value: BlockExpr) -> Self {
		Self::BlockExpr(value)
	}
}

impl From<IfExpr> for Structure {
	fn from(value: IfExpr) -> Self {
		Self::IfExpr(value)
	}
}

impl From<LetStmt> for Structure {
	fn from(value: LetStmt) -> Self {
		Self::LetStmt(value)
	}
}

impl From<ReturnStmt> for Structure {
	fn from(value: ReturnStmt) -> Self {
		Self::ReturnStmt(value)
	}
}

impl From<ExprStmt> for Structure {
	fn from(value: ExprStmt) -> Self {
		Self::ExprStmt(value)
	}
}

impl From<BinaryExpr> for Structure {
	fn from(value: BinaryExpr) -> Self {
		Self::BinaryExpr(value)
	}
}

impl From<CallExpr> for Structure {
	fn from(value: CallExpr) -> Self {
		Self::CallExpr(value)
	}
}

impl From<ParenExpr> for Structure {
	fn from(value: ParenExpr) -> Self {
		Self::ParenExpr(value)
	}
}

impl From<UnaryExpr> for Structure {
	fn from(value: UnaryExpr) -> Self {
		Self::UnaryExpr(value)
	}
}

// ---------------------------------------------------------------------------
// Top-level and items
// ---------------------------------------------------------------------------

/// File structure: each item is a child node ref in the parent [`Node`]'s `children` array.
#[derive(Debug, Serialize, Deserialize)]
pub struct FileStructure {
	pub items: Vec<ChildRef>,
}

/// Function definition structure: token fields index the parent [`Node`]'s `children` array;
/// `block` is a child node ref to a nested [`Structure::BlockExpr`] node.
#[derive(Debug, Serialize, Deserialize)]
pub struct FunctionDef {
	pub fn_keyword: ChildRef,
	pub name: ChildRef,
	pub open_paren: ChildRef,
	pub params: Vec<Param>,
	pub close_paren: ChildRef,
	pub arrow: ChildRef,
	pub return_type: ChildRef,
	pub block: ChildRef,
}

/// Single parameter: `name: type`. Indexes into the parent node's `children` array.
#[derive(Debug, Serialize, Deserialize)]
pub struct Param {
	pub name: ChildRef,
	pub colon: ChildRef,
	pub type_ident: ChildRef,
}

// ---------------------------------------------------------------------------
// Blocks and statements
// ---------------------------------------------------------------------------

/// Block structure: delimiter fields index the parent [`Node`]'s `children` array.
/// `statements` and `final_expression` refer to child statement/expression nodes.
/// `final_expression` is [`Option`] because a block may legally end with statements only.
#[derive(Debug, Serialize, Deserialize)]
pub struct BlockExpr {
	pub open_brace: ChildRef,
	pub statements: Vec<ChildRef>,
	pub final_expression: Option<ChildRef>,
	pub close_brace: ChildRef,
}

/// If-expression structure: token fields index the parent [`Node`]'s `children` array.
/// `condition` and `block` refer to child nodes.
/// The condition is an expression (parentheses around it are optional in the grammar).
#[derive(Debug, Serialize, Deserialize)]
pub struct IfExpr {
	pub keyword: ChildRef,
	pub condition: ChildRef,
	pub block: ChildRef,
}

/// Optional type annotation on a let binding: `: type`. When present, both `colon` and
/// `type_ident` are required (use [`ChildRef`] only; [`Missing`](ChildRef::Missing) = error).
#[derive(Debug, Serialize, Deserialize)]
pub struct LetType {
	pub colon: ChildRef,
	pub type_: ChildRef,
}

/// Optional initializer on a let binding: `= expr`. When present, both `equals` and
/// `expr` are required. When omitted (e.g. `let name: string;`), the binding is
/// taken to have the zero value for the type; the type must be supplied in that case.
#[derive(Debug, Serialize, Deserialize)]
pub struct LetValue {
	pub equals: ChildRef,
	pub expr: ChildRef,
}

/// Let statement structure: all fields are indexes into the parent [`Node`]'s `children` array.
/// The type annotation is [`Option`] because `let name = expr;` is valid without a type;
/// when present, the whole `: type` is required (use [`Option`]<[`LetType`]>).
/// The initializer is [`Option`] because `let name: type;` is valid without a value (zero
/// value); when present, the whole `= expr` is required (use [`Option`]<[`LetValue`]>).
/// At least one of type or initializer must be supplied.
#[derive(Debug, Serialize, Deserialize)]
pub struct LetStmt {
	pub let_keyword: ChildRef,
	pub name: ChildRef,
	pub type_annotation: Option<LetType>,
	pub initializer: Option<LetValue>,
	pub semi: ChildRef,
}

/// Return statement structure: token fields index the parent [`Node`]'s `children` array.
/// `expr` refers to a child expression node.
#[derive(Debug, Serialize, Deserialize)]
pub struct ReturnStmt {
	pub return_keyword: ChildRef,
	pub expr: ChildRef,
	pub semi: ChildRef,
}

/// Expression statement structure: `semi` indexes the parent [`Node`]'s `children` array.
/// `expr` refers to a child expression node.
#[derive(Debug, Serialize, Deserialize)]
pub struct ExprStmt {
	pub expr: ChildRef,
	pub semi: ChildRef,
}

// ---------------------------------------------------------------------------
// Expressions
// ---------------------------------------------------------------------------

/// Binary expression: left op right. `op` indexes the parent [`Node`]'s `children` array.
/// `left` and `right` refer to child expression nodes.
#[derive(Debug, Serialize, Deserialize)]
pub struct BinaryExpr {
	pub left: ChildRef,
	pub op: ChildRef,
	pub right: ChildRef,
}

/// Call expression: callee(args). Delimiter fields index the parent [`Node`]'s `children` array.
/// `callee` and `args` refer to child expression nodes.
#[derive(Debug, Serialize, Deserialize)]
pub struct CallExpr {
	pub callee: ChildRef,
	pub open_paren: ChildRef,
	pub args: Vec<ChildRef>,
	pub close_paren: ChildRef,
}

/// Parenthesized expression: ( expr ). Delimiters index the parent [`Node`]'s `children` array;
/// `expr` refers to a child expression node.
#[derive(Debug, Serialize, Deserialize)]
pub struct ParenExpr {
	pub open_paren: ChildRef,
	pub expr: ChildRef,
	pub close_paren: ChildRef,
}

/// Unary expression: `op` indexes the parent [`Node`]'s `children` array;
/// `operand` refers to a child expression node.
#[derive(Debug, Serialize, Deserialize)]
pub struct UnaryExpr {
	pub op: ChildRef,
	pub operand: ChildRef,
}
