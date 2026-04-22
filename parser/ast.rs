use crate::parser::Token;
use serde::{Deserialize, Serialize};

/// Reference to a token in a node's `tokens` array. Either an index into the array
/// ([`Index`](TokenRef::Index)) or [`Missing`](TokenRef::Missing) when the parser
/// recovered from an error and the token was not present (e.g. omitted or invalid).
///
/// **Option vs TokenRef:** Use [`Option`]&lt;[`TokenRef`]&gt; (or `Option<SomeStruct>`) only when
/// the element is *legally* optional in the grammar. Use plain [`TokenRef`] for required
/// elements; [`Missing`](TokenRef::Missing) there always indicates a syntax error and
/// error recovery.
#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ChildRef {
	Index(usize),
	Missing,
}

/// One entry in a [`Node`]'s `tokens` array.
///
/// Normal tokens are stored as [`Token`](NodeChild::Token). Rare recovery artifacts can be
/// preserved as [`Excess`](NodeChild::Excess) when the parser wants to keep unexpected source
/// text without treating it as valid syntax for the node. Nested syntax is stored as
/// [`Node`](NodeChild::Node).
#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum NodeChild {
	Token(Token),
	Excess(Token),
	Node(Node),
}

/// A parse node: the unit of syntax that owns its token stream and structure.
///
/// **Token storage model:** Every token for this node lives in `tokens`. The fields in
/// `structure` (and its variants like [`BlockExpr`], [`IfExpr`]) are *[`TokenRef`]s* into
/// `tokens`—e.g. `BlockExpr::open_brace` is the `{` token in this array,
/// not the token itself. This keeps a single source of truth for token data and lets
/// structure types stay small (just indices).
#[derive(Debug, Serialize, Deserialize)]
pub struct Node {
	pub tokens: Vec<NodeChild>,
	pub structure: Structure,
	pub content_length: usize,
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

// ---------------------------------------------------------------------------
// Top-level and items
// ---------------------------------------------------------------------------

/// File structure: all fields are indexes into the parent [`Node`]'s `tokens` array.
/// Each entry in `item_starts` is the index of the first token of a top-level item
/// (e.g. the `fn` token of a function definition).
#[derive(Debug, Serialize, Deserialize)]
pub struct FileStructure {
	pub items: Vec<ChildRef>,
}

/// Function definition structure: all fields are indexes into the parent [`Node`]'s
/// `tokens` array; `block` is a nested structure whose fields also index the same array.
#[derive(Debug, Serialize, Deserialize)]
pub struct FunctionDef {
	pub fn_keyword: ChildRef,
	pub name: ChildRef,
	pub open_paren: ChildRef,
	pub params: Vec<Param>,
	pub close_paren: ChildRef,
	pub arrow: ChildRef,
	pub return_type: ChildRef,
	pub block: BlockExpr,
}

/// Single parameter: `name: type`. Indexes into the parent node's `tokens` array.
#[derive(Debug, Serialize, Deserialize)]
pub struct Param {
	pub name: ChildRef,
	pub colon: ChildRef,
	pub type_ident: ChildRef,
}

// ---------------------------------------------------------------------------
// Blocks and statements
// ---------------------------------------------------------------------------

/// Block structure: all fields are indexes into the parent [`Node`]'s `tokens` array.
/// Each entry in `statements` is the index of the first token of that statement
/// (e.g. `let`, `return`, or the start of an expression).
/// `final_expression` is [`Option`] because a block may legally end with statements only.
#[derive(Debug, Serialize, Deserialize)]
pub struct BlockExpr {
	pub open_brace: ChildRef,
	pub statements: Vec<ChildRef>,
	pub final_expression: Option<ChildRef>,
	pub close_brace: ChildRef,
}

/// If-expression structure: all token/expr fields are indexes into the parent [`Node`]'s
/// `tokens` array; `block` is a nested structure whose fields also index the same array.
/// The condition is an expression (parentheses around it are optional in the grammar).
#[derive(Debug, Serialize, Deserialize)]
pub struct IfExpr {
	pub keyword: ChildRef,
	pub condition: ChildRef,
	pub block: BlockExpr,
}

/// Optional type annotation on a let binding: `: type`. When present, both `colon` and
/// `type_ident` are required (use [`TokenRef`] only; [`Missing`](TokenRef::Missing) = error).
#[derive(Debug, Serialize, Deserialize)]
pub struct LetType {
	pub colon: ChildRef,
	pub type_: ChildRef,
}

/// Optional initializer on a let binding: `= expr`. When present, both `equals` and
/// `expr_start` are required. When omitted (e.g. `let name: string;`), the binding is
/// taken to have the zero value for the type; the type must be supplied in that case.
#[derive(Debug, Serialize, Deserialize)]
pub struct LetValue {
	pub equals: ChildRef,
	pub expr: ChildRef,
}

/// Let statement structure: all fields are indexes into the parent [`Node`]'s `tokens` array.
/// The type annotation is [`Option`] because `let name = expr;` is valid without a type;
/// when present, the whole `: type` is required (use [`Option`]&lt;[`LetType`]&gt;).
/// The initializer is [`Option`] because `let name: type;` is valid without a value (zero
/// value); when present, the whole `= expr` is required (use [`Option`]&lt;[`LetValue`]&gt;).
/// At least one of type or initializer must be supplied.
#[derive(Debug, Serialize, Deserialize)]
pub struct LetStmt {
	pub let_keyword: ChildRef,
	pub name: ChildRef,
	pub type_annotation: Option<LetType>,
	pub initializer: Option<LetValue>,
	pub semi: ChildRef,
}

/// Return statement structure: all fields are indexes into the parent [`Node`]'s `tokens` array.
/// The returned expression spans from `expr_start` through the token immediately before `semi`.
#[derive(Debug, Serialize, Deserialize)]
pub struct ReturnStmt {
	pub return_keyword: ChildRef,
	pub expr: ChildRef,
	pub semi: ChildRef,
}

/// Expression statement structure: indexes into the parent [`Node`]'s `tokens` array.
/// The expression spans from `expr_start` through the token immediately before `semi`.
#[derive(Debug, Serialize, Deserialize)]
pub struct ExprStmt {
	pub expr: ChildRef,
	pub semi: ChildRef,
}

// ---------------------------------------------------------------------------
// Expressions
// ---------------------------------------------------------------------------

/// Binary expression: left op right. All fields index the parent [`Node`]'s `tokens` array.
/// `left` and `right` are the first token of each operand (operands may span multiple tokens).
#[derive(Debug, Serialize, Deserialize)]
pub struct BinaryExpr {
	pub left: ChildRef,
	pub op: ChildRef,
	pub right: ChildRef,
}

/// Call expression: callee(args). All fields index the parent [`Node`]'s `tokens` array.
/// `args` holds the start index of each argument expression (comma-separated).
#[derive(Debug, Serialize, Deserialize)]
pub struct CallExpr {
	pub callee: ChildRef,
	pub open_paren: ChildRef,
	pub args: Vec<ChildRef>,
	pub close_paren: ChildRef,
}

/// Parenthesized expression: ( expr ). Indexes into the parent [`Node`]'s `tokens` array.
#[derive(Debug, Serialize, Deserialize)]
pub struct ParenExpr {
	pub open_paren: ChildRef,
	pub expr: ChildRef,
	pub close_paren: ChildRef,
}

/// Unary expression: op operand. Indexes into the parent [`Node`]'s `tokens` array.
#[derive(Debug, Serialize, Deserialize)]
pub struct UnaryExpr {
	pub op: ChildRef,
	pub operand: ChildRef,
}
