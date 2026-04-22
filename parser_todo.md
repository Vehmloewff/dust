# Syntax Tree Completion Spec

## Purpose

This document defines the next implementation phase for the parser: completing the syntax tree so that every parsed grammar construct is represented by an explicit syntax node, and every lexed token that belongs to a construct remains present in the tree.

This is **not** a design flaw report. The current codebase already moved from "AST" terminology to **syntax tree** terminology, and the remaining work is to finish implementing the planned node coverage.

---

## Core goals

The syntax tree should satisfy these properties:

1. **Explicit structure**
   - Every parsed construct becomes a syntax node with a `Structure` variant.
   - Statements and expressions are represented by child nodes, not just token start indices.

2. **Token preservation**
   - Every lexed token that is accepted into parsing remains present in the owning node's `tokens` buffer.
   - Trivia continues to be preserved.
   - Recovery may still use `ChildRef::Missing` for required-but-absent elements.

3. **Natural traversal**
   - File items, block statements, final expressions, operands, callees, arguments, and nested blocks are traversed through nodes.
   - Downstream code should not need to reconstruct structure from raw token offsets.

4. **Incremental completion of the planned syntax model**
   - The existing `Structure` variants in `parser/syntax_tree.rs` define the near-term target.
   - Parser behavior should be brought into alignment with those variants.

---

## Non-goals

This phase does **not** require:

- introducing a separate semantic AST layer yet
- changing the lossless token-owning design
- redesigning precedence or grammar rules
- reducing error recovery support

Those can happen later. This phase is specifically about completing the syntax tree representation.

---

## Current direction

The project should continue treating parser output as a **syntax tree**.

Relevant file:

- `parser/syntax_tree.rs`

The parser should keep its current lossless approach:

- each node owns a `tokens: Vec<NodeChild>`
- child structure uses `ChildRef`
- nested parsed constructs are stored as `NodeChild::Node(Node)`
- missing required syntax is represented with `ChildRef::Missing`

What changes in this phase is **construction completeness**: parsed constructs must become explicit nodes consistently.

---

## Required syntax coverage

The parser must construct nodes for all currently declared `Structure` variants:

- `File`
- `FunctionDef`
- `BlockExpr`
- `IfExpr`
- `LetStmt`
- `ReturnStmt`
- `ExprStmt`
- `BinaryExpr`
- `CallExpr`
- `NumberLit`
- `StringLit`
- `Ident`
- `ParenExpr`
- `UnaryExpr`

A construct is considered implemented only when the parser emits a `Node` with the matching `Structure` variant, not when it merely consumes the relevant tokens.

---

## Tree invariants

The following invariants should hold after this work is complete.

### 1. Every parsed construct is a node

Examples:

- a `let` statement becomes `Structure::LetStmt`
- `foo(1, 2)` becomes `Structure::CallExpr`
- `1 + 2 * 3` becomes nested `Structure::BinaryExpr` nodes according to precedence
- `(x + y)` becomes `Structure::ParenExpr`
- `-x` becomes `Structure::UnaryExpr`
- `if cond { ... }` becomes `Structure::IfExpr`

### 2. Parent structures refer to child nodes

Examples:

- `FileStructure::items` points to child nodes for top-level items
- `BlockExpr::statements` points to child statement nodes
- `BlockExpr::final_expression` points to an expression node when present
- `LetValue::expr` points to the initializer expression node
- `ReturnStmt::expr` points to the returned expression node
- `BinaryExpr::{left,right}` point to expression child nodes
- `CallExpr::callee` and `CallExpr::args` point to expression child nodes
- `IfExpr::condition` points to an expression child node

### 3. Tokens remain owned by the containing node

Node fields still refer into the node-local `tokens` array. Nested syntax is represented by `NodeChild::Node`, allowing explicit tree shape without losing token ownership.

### 4. Missing required syntax remains representable

For required syntax that was not present in source, the parser may still emit:

- `ChildRef::Missing` for required token slots
- structurally valid nodes containing missing fields after recovery

---

## Structural changes required in `parser/syntax_tree.rs`

The syntax tree types should be updated so that structure fields reference child nodes rather than token-start placeholders wherever the field semantically refers to a nested construct.

### File and item level

`FileStructure` already uses `items: Vec<ChildRef>` and should continue doing so, with each entry pointing to a `NodeChild::Node` containing a top-level item.

### Function definition

`FunctionDef` should stop embedding `BlockExpr` directly and instead point to a nested block node.

Recommended shape:

```rust
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
```

Where `block` refers to a `NodeChild::Node` whose `structure` is `Structure::BlockExpr`.

### Block expression

`BlockExpr` should store child statement/expression nodes.

Recommended shape:

```rust
pub struct BlockExpr {
    pub open_brace: ChildRef,
    pub statements: Vec<ChildRef>,
    pub final_expression: Option<ChildRef>,
    pub close_brace: ChildRef,
}
```

This type can remain structurally the same, but its semantics change:

- `statements` entries must point to child statement nodes
- `final_expression` must point to a child expression node

### If expression

`IfExpr` should reference child nodes for condition and block.

Recommended shape:

```rust
pub struct IfExpr {
    pub keyword: ChildRef,
    pub condition: ChildRef,
    pub block: ChildRef,
}
```

### Let and return statements

These already have the right overall shape, but their expression fields must point to actual child expression nodes.

### Expression nodes

These types should be interpreted as child-node references:

```rust
pub struct BinaryExpr {
    pub left: ChildRef,
    pub op: ChildRef,
    pub right: ChildRef,
}

pub struct CallExpr {
    pub callee: ChildRef,
    pub open_paren: ChildRef,
    pub args: Vec<ChildRef>,
    pub close_paren: ChildRef,
}

pub struct ParenExpr {
    pub open_paren: ChildRef,
    pub expr: ChildRef,
    pub close_paren: ChildRef,
}

pub struct UnaryExpr {
    pub op: ChildRef,
    pub operand: ChildRef,
}
```

For literal and identifier variants:

- `NumberLit(ChildRef)` continues to point to the token
- `StringLit(ChildRef)` continues to point to the token
- `Ident(ChildRef)` continues to point to the token

Those nodes are still explicit syntax nodes even though they wrap a single token reference.

---

## Parser implementation requirements

Relevant file:

- `parser/parse.rs`

The parser should be updated so that parsing functions return `ChildRef` references to newly-created child nodes whenever they parse a nested construct.

### General construction pattern

When parsing a nested construct:

1. swap out the current buffer or otherwise create an isolated child-node buffer
2. parse the construct into that buffer
3. build a `Node { tokens, structure, content_length }`
4. push it into the parent buffer as `NodeChild::Node(node)`
5. return `ChildRef::Index(idx)` for that node slot

That is already how top-level function definitions are created. The same pattern should be applied throughout statements and expressions.

---

## Required parser refactors

### 1. Parse statements into nodes

Current `parse_statement()` returns no structure.

Target:

- `parse_statement()` should return `Option<ChildRef>`
- it should dispatch to node-building helpers:
  - `parse_let_stmt_node()`
  - `parse_return_stmt_node()`
  - `parse_expr_stmt_node()` when needed at block level

#### Let statement

`parse_let_stmt()` should construct:

- a child node with `Structure::LetStmt`
- token refs for `let`, name, optional type annotation, optional initializer, semicolon
- initializer expression stored as a child node ref in `LetValue::expr`

#### Return statement

`parse_return_stmt()` should construct:

- a child node with `Structure::ReturnStmt`
- expression stored as a child expression node ref
- semicolon token ref

#### Expression statement

When a block parses `expr;`, it should construct:

- a child node with `Structure::ExprStmt`
- expression stored as a child expression node ref
- semicolon token ref

### 2. Parse block expressions into nodes

`parse_block()` should produce a `BlockExpr` structure for a dedicated child node rather than being embedded directly in `FunctionDef` or `IfExpr`.

Target behavior:

- `parse_block_node() -> ChildRef`
- block contents are parsed into statement child nodes and an optional final expression child node
- `Structure::BlockExpr(BlockExpr)` is wrapped in a child `Node`

### 3. Parse prefix expressions into nodes

`parse_prefix_expression()` currently consumes tokens only.

Target:

- replace it with a node-producing function, e.g. `parse_prefix_expression_node() -> Option<ChildRef>`

Required emitted nodes:

- identifier -> `Structure::Ident`
- number literal -> `Structure::NumberLit`
- string/raw string literal -> `Structure::StringLit`
- parenthesized expression -> `Structure::ParenExpr`
- unary expression -> `Structure::UnaryExpr`
- `if` expression -> `Structure::IfExpr`
- block expression -> `Structure::BlockExpr`

### 4. Parse postfix call expressions into nodes

`parse_postfix_expression()` should take an already-parsed callee node and repeatedly wrap it in `CallExpr` nodes.

Target shape:

- input: `callee: ChildRef`
- output: final expression `ChildRef`

For each `(` argument list `)` encountered:

- construct a `Structure::CallExpr`
- store the previous expression node as `callee`
- parse each argument into an expression child node and store refs in `args`
- return the new call node as the current expression

This supports chaining like:

- `foo()(bar)`
- `outer(inner(1))`

### 5. Parse infix expressions into nodes

`parse_expression_bp()` should return `Option<ChildRef>` instead of `Option<()>`.

Target behavior:

- parse left-hand side as a node
- for each infix operator with sufficient binding power:
  - parse operator token
  - parse right-hand side as a node
  - create `Structure::BinaryExpr(BinaryExpr { left, op, right })`
  - use that new node as the current left-hand side
- return the final expression node ref

This must preserve existing precedence and associativity behavior.

### 6. Parse `if` expressions into nodes

An `if` expression should produce:

- `keyword` -> `if` token
- `condition` -> expression child node
- `block` -> block child node

If parentheses around the condition are supported by the grammar, they should naturally appear as a nested `ParenExpr` node when present.

### 7. Update function definitions to reference block nodes

`parse_function_def_inner()` should:

- parse the body using `parse_block_node()`
- store the resulting child ref in `FunctionDef::block`

---

## Suggested parser API shape

One practical direction is:

```rust
fn parse_statement(&mut self) -> Option<ChildRef>;
fn parse_let_stmt_node(&mut self) -> Option<ChildRef>;
fn parse_return_stmt_node(&mut self) -> Option<ChildRef>;
fn parse_expr_stmt_node(&mut self, expr: ChildRef) -> ChildRef;

fn parse_block_node(&mut self) -> ChildRef;
fn parse_expression(&mut self) -> Option<ChildRef>;
fn parse_expression_bp(&mut self, min_bp: u8) -> Option<ChildRef>;
fn parse_prefix_expression_node(&mut self) -> Option<ChildRef>;
fn parse_postfix_expression_node(&mut self, lhs: ChildRef) -> ChildRef;
```

Exact naming can differ, but the important part is that nested constructs return child-node refs, not only success/failure.

---

## Implementation order

Recommended order of work:

### Phase 1: Adjust syntax tree field shapes

Update `parser/syntax_tree.rs` so embedded nested structures become child refs where appropriate:

- `FunctionDef::block: ChildRef`
- `IfExpr::block: ChildRef`
- any comments/docs should describe child-node refs, not token-start spans

### Phase 2: Node-producing block and statement parsing

Implement child-node construction for:

- `BlockExpr`
- `LetStmt`
- `ReturnStmt`
- `ExprStmt`

Once this phase is done, block contents are structurally traversable.

### Phase 3: Node-producing primary/prefix expressions

Implement explicit nodes for:

- `Ident`
- `NumberLit`
- `StringLit`
- `ParenExpr`
- `UnaryExpr`
- `IfExpr`
- block expressions used in expression position

### Phase 4: Node-producing postfix and infix expressions

Implement explicit nodes for:

- `CallExpr`
- `BinaryExpr`

This completes expression nesting.

### Phase 5: Wiring and traversal cleanup

Update all parent structures so they store the returned child refs.

### Phase 6: Tests

Add structure-focused parser tests.

---

## Testing requirements

Parser tests should validate tree shape, not only successful parsing.

### Minimum test cases

1. **Function body statements become nodes**
   - parse a function containing `let`, `return`, and `expr;`
   - assert block statement refs point to child nodes with the expected `Structure` variants

2. **Final expression is a node**
   - parse a block ending in an expression without semicolon
   - assert `final_expression` is present and points to the expected expression node

3. **Binary precedence is explicit**
   - parse `1 + 2 * 3`
   - assert a top-level `BinaryExpr(+)` whose right child is `BinaryExpr(*)`

4. **Call expressions are explicit**
   - parse `foo(1, 2 + 3)`
   - assert `CallExpr` with explicit callee and argument nodes

5. **Unary and parenthesized expressions are explicit**
   - parse `-(x + 1)`
   - assert `UnaryExpr` -> `ParenExpr` -> `BinaryExpr`

6. **If expressions are explicit**
   - parse `if x { y }`
   - assert `IfExpr` with condition node and block node

7. **Recovery preserves structure**
   - parse malformed input with missing delimiters/operators
   - assert nodes are still produced where possible and required missing fields use `ChildRef::Missing`

---

## Example target tree

For:

```dust
fn main() -> i32 {
  let x = 1 + 2 * 3;
  return foo(x);
}
```

The syntax tree should be structurally equivalent to:

- `File`
  - `FunctionDef`
    - `name`: token `main`
    - `block`: `BlockExpr`
      - `statements`
        - `LetStmt`
          - `name`: token `x`
          - `initializer`
            - `BinaryExpr` `+`
              - `left`: `NumberLit` `1`
              - `right`: `BinaryExpr` `*`
                - `left`: `NumberLit` `2`
                - `right`: `NumberLit` `3`
        - `ReturnStmt`
          - `expr`
            - `CallExpr`
              - `callee`: `Ident` `foo`
              - `args`
                - `Ident` `x`

Exact token ownership remains local to each node, but this structural shape must be directly represented in the tree.

---

## Success criteria

This spec is complete when all of the following are true:

- every currently declared `Structure` variant is emitted by the parser for its matching construct
- blocks store statement/expression child nodes rather than token-start placeholders
- function definitions and `if` expressions reference nested block nodes
- expression parsing returns explicit nodes for literals, identifiers, unary, paren, call, binary, block, and `if`
- every accepted lexed token remains present in the syntax tree
- parser tests assert structural shape for nested statements and expressions

---

## Summary

The project direction is correct: build a lossless syntax tree with explicit structure.

The remaining work is to finish that implementation consistently. The parser should keep preserving tokens and recovery behavior, while upgrading all statement and expression parsing paths so that they emit real syntax nodes throughout the tree.