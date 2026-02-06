use std::fmt;

// String literals
pub const STRING_BOUNDARY: char = '"';
pub const STRING_DOUBLE_QUOTE: char = '"';
pub const STRING_SINGLE_QUOTE: char = '\'';
pub const STRING_ESCAPE: char = '\\';
pub const STRING_INTERPOLATION_START: &str = "${"; // string interpolation stop will stop at BRACE_CLOSE
pub const RAW_STRING_NOTE: char = 'r';
pub const RAW_STRING_COUNTER: char = '#';

// Braces (also used for string interpolation)
pub const BRACE_OPEN: char = '{';
pub const BRACE_CLOSE: char = '}';

// Parentheses
pub const PAREN_OPEN: char = '(';
pub const PAREN_CLOSE: char = ')';

// Keywords
pub const PUB_TOKEN: &str = "pub";
pub const FN_TOKEN: &str = "fn";
pub const LET_TOKEN: &str = "let";
pub const STRUCT_TOKEN: &str = "struct";
pub const RETURN_TOKEN: &str = "return";

// Punctuation
pub const EQUALS: char = '=';
pub const SEMI: char = ';';
pub const COMMA: char = ',';
pub const COLON: char = ':';
pub const DECIMAL_POINT: char = '.';

// Single-character operators
pub const OP_ADD: char = '+';
pub const OP_SUB: char = '-';
pub const OP_MUL: char = '*';
pub const OP_DIV: char = '/';
pub const OP_MOD: char = '%';
pub const OP_POW: char = '^';
pub const OP_GT: char = '>';
pub const OP_LT: char = '<';

// Multi-character operators
pub const OP_GE: &str = ">=";
pub const OP_LE: &str = "<=";
pub const OP_EQUALITY: &str = "==";
pub const OP_AND: &str = "&&";
pub const OP_OR: &str = "||";

// Line comment
pub const LINE_COMMENT_START: &str = "//";

// Return type arrow
pub const RETURN_TYPE_ARROW: &str = "->";

#[derive(Debug, Clone, PartialEq)]
pub enum Operator {
	Add,
	Sub,
	Mul,
	Div,
	Mod,
	Pow,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ComparisonOperator {
	LessThan,
	LessThanOrEqual,
	GreaterThan,
	GreaterThanOrEqual,
	Equality,
	And,
	Or,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Mode {
	Open,
	Close,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StringQuoting {
	FullyQuoted,
	Unquoted,
	LeftQuoted,
	RightQuoted,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
	// Keywords
	Pub,
	Fn,
	Let,
	Struct,
	Return,

	// Syntax
	Operator(Operator),
	Paren(Mode),
	Brace(Mode),
	Comparison(ComparisonOperator),
	Equals,
	Semi,
	Comma,
	Colon,
	Arrow,

	// Values
	Ident(String),
	Number(f64),
	String(String, StringQuoting),
	/// The number of # delimiters and the raw string content (so source can be reconstructed)
	RawString(u8, String),
	StringInteropolationStart,
	StringInteropolationEnd,

	Whitespace(String),
	Comment(String),
	Unknown(String),
	/// A zero-width token that is placed at the end of a file if it contained an unterminated string literal
	StringNotTerminated,
}

struct Cursor {
	chars: Vec<char>,
	pos: usize,
}

impl Cursor {
	fn new(code: &str) -> Self {
		Cursor {
			chars: code.chars().collect(),
			pos: 0,
		}
	}

	fn peek(&self) -> Option<char> {
		self.chars.get(self.pos).copied()
	}

	fn peek_n(&self, n: usize) -> Option<char> {
		self.chars.get(self.pos + n).copied()
	}

	fn advance(&mut self) -> Option<char> {
		let c = self.peek();
		if c.is_some() {
			self.pos += 1;
		}
		c
	}

	fn advance_over(&mut self, s: &str) -> bool {
		for (i, c) in s.chars().enumerate() {
			if self.peek_n(i) != Some(c) {
				return false;
			}
		}
		self.pos += s.chars().count();
		true
	}

	fn is_at_end(&self) -> bool {
		self.pos >= self.chars.len()
	}

	fn take_while(&mut self, mut pred: impl FnMut(char) -> bool) -> String {
		let mut s = String::new();
		while let Some(c) = self.peek() {
			if !pred(c) {
				break;
			}
			s.push(c);
			self.advance();
		}
		s
	}
}

fn is_ident_start(c: char) -> bool {
	c.is_ascii_alphabetic() || c == '_'
}

fn is_ident_rest(c: char) -> bool {
	c.is_ascii_alphanumeric() || c == '_'
}

fn keyword_to_token(s: &str) -> Option<Token> {
	match s {
		PUB_TOKEN => Some(Token::Pub),
		FN_TOKEN => Some(Token::Fn),
		LET_TOKEN => Some(Token::Let),
		STRUCT_TOKEN => Some(Token::Struct),
		RETURN_TOKEN => Some(Token::Return),
		_ => None,
	}
}

fn lex_one(cursor: &mut Cursor) -> Option<Token> {
	let c = cursor.peek()?;

	// Whitespace
	if c.is_whitespace() {
		let s = cursor.take_while(|x| x.is_whitespace());
		return Some(Token::Whitespace(s));
	}

	// Multi-character operators (before single-char)
	if cursor.advance_over(OP_OR) {
		return Some(Token::Comparison(ComparisonOperator::Or));
	}
	if cursor.advance_over(OP_AND) {
		return Some(Token::Comparison(ComparisonOperator::And));
	}
	if cursor.advance_over(OP_GE) {
		return Some(Token::Comparison(ComparisonOperator::GreaterThanOrEqual));
	}
	if cursor.advance_over(OP_LE) {
		return Some(Token::Comparison(ComparisonOperator::LessThanOrEqual));
	}
	if cursor.advance_over(OP_EQUALITY) {
		return Some(Token::Comparison(ComparisonOperator::Equality));
	}
	if cursor.advance_over(RETURN_TYPE_ARROW) {
		return Some(Token::Arrow);
	}

	// Single-character tokens
	if c == OP_ADD && cursor.advance().is_some() {
		return Some(Token::Operator(Operator::Add));
	}
	if c == OP_SUB && cursor.advance().is_some() {
		return Some(Token::Operator(Operator::Sub));
	}
	if c == OP_MUL && cursor.advance().is_some() {
		return Some(Token::Operator(Operator::Mul));
	}
	if c == OP_DIV && cursor.advance().is_some() {
		return Some(Token::Operator(Operator::Div));
	}
	if c == OP_MOD && cursor.advance().is_some() {
		return Some(Token::Operator(Operator::Mod));
	}
	if c == OP_POW && cursor.advance().is_some() {
		return Some(Token::Operator(Operator::Pow));
	}
	if c == OP_GT && cursor.advance().is_some() {
		return Some(Token::Comparison(ComparisonOperator::GreaterThan));
	}
	if c == OP_LT && cursor.advance().is_some() {
		return Some(Token::Comparison(ComparisonOperator::LessThan));
	}
	if c == PAREN_OPEN {
		cursor.advance();
		return Some(Token::Paren(Mode::Open));
	}
	if c == PAREN_CLOSE {
		cursor.advance();
		return Some(Token::Paren(Mode::Close));
	}
	if c == BRACE_OPEN {
		cursor.advance();
		return Some(Token::Brace(Mode::Open));
	}
	if c == BRACE_CLOSE {
		cursor.advance();
		return Some(Token::Brace(Mode::Close));
	}
	if c == EQUALS {
		cursor.advance();
		return Some(Token::Equals);
	}
	if c == SEMI {
		cursor.advance();
		return Some(Token::Semi);
	}
	if c == COMMA {
		cursor.advance();
		return Some(Token::Comma);
	}
	if c == COLON {
		cursor.advance();
		return Some(Token::Colon);
	}

	// Ident or keyword
	if is_ident_start(c) {
		let word = cursor.take_while(is_ident_rest);
		return Some(keyword_to_token(&word).unwrap_or_else(|| Token::Ident(word)));
	}

	// Number
	if c.is_ascii_digit() || (c == DECIMAL_POINT && cursor.peek_n(1).map_or(false, |d| d.is_ascii_digit())) {
		let mut s = String::new();
		if c != DECIMAL_POINT {
			s.push_str(&cursor.take_while(|x| x.is_ascii_digit()));
		}
		if cursor.peek() == Some(DECIMAL_POINT) {
			cursor.advance();
			s.push(DECIMAL_POINT);
			s.push_str(&cursor.take_while(|x| x.is_ascii_digit()));
		}
		if let Ok(n) = s.parse::<f64>() {
			return Some(Token::Number(n));
		}
		return Some(Token::Unknown(s));
	}

	// Unknown single char
	cursor.advance();
	Some(Token::Unknown(c.to_string()))
}

pub fn lex(code: &str) -> Vec<Token> {
	let mut cursor = Cursor::new(code);
	let mut tokens = Vec::new();

	loop {
		// Raw string: r then 0+ # then "
		if cursor.peek() == Some(RAW_STRING_NOTE) && cursor.peek_n(1).map_or(false, |c| c == RAW_STRING_COUNTER || c == STRING_BOUNDARY) {
			let start = cursor.pos;
			cursor.advance(); // r
			let n_hashes = cursor.take_while(|c| c == RAW_STRING_COUNTER).len();
			if cursor.peek() != Some(STRING_BOUNDARY) {
				// Not a raw string; rewind and lex as ident
				cursor.pos = start;
			} else {
				cursor.advance(); // "
				let end_marker = format!("\"{}", (0..n_hashes).map(|_| RAW_STRING_COUNTER).collect::<String>());
				let mut content = String::new();
				let mut closed = false;
				loop {
					if cursor.is_at_end() {
						tokens.push(Token::RawString(n_hashes as u8, content.clone()));
						tokens.push(Token::StringNotTerminated);
						break;
					}
					let rest: String = cursor.chars[cursor.pos..].iter().collect();
					if rest.starts_with(&end_marker) {
						for _ in 0..end_marker.chars().count() {
							cursor.advance();
						}
						closed = true;
						break;
					}
					content.push(cursor.advance().unwrap());
				}
				if closed {
					tokens.push(Token::RawString(n_hashes as u8, content));
				}
				continue;
			}
		}

		// Normal or interpolated string: "
		if cursor.peek() == Some(STRING_BOUNDARY) {
			cursor.advance(); // consume "
			let mut value = String::new();
			let mut segment_has_left_quote = true;
			loop {
				if cursor.is_at_end() {
					let quoting = if segment_has_left_quote {
						StringQuoting::LeftQuoted
					} else {
						StringQuoting::RightQuoted
					};
					tokens.push(Token::String(value, quoting));
					tokens.push(Token::StringNotTerminated);
					break;
				}
				let c = cursor.peek().unwrap();
				if c == STRING_ESCAPE {
					cursor.advance();
					if let Some(esc) = cursor.advance() {
						value.push(match esc {
							'n' => '\n',
							't' => '\t',
							'r' => '\r',
							'0' => '\0',
							_ => esc,
						});
					}
					continue;
				}
				if c == STRING_BOUNDARY {
					cursor.advance();
					let quoting = if segment_has_left_quote {
						StringQuoting::FullyQuoted
					} else {
						StringQuoting::RightQuoted
					};
					tokens.push(Token::String(value, quoting));
					break;
				}
				if cursor.advance_over(STRING_INTERPOLATION_START) {
					let quoting = if segment_has_left_quote {
						StringQuoting::LeftQuoted
					} else {
						StringQuoting::Unquoted
					};
					tokens.push(Token::String(std::mem::take(&mut value), quoting));
					tokens.push(Token::StringInteropolationStart);
					segment_has_left_quote = false;
					// Lex expression content until matching BRACE_CLOSE
					let mut depth = 1u32;
					loop {
						if cursor.is_at_end() {
							tokens.push(Token::StringNotTerminated);
							break;
						}
						if cursor.peek() == Some(BRACE_CLOSE) && depth == 1 {
							cursor.advance();
							tokens.push(Token::StringInteropolationEnd);
							break;
						}
						if cursor.peek() == Some(BRACE_OPEN) {
							depth += 1;
							cursor.advance();
							tokens.push(Token::Brace(Mode::Open));
							continue;
						}
						if cursor.peek() == Some(BRACE_CLOSE) {
							depth -= 1;
							cursor.advance();
							tokens.push(Token::Brace(Mode::Close));
							continue;
						}
						if cursor.peek() == Some(STRING_BOUNDARY) {
							cursor.advance();
							while let Some(x) = cursor.peek() {
								if x == STRING_ESCAPE {
									cursor.advance();
									cursor.advance();
								} else if x == STRING_BOUNDARY {
									cursor.advance();
									break;
								} else {
									cursor.advance();
								}
							}
							continue;
						}
						match lex_one(&mut cursor) {
							Some(t) => tokens.push(t),
							None => break,
						}
					}
					continue;
				}
				value.push(cursor.advance().unwrap());
		}
		continue;
	}

		// Single-line comment: // ... to end of line
		if cursor.advance_over(LINE_COMMENT_START) {
			let rest = cursor.take_while(|c| c != '\n');
			let full = format!("{}{}", LINE_COMMENT_START, rest);
			tokens.push(Token::Comment(full));
			continue;
		}

		match lex_one(&mut cursor) {
			Some(t) => tokens.push(t),
			None => break,
		}
	}

	tokens
}

impl fmt::Display for Token {
	/// Writes the source text that produced this token, so the original code can be reconstructed by concatenating these.
	/// Implemented as Display to allow writing directly to a stream without allocating.
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Token::Pub => write!(f, "{}", PUB_TOKEN),
			Token::Fn => write!(f, "{}", FN_TOKEN),
			Token::Let => write!(f, "{}", LET_TOKEN),
			Token::Struct => write!(f, "{}", STRUCT_TOKEN),
			Token::Return => write!(f, "{}", RETURN_TOKEN),
			Token::Operator(op) => match op {
				Operator::Add => write!(f, "{}", OP_ADD),
				Operator::Sub => write!(f, "{}", OP_SUB),
				Operator::Mul => write!(f, "{}", OP_MUL),
				Operator::Div => write!(f, "{}", OP_DIV),
				Operator::Mod => write!(f, "{}", OP_MOD),
				Operator::Pow => write!(f, "{}", OP_POW),
			},
			Token::Paren(m) => match m {
				Mode::Open => write!(f, "{}", PAREN_OPEN),
				Mode::Close => write!(f, "{}", PAREN_CLOSE),
			},
			Token::Brace(m) => match m {
				Mode::Open => write!(f, "{}", BRACE_OPEN),
				Mode::Close => write!(f, "{}", BRACE_CLOSE),
			},
			Token::Comparison(c) => match c {
				ComparisonOperator::LessThan => write!(f, "{}", OP_LT),
				ComparisonOperator::LessThanOrEqual => write!(f, "{}", OP_LE),
				ComparisonOperator::GreaterThan => write!(f, "{}", OP_GT),
				ComparisonOperator::GreaterThanOrEqual => write!(f, "{}", OP_GE),
				ComparisonOperator::Equality => write!(f, "{}", OP_EQUALITY),
				ComparisonOperator::And => write!(f, "{}", OP_AND),
				ComparisonOperator::Or => write!(f, "{}", OP_OR),
			},
			Token::Equals => write!(f, "{}", EQUALS),
			Token::Semi => write!(f, "{}", SEMI),
			Token::Comma => write!(f, "{}", COMMA),
			Token::Colon => write!(f, "{}", COLON),
			Token::Arrow => write!(f, "{}", RETURN_TYPE_ARROW),
			Token::Ident(s) => write!(f, "{}", s),
			Token::Number(n) => write!(f, "{}", n),
			Token::String(s, quoting) => {
				use StringQuoting::*;
				if matches!(quoting, FullyQuoted | LeftQuoted) {
					write!(f, "{}", STRING_BOUNDARY)?;
				}
				for c in s.chars() {
					match c {
						'\\' => write!(f, "\\\\")?,
						'"' => write!(f, "\\\"")?,
						'\n' => write!(f, "\\n")?,
						'\t' => write!(f, "\\t")?,
						'\r' => write!(f, "\\r")?,
						'\0' => write!(f, "\\0")?,
						_ => write!(f, "{}", c)?,
					}
				}
				if matches!(quoting, FullyQuoted | RightQuoted) {
					write!(f, "{}", STRING_BOUNDARY)?;
				}
				Ok(())
			}
			Token::RawString(n, content) => {
				write!(f, "{}", RAW_STRING_NOTE)?;
				for _ in 0..*n as usize {
					write!(f, "{}", RAW_STRING_COUNTER)?;
				}
				write!(f, "{}", STRING_BOUNDARY)?;
				write!(f, "{}", content)?;
				write!(f, "{}", STRING_BOUNDARY)?;
				for _ in 0..*n as usize {
					write!(f, "{}", RAW_STRING_COUNTER)?;
				}
				Ok(())
			}
			Token::StringInteropolationStart => write!(f, "{}", STRING_INTERPOLATION_START),
			Token::StringInteropolationEnd => write!(f, "{}", BRACE_CLOSE),
			Token::Whitespace(s) => write!(f, "{}", s),
			Token::Comment(s) => write!(f, "{}", s),
			Token::Unknown(s) => write!(f, "{}", s),
			Token::StringNotTerminated => Ok(()),
		}
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	fn content_tokens(tokens: &[Token]) -> Vec<&Token> {
		tokens
			.iter()
			.filter(|t| !matches!(t, Token::Whitespace(_)))
			.collect()
	}

	#[test]
	fn empty_input() {
		let tokens = lex("");
		assert!(tokens.is_empty());
	}

	#[test]
	fn whitespace_only() {
		let tokens = lex("  \n\t ");
		assert_eq!(tokens.len(), 1);
		assert!(matches!(&tokens[0], Token::Whitespace(s) if s == "  \n\t "));
	}

	#[test]
	fn keywords() {
		let tokens = lex("pub fn let struct return");
		let c: Vec<_> = content_tokens(&tokens);
		assert_eq!(c, vec![&Token::Pub, &Token::Fn, &Token::Let, &Token::Struct, &Token::Return]);
	}

	#[test]
	fn identifiers() {
		let tokens = lex("foo bar_baz _x");
		let c: Vec<_> = content_tokens(&tokens);
		assert_eq!(
			c,
			vec![
				&Token::Ident("foo".into()),
				&Token::Ident("bar_baz".into()),
				&Token::Ident("_x".into()),
			]
		);
	}

	#[test]
	fn keyword_vs_ident() {
		let tokens = lex("fn myfn");
		let c: Vec<_> = content_tokens(&tokens);
		assert_eq!(c, vec![&Token::Fn, &Token::Ident("myfn".into())]);
	}

	#[test]
	fn numbers() {
		let tokens = lex("0 42 3.14 .5");
		let c: Vec<_> = content_tokens(&tokens);
		assert_eq!(c.len(), 4);
		assert!(matches!(&c[0], Token::Number(n) if *n == 0.0));
		assert!(matches!(&c[1], Token::Number(n) if *n == 42.0));
		assert!(matches!(&c[2], Token::Number(n) if (*n - 3.14).abs() < 1e-10));
		assert!(matches!(&c[3], Token::Number(n) if (*n - 0.5).abs() < 1e-10));
	}

	#[test]
	fn single_char_operators() {
		let tokens = lex("+ - * / % ^");
		let c: Vec<_> = content_tokens(&tokens);
		assert_eq!(
			c,
			vec![
				&Token::Operator(Operator::Add),
				&Token::Operator(Operator::Sub),
				&Token::Operator(Operator::Mul),
				&Token::Operator(Operator::Div),
				&Token::Operator(Operator::Mod),
				&Token::Operator(Operator::Pow),
			]
		);
	}

	#[test]
	fn multi_char_comparisons() {
		let tokens = lex(">= <= == && ||");
		let c: Vec<_> = content_tokens(&tokens);
		assert_eq!(
			c,
			vec![
				&Token::Comparison(ComparisonOperator::GreaterThanOrEqual),
				&Token::Comparison(ComparisonOperator::LessThanOrEqual),
				&Token::Comparison(ComparisonOperator::Equality),
				&Token::Comparison(ComparisonOperator::And),
				&Token::Comparison(ComparisonOperator::Or),
			]
		);
	}

	#[test]
	fn gt_lt() {
		let tokens = lex("> <");
		let c: Vec<_> = content_tokens(&tokens);
		assert_eq!(
			c,
			vec![
				&Token::Comparison(ComparisonOperator::GreaterThan),
				&Token::Comparison(ComparisonOperator::LessThan),
			]
		);
	}

	#[test]
	fn parens_and_braces() {
		let tokens = lex("( ) { }");
		let c: Vec<_> = content_tokens(&tokens);
		assert_eq!(
			c,
			vec![
				&Token::Paren(Mode::Open),
				&Token::Paren(Mode::Close),
				&Token::Brace(Mode::Open),
				&Token::Brace(Mode::Close),
			]
		);
	}

	#[test]
	fn equals_and_semi() {
		let tokens = lex("= ;");
		let c: Vec<_> = content_tokens(&tokens);
		assert_eq!(c, vec![&Token::Equals, &Token::Semi]);
	}

	#[test]
	fn simple_string() {
		let tokens = lex(r#""hello""#);
		let c: Vec<_> = content_tokens(&tokens);
		assert_eq!(c, vec![&Token::String("hello".into(), StringQuoting::FullyQuoted)]);
	}

	#[test]
	fn empty_string() {
		let tokens = lex(r#""""#);
		let c: Vec<_> = content_tokens(&tokens);
		assert_eq!(c, vec![&Token::String("".into(), StringQuoting::FullyQuoted)]);
	}

	#[test]
	fn string_with_escape() {
		let tokens = lex(r#""hi\n\t""#);
		let c: Vec<_> = content_tokens(&tokens);
		assert_eq!(c.len(), 1);
		assert!(matches!(&c[0], Token::String(s, _) if s.contains('\n') && s.contains('\t')));
	}

	#[test]
	fn raw_string_zero_hashes() {
		let tokens = lex(r#"r"raw""#);
		let c: Vec<_> = content_tokens(&tokens);
		assert_eq!(c, vec![&Token::RawString(0, "raw".into())]);
	}

	#[test]
	fn raw_string_with_hashes() {
		let tokens = lex(r##"r#"raw # content"#"##);
		let c: Vec<_> = content_tokens(&tokens);
		assert_eq!(c, vec![&Token::RawString(1, "raw # content".into())]);
	}

	#[test]
	fn string_interpolation() {
		let tokens = lex(r#""a ${ 1 + 2 } b""#);
		let c: Vec<_> = content_tokens(&tokens);
		assert!(c.len() >= 3);
		assert_eq!(c[0], &Token::String("a ".into(), StringQuoting::LeftQuoted));
		assert_eq!(c[1], &Token::StringInteropolationStart);
		// Interior: whitespace, 1, +, 2, whitespace, then StringInterpolationEnd
		assert!(
			c.contains(&&Token::StringInteropolationEnd),
			"expected StringInterpolationEnd in {:?}",
			c
		);
		let str_tokens: Vec<_> = c.iter().filter(|t| matches!(t, Token::String(_, _))).collect();
		assert!(str_tokens.len() >= 2, "expected at least 2 string tokens in {:?}", c);
		assert_eq!(
			**str_tokens[str_tokens.len() - 1],
			Token::String(" b".into(), StringQuoting::RightQuoted)
		);
	}

	#[test]
	fn unterminated_string() {
		let tokens = lex(r#""never closes"#);
		let c: Vec<_> = content_tokens(&tokens);
		assert_eq!(c.last(), Some(&&Token::StringNotTerminated));
		assert!(matches!(&c[0], Token::String(s, _) if s == "never closes"));
	}

	#[test]
	fn expression_like() {
		let tokens = lex("let x = 1 + 2 ;");
		let c: Vec<_> = content_tokens(&tokens);
		assert_eq!(c[0], &Token::Let);
		assert_eq!(c[1], &Token::Ident("x".into()));
		assert_eq!(c[2], &Token::Equals);
		assert!(matches!(&c[3], Token::Number(_)));
		assert_eq!(c[4], &Token::Operator(Operator::Add));
		assert!(matches!(&c[5], Token::Number(_)));
		assert_eq!(c[6], &Token::Semi);
	}

	#[test]
	fn line_comment_token() {
		let tokens = lex("let x = 1 ; // trailing comment");
		let c: Vec<_> = content_tokens(&tokens);
		assert_eq!(c.len(), 6);
		assert_eq!(c[0], &Token::Let);
		assert_eq!(c[4], &Token::Semi);
		assert_eq!(c[5], &Token::Comment("// trailing comment".into()));
	}

	#[test]
	fn line_comment_whole_line() {
		let tokens = lex("// comment only\nlet y = 2 ;");
		let c: Vec<_> = content_tokens(&tokens);
		assert_eq!(c[0], &Token::Comment("// comment only".into()));
		assert_eq!(c[1], &Token::Let);
		assert_eq!(c[2], &Token::Ident("y".into()));
		assert_eq!(c[3], &Token::Equals);
		assert!(matches!(&c[4], Token::Number(_)));
		assert_eq!(c[5], &Token::Semi);
	}

	#[test]
	fn slash_not_comment() {
		let tokens = lex("1 / 2");
		let c: Vec<_> = content_tokens(&tokens);
		assert_eq!(c.len(), 3);
		assert_eq!(c[1], &Token::Operator(Operator::Div));
	}

	#[test]
	fn return_type_arrow() {
		let tokens = lex("fn id(x: i32) -> i32");
		let c: Vec<_> = content_tokens(&tokens);
		assert!(c.contains(&&Token::Arrow));
		assert_eq!(c[7], &Token::Arrow);
	}
}
