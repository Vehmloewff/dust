use super::{Expression, parse_expression};
use rupert::{Diagnostic, InputStream, ParseResult, Span};

#[derive(Debug, Clone)]
pub struct StringLiteral {
	span: Span,
	value: String,
	interpolations: Vec<StringInterpolation>,
}

#[derive(Debug, Clone)]
pub struct StringInterpolation {
	index: usize,
	expression: Expression,
}

pub fn parse_string_literal(mut stream: InputStream) -> ParseResult<Expression> {
	let start_pin = stream.pin();
	let is_single_quote = stream.consume_char('\'').is_some();
	let is_double_quote = !is_single_quote && stream.consume_char('"').is_some();

	if !is_single_quote && !is_double_quote {
		return ParseResult::Reject(stream);
	}

	let mut interpolations = Vec::new();
	let mut is_escaped = false;
	let mut was_delimited = false;
	let mut string = String::new();
	let mut scheduled_diagnostic = None;

	loop {
		let next_character = match stream.peek() {
			Some(character) => character,
			None => break,
		};

		if !is_escaped && next_character == '\\' {
			is_escaped = true;
			continue;
		}

		if !is_escaped && is_double_quote && next_character == '"' {
			was_delimited = true;
			stream.pop();
			break;
		}

		if !is_escaped && is_single_quote && next_character == '\'' {
			was_delimited = true;
			stream.pop();
			break;
		}

		if !is_escaped && next_character == '{' {
			stream.pop();
			stream.consume_whitespace();

			let (new_stream, expression) = match parse_expression(stream) {
				ParseResult::Built(stream, expression) => (stream, expression),
				ParseResult::Reject(stream) => {
					scheduled_diagnostic = Some("Expected an expression".to_owned());

					(stream, Expression::Never)
				}
			};

			stream = new_stream;

			interpolations.push(StringInterpolation {
				index: string.len(),
				expression,
			});

			stream.consume_whitespace();
			let has_closing_bracket = stream.consume_char('}').is_some();

			if !has_closing_bracket {
				scheduled_diagnostic = Some("Expected a closing bracket '}'".to_owned())
			}

			continue;
		}

		if is_escaped {
			is_escaped = false;
		}

		string.push(next_character);
		stream.pop();

		if let Some(message) = scheduled_diagnostic.take() {
			stream.add_diagnostic(Diagnostic::new_error(
				Span::new(stream.get_index() - 1, stream.get_index()),
				message,
			));
		}
	}

	if !was_delimited {
		stream.add_diagnostic(Diagnostic::new_error(
			Span::new(stream.get_index() - 1, stream.get_index()),
			format!("Expected a closing quote ({})", if is_double_quote { '"' } else { '\'' }),
		))
	}

	let node = StringLiteral {
		span: start_pin.build(&stream),
		value: string,
		interpolations,
	};

	ParseResult::Built(stream, Expression::StringLiteral(node))
}
