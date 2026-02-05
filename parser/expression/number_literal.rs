use super::Expression;
use rupert::{InputStream, ParseResult, Span};

#[derive(Debug, Clone)]
pub struct NumberLiteral {
    span: Span,
    number: f64,
}

pub fn parse_number_literal(mut stream: InputStream) -> ParseResult<Expression> {
    let span_builder = stream.pin();
    let mut start = String::new();
    let mut has_decimal = false;

    loop {
        if let Some(digits) = stream.consume_digits() {
            start.push_str(digits.as_str())
        } else if !has_decimal && stream.consume_char('.').is_some() {
            has_decimal = true;
            start.push('.')
        } else {
            break;
        }
    }

    if start.is_empty() {
        ParseResult::Reject(stream)
    } else {
        let number = start.parse::<f64>().unwrap_or(0.0);
        let node = NumberLiteral {
            span: span_builder.build(&stream),
            number,
        };

        ParseResult::Built(stream, Expression::NumberLiteral(node))
    }
}
