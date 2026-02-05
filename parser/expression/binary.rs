use super::{parse_expression, Expression};
use rupert::{wrap, InputStream, ParseResult, Span, WrapResult};

#[derive(Debug, PartialEq, Clone)]
pub enum BinaryOperator {
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Modulus,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
    LogicalAnd,
    LogicalOr,
}

fn parse_binary_operator(
    mut stream: InputStream,
    operators: &[BinaryOperator],
) -> ParseResult<BinaryOperator> {
    if operators
        .contains(&BinaryOperator::Addition)
        .then(|| stream.consume_char('+').is_some())
        .unwrap_or(false)
    {
        ParseResult::Built(stream, BinaryOperator::Addition)
    } else if operators
        .contains(&BinaryOperator::Subtraction)
        .then(|| stream.consume_char('-').is_some())
        .unwrap_or(false)
    {
        ParseResult::Built(stream, BinaryOperator::Subtraction)
    } else if operators
        .contains(&BinaryOperator::Multiplication)
        .then(|| stream.consume_char('*').is_some())
        .unwrap_or(false)
    {
        ParseResult::Built(stream, BinaryOperator::Multiplication)
    } else if operators
        .contains(&BinaryOperator::Division)
        .then(|| stream.consume_char('/').is_some())
        .unwrap_or(false)
    {
        ParseResult::Built(stream, BinaryOperator::Division)
    } else if operators
        .contains(&BinaryOperator::Modulus)
        .then(|| stream.consume_char('%').is_some())
        .unwrap_or(false)
    {
        ParseResult::Built(stream, BinaryOperator::Modulus)
    } else if operators
        .contains(&BinaryOperator::GreaterThan)
        .then(|| stream.consume_char('>').is_some())
        .unwrap_or(false)
    {
        ParseResult::Built(stream, BinaryOperator::GreaterThan)
    } else if operators
        .contains(&BinaryOperator::GreaterThanOrEqual)
        .then(|| stream.consume_text(">=").is_some())
        .unwrap_or(false)
    {
        ParseResult::Built(stream, BinaryOperator::GreaterThanOrEqual)
    } else if operators
        .contains(&BinaryOperator::LessThan)
        .then(|| stream.consume_char('<').is_some())
        .unwrap_or(false)
    {
        ParseResult::Built(stream, BinaryOperator::LessThan)
    } else if operators
        .contains(&BinaryOperator::LessThanOrEqual)
        .then(|| stream.consume_text("<=").is_some())
        .unwrap_or(false)
    {
        ParseResult::Built(stream, BinaryOperator::LessThanOrEqual)
    } else if operators
        .contains(&BinaryOperator::LogicalAnd)
        .then(|| stream.consume_text("&&").is_some())
        .unwrap_or(false)
    {
        ParseResult::Built(stream, BinaryOperator::LogicalAnd)
    } else if operators
        .contains(&BinaryOperator::LogicalOr)
        .then(|| stream.consume_text("||").is_some())
        .unwrap_or(false)
    {
        ParseResult::Built(stream, BinaryOperator::LogicalOr)
    } else {
        ParseResult::Reject(stream)
    }
}

#[derive(Debug, Clone)]
pub struct BinaryExpression {
    span: Span,
    left: Box<Expression>,
    operator_span: Span,
    operator: BinaryOperator,
    right: Box<Expression>,
}

pub fn wrap_binary_expression(stream: InputStream, left: Expression) -> WrapResult<Expression> {
    wrap!(stream, left; wrap_multiplicative, wrap_additive, wrap_comparative, wrap_logical)
}

fn wrap_with_operators(
    stream: InputStream,
    left: Expression,
    operators: &[BinaryOperator],
) -> WrapResult<Expression> {
    let start_pin = stream.pin();

    let (mut stream, operator) = match parse_binary_operator(stream, operators) {
        ParseResult::Built(stream, operator) => (stream, operator),
        ParseResult::Reject(stream) => return WrapResult::Reject(stream, left),
    };

    let operator_span = start_pin.build(&stream);

    stream.consume_whitespace();

    let (stream, right) = match parse_expression(stream) {
        ParseResult::Built(stream, expression) => (stream, expression),
        ParseResult::Reject(mut stream) => {
            start_pin.register_error(&mut stream, "Expected a right-hand side expression");

            (stream, Expression::Never)
        }
    };

    let node = BinaryExpression {
        span: start_pin.build(&stream),
        operator_span,
        left: Box::new(left),
        operator,
        right: Box::new(right),
    };

    WrapResult::Built(stream, Expression::BinaryExpression(node))
}

fn wrap_multiplicative(stream: InputStream, left: Expression) -> WrapResult<Expression> {
    wrap_with_operators(
        stream,
        left,
        &[
            BinaryOperator::Multiplication,
            BinaryOperator::Division,
            BinaryOperator::Modulus,
        ],
    )
}

fn wrap_additive(stream: InputStream, left: Expression) -> WrapResult<Expression> {
    wrap_with_operators(
        stream,
        left,
        &[BinaryOperator::Addition, BinaryOperator::Subtraction],
    )
}

fn wrap_comparative(stream: InputStream, left: Expression) -> WrapResult<Expression> {
    wrap_with_operators(
        stream,
        left,
        &[
            BinaryOperator::LessThan,
            BinaryOperator::LessThanOrEqual,
            BinaryOperator::GreaterThan,
            BinaryOperator::GreaterThanOrEqual,
        ],
    )
}

fn wrap_logical(stream: InputStream, left: Expression) -> WrapResult<Expression> {
    wrap_with_operators(
        stream,
        left,
        &[BinaryOperator::LogicalAnd, BinaryOperator::LogicalOr],
    )
}
