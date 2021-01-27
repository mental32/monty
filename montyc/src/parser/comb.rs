//! Nom combinators used for parsing.

use super::ast::AstObject;
use super::*;

use ast::AstNode;
use nom::{
    branch::alt,
    combinator::not,
    multi::{many0, many1, many_m_n},
    sequence::{preceded, terminated, tuple},
    IResult,
};

use nom::{
    error::{Error, ErrorKind},
    Err,
};

#[cfg(test)]
mod test;

// -- helper combinators

#[inline]
fn expect_many_n<const N: usize>(
    value: PyToken,
) -> impl for<'a> Fn(TokenSlice<'a>) -> IResult<TokenSlice<'a>, Vec<AstObject>> {
    move |stream| match N {
        0 => many0(expect_(value))(stream),
        1 => many1(expect_(value))(stream),
        m => many_m_n(m, m.saturating_add(1), expect_(value))(stream),
    }
}

#[inline]
fn expect_(
    value: PyToken,
) -> impl for<'a> Fn(TokenSlice<'a>) -> IResult<TokenSlice<'a>, AstObject> {
    move |stream: TokenSlice<'_>| {
        let (stream, (_, o)) = expect(stream, value)?;
        Ok((stream, o))
    }
}

#[inline]
fn expect_token(
    value: PyToken,
) -> impl for<'a> Fn(TokenSlice<'a>) -> IResult<TokenSlice<'a>, (PyToken, AstObject)> {
    move |stream: TokenSlice<'_>| expect(stream, value)
}

#[inline]
fn expect_any_token<const N: usize>(
    values: [PyToken; N],
) -> impl for<'a> Fn(TokenSlice<'a>) -> IResult<TokenSlice<'a>, (PyToken, AstObject)> {
    move |stream: TokenSlice<'_>| {
        for token in values.iter() {
            match expect(stream, *token) {
                Ok(r) => return Ok(r),
                Err(_) => continue,
            }
        }

        Err(Err::Error(Error::new(stream, ErrorKind::Alt)))
    }
}

#[inline]
fn expect_with<'a, F>(
    stream: TokenSlice<'a>,
    predicate: F,
) -> IResult<TokenSlice<'a>, (PyToken, AstObject)>
where
    F: Fn(Token) -> bool,
{
    match stream {
        [(token, span), ..] => {
            if predicate((*token, span.clone())) {
                let rest = &stream[1..];
                let span = span.clone();

                let obj = AstObject {
                    span,
                    inner: AstNode::from(*token),
                };

                Ok((rest, (*token, obj)))
            } else {
                let err = Error::new(stream, ErrorKind::IsNot);
                Err(Err::Error(err))
            }
        }

        [] => Err(Err::Error(Error::new(stream, ErrorKind::Eof))),
    }
}

#[inline]
fn expect<'a>(
    stream: TokenSlice<'a>,
    value: PyToken,
) -> IResult<TokenSlice<'a>, (PyToken, AstObject)> {
    expect_with(stream, move |(t, _)| t == value)
}

fn expect_spanref<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, (PyToken, AstObject)> {
    expect_with(stream, move |(t, _)| matches!(t, PyToken::SpanRef(_)))
}

fn expect_ident<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, (PyToken, AstObject)> {
    expect_with(stream, move |(t, _)| matches!(t, PyToken::Ident(_)))
}

fn expect_digits<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, (PyToken, AstObject)> {
    expect_with(stream, move |(t, _)| matches!(t, PyToken::Digits(_)))
}

fn expect_wrapped<'a>(
    parser: impl Fn(TokenSlice<'a>) -> IResult<TokenSlice<'a>, AstObject>,
    wrapper: PyToken,
) -> impl Fn(TokenSlice<'a>) -> IResult<TokenSlice<'a>, AstObject> {
    move |stream| {
        let (stream, _) = expect_many_n::<0>(wrapper)(stream)?;
        let (stream, ast_object) = parser(stream)?;
        let (stream, _) = expect_many_n::<0>(wrapper)(stream)?;

        Ok((stream, ast_object))
    }
}

fn expect_wrapped_values<const N: usize>(
    values: [PyToken; N],
    wrapper: PyToken,
) -> impl for<'a> Fn(TokenSlice<'a>) -> IResult<TokenSlice<'a>, Vec<(PyToken, AstObject)>> {
    if N == 0 {
        panic!("Must proved a value array that contains at least one token, not zero.")
    }

    move |stream| {
        let (mut stream, _) = expect_many_n::<0>(wrapper)(stream)?;

        let mut results = Vec::with_capacity(N);

        for token in values.iter() {
            let (s, obj) = expect(stream, *token)?;
            stream = s;
            results.push(obj)
        }

        let (stream, _) = expect_many_n::<0>(wrapper)(stream)?;

        Ok((stream, results))
    }
}

#[inline]
fn parse_single_binop<'a, F>(
    stream: TokenSlice<'a>,
    parser: fn(TokenSlice<'a>) -> IResult<TokenSlice<'a>, AstObject>,
    ctor: F,
    tok: PyToken,
) -> IResult<TokenSlice<'a>, AstObject>
where
    F: FnOnce(Box<AstObject>, Box<AstObject>) -> AstNode,
{
    let (stream, _) = expect_many_n::<0>(PyToken::Whitespace)(stream)?;
    let (stream, base) = parser(stream)?;

    if let Ok((stream, _)) = expect(stream, tok) {
        let (stream, _) = expect_many_n::<0>(PyToken::Whitespace)(stream)?;
        let (stream, value) = parser(stream)?;

        let span = base.span.start..value.span.end;

        let left = Box::new(base);
        let right = Box::new(value);

        let obj = AstObject {
            span,
            inner: ctor(left, right),
        };

        Ok((stream, obj))
    } else {
        Ok((stream, base))
    }
}

#[inline]
fn parse_unary<'a, P, F>(
    stream: TokenSlice<'a>,
    prefix: P,
    parser: fn(TokenSlice<'a>) -> IResult<TokenSlice<'a>, AstObject>,
    ctor: F,
) -> IResult<TokenSlice<'a>, AstObject>
where
    P: FnOnce(TokenSlice<'a>) -> IResult<TokenSlice<'a>, AstObject>,
    F: FnOnce(Box<AstObject>) -> AstNode,
{
    let (stream, first) = prefix(stream)?;
    let (stream, value) = parser(stream)?;

    let span = first.span.start..value.span.end;
    let inner = ctor(Box::new(value));

    let obj = AstObject { span, inner };

    Ok((stream, obj))
}

// -- parser combinators

#[inline]
fn float<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, (PyToken, AstObject)> {
    let (stream, (l_tok, left)) = expect_digits(stream)?;
    let (stream, _) = expect(stream, PyToken::Dot)?;
    let (stream, (r_tok, right)) = expect_digits(stream)?;

    let value: f64 = if let (PyToken::Digits(a), PyToken::Digits(b)) = (l_tok, r_tok) {
        format!("{}.{}", a, b)
            .parse()
            .expect("Unable to parse float literal.")
    } else {
        unreachable!()
    };

    let obj = AstObject {
        span: left.span.start..right.span.end,
        inner: AstNode::Constant(ast::Constant::Float(value)),
    };

    Ok((stream, (PyToken::Invalid, obj)))
}

#[inline]
fn atom<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, AstObject> {
    let (stream, (_, obj)) = alt((
        expect_ident,
        expect_token(PyToken::True),
        expect_token(PyToken::False),
        expect_token(PyToken::None),
        expect_spanref,
        float,
        expect_digits,
        expect_token(PyToken::Ellipsis),
    ))(stream)?;

    Ok((stream, obj))
}

#[inline]
fn primary<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, AstObject> {
    #[inline]
    fn primary_subscript<'a>(
        stream: TokenSlice<'a>,
        base: &AstObject,
    ) -> IResult<TokenSlice<'a>, AstObject> {
        let (stream, _) = expect(stream, PyToken::LBracket)?;
        let (stream, _) = expect_many_n::<0>(PyToken::Whitespace)(stream)?;
        let (stream, (_, rbracket)) = expect(stream, PyToken::RBracket)?;

        let obj = AstObject {
            span: base.span.start..rbracket.span.end,
            inner: AstNode::Subscript {
                value: Box::new(base.clone()),
            },
        };

        Ok((stream, obj))
    }

    #[inline]
    fn primary_call<'a>(
        stream: TokenSlice<'a>,
        base: &AstObject,
    ) -> IResult<TokenSlice<'a>, AstObject> {
        let (stream, _) = expect(stream, PyToken::LParen)?;
        let (stream, _) = expect_many_n::<0>(PyToken::Whitespace)(stream)?;
        let (stream, (_, rparen)) = expect(stream, PyToken::RParen)?;

        let obj = AstObject {
            span: base.span.start..rparen.span.end,
            inner: AstNode::Call {
                func: Box::new(base.clone()),
            },
        };

        Ok((stream, obj))
    }

    #[inline]
    fn primary_dot_name<'a>(
        stream: TokenSlice<'a>,
        base: &AstObject,
    ) -> IResult<TokenSlice<'a>, AstObject> {
        let (stream, _) = expect_many_n::<0>(PyToken::Whitespace)(stream)?;
        let (stream, _) = expect(stream, PyToken::Dot)?;
        let (stream, _) = expect_many_n::<0>(PyToken::Whitespace)(stream)?;
        let (stream, (_, ident)) = expect_ident(stream)?;
        let (stream, _) = expect_many_n::<0>(PyToken::Whitespace)(stream)?;

        let attr = if let AstNode::RawName(n) = ident.inner {
            n
        } else {
            unreachable!()
        };

        let obj = AstObject {
            span: base.span.start..ident.span.end,
            inner: AstNode::Attribute {
                value: Box::new(base.clone()),
                attr,
            },
        };

        Ok((stream, obj))
    }

    #[inline]
    fn primary_left_recurse<'a>(
        stream: TokenSlice<'a>,
        base: &AstObject,
    ) -> IResult<TokenSlice<'a>, AstObject> {
        // Try each of the subrules until success or bail with a nom error.
        let mut output = primary_dot_name(stream, &base)
            .or_else(|_| primary_call(stream, &base))
            .or_else(|_| primary_subscript(stream, &base))
            .map_err(|_| Err::Error(Error::new(stream, ErrorKind::IsNot)))?;

        // recurse here and parse as much as we can for a primary.
        while let Ok((stream, object)) = primary_left_recurse(output.0, &output.1) {
            output = (stream, object);
        }

        Ok(output)
    }

    let (stream, base) = atom(stream)?;

    primary_left_recurse(stream, &base).or(Ok((stream, base)))
}

#[inline]
fn await_primary<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, AstObject> {
    let mut await_ = terminated(
        expect_(PyToken::Await),
        expect_many_n::<1>(PyToken::Whitespace),
    );

    if let Ok((stream, await_node)) = await_(stream) {
        let (stream, value) = primary(stream)?;

        let obj = AstObject {
            span: await_node.span.start..value.span.end,
            inner: AstNode::Await(Box::new(value)),
        };

        Ok((stream, obj))
    } else {
        primary(stream)
    }
}

#[inline]
fn power<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, AstObject> {
    let (stream, left) = await_primary(stream)?;

    if let Ok((stream, _)) = expect_wrapped_values([PyToken::Star; 2], PyToken::Whitespace)(stream)
    {
        let (stream, right) = factor(stream)?;

        let obj = AstObject {
            span: left.span.start..right.span.end,
            inner: AstNode::Power {
                left: Box::new(left),
                right: Box::new(right),
            },
        };

        Ok((stream, obj))
    } else {
        Ok((stream, left))
    }
}

#[inline]
fn factor<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, AstObject> {
    use PyToken::{Minus, Plus, Tilde};

    if let Ok((stream, (token, object))) = expect_any_token([Plus, Minus, Tilde])(stream) {
        let (stream, value) = factor(stream)?;
        let (span, value) = (value.span.clone(), Box::new(value));

        let inner = match token {
            Plus => AstNode::UAdd(value),
            Minus => AstNode::USub(value),
            Tilde => AstNode::UInvert(value),
            _ => unreachable!(),
        };

        let wrapped = AstObject {
            inner,
            span: object.span.start..span.end,
        };

        Ok((stream, wrapped))
    } else {
        power(stream)
    }
}

#[inline]
fn term<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, AstObject> {
    #[inline]
    fn term_<'a>(stream: TokenSlice<'a>, left: &AstObject) -> IResult<TokenSlice<'a>, AstObject> {
        use PyToken::{At, Div, Modulo, Star};

        let (stream, _) = expect_many_n::<0>(PyToken::Whitespace)(stream)?;
        let (stream, (tok, _)) = expect_any_token([Star, Div, Modulo, At])(stream)?;

        if let Ok((stream, _)) = expect_(Div)(stream) {
            let (stream, _) = expect_many_n::<0>(PyToken::Whitespace)(stream)?;

            let (stream, right) = factor(stream)?;

            let right = Box::new(right);
            let left = Box::new(left.clone());

            let obj = AstObject {
                span: left.span.start..right.span.end,
                inner: AstNode::FloorDiv { left, right },
            };

            Ok((stream, obj))
        } else {
            let (stream, _) = expect_many_n::<0>(PyToken::Whitespace)(stream)?;

            let (stream, right) = factor(stream)?;
            let left = Box::new(left.clone());
            let span = left.span.start..right.span.end;
            let right = Box::new(right);

            let inner = match tok {
                At => AstNode::MatMult { left, right },
                Modulo => AstNode::Mod { left, right },
                Star => AstNode::Mult { left, right },
                Div => AstNode::Div { left, right },
                _ => unreachable!(),
            };

            let obj = AstObject { span, inner };

            Ok((stream, obj))
        }
    }

    let (stream, _) = expect_many_n::<0>(PyToken::Whitespace)(stream)?;

    let (stream, base) = factor(stream)?;

    term_(stream, &base).or(Ok((stream, base)))
}

#[inline]
fn sum<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, AstObject> {
    use PyToken::{Minus, Plus};

    let (stream, _) = expect_many_n::<0>(PyToken::Whitespace)(stream)?;
    let (stream, base) = term(stream)?;

    if let Ok((stream, (tok, _))) = expect_any_token([Plus, Minus])(stream) {
        let (stream, _) = expect_many_n::<0>(PyToken::Whitespace)(stream)?;
        let (stream, value) = term(stream)?;

        let span = base.span.start..value.span.end;

        let left = Box::new(base);
        let right = Box::new(value);

        let inner = match tok {
            Plus => AstNode::Add { left, right },
            Minus => AstNode::Sub { left, right },
            _ => unreachable!(),
        };

        let obj = AstObject { span, inner };

        Ok((stream, obj))
    } else {
        Ok((stream, base))
    }
}

#[inline]
fn shift_expr<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, AstObject> {
    use PyToken::{GreaterThan, LessThan};

    let (stream, _) = expect_many_n::<0>(PyToken::Whitespace)(stream)?;
    let (stream, base) = sum(stream)?;

    if let Ok((stream, (tok, _))) = expect_any_token([LessThan, GreaterThan])(stream) {
        let (stream, _) = expect_many_n::<0>(PyToken::Whitespace)(stream)?;
        let (stream, _) = expect(stream, LessThan)?;

        let is_lt = matches!(tok, LessThan);

        let (stream, value) = sum(stream)?;

        let span = base.span.start..value.span.end;

        let left = Box::new(base);
        let right = Box::new(value);

        let inner = if is_lt {
            AstNode::LShift { left, right }
        } else {
            AstNode::RShift { left, right }
        };

        let obj = AstObject { span, inner };

        Ok((stream, obj))
    } else {
        Ok((stream, base))
    }
}

#[inline]
fn bitwise_and<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, AstObject> {
    parse_single_binop(
        stream,
        shift_expr,
        |left, right| AstNode::And { left, right },
        PyToken::And,
    )
}

#[inline]
fn bitwise_xor<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, AstObject> {
    parse_single_binop(
        stream,
        bitwise_and,
        |left, right| AstNode::Xor { left, right },
        PyToken::Caret,
    )
}

#[inline]
fn bitwise_or<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, AstObject> {
    parse_single_binop(
        stream,
        bitwise_xor,
        |left, right| AstNode::Or { left, right },
        PyToken::Pipe,
    )
}

#[inline]
fn is_bitwise_or<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, AstObject> {
    parse_unary(stream, expect_(PyToken::Is), bitwise_or, |value| {
        AstNode::Is(value)
    })
}

#[inline]
fn is_not_bitwise_or<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, AstObject> {
    parse_unary(
        stream,
        terminated(expect_(PyToken::Is), expect_(PyToken::Not)),
        bitwise_or,
        |value| AstNode::IsNot(value),
    )
}

#[inline]
fn in_bitwise_or<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, AstObject> {
    parse_unary(stream, expect_(PyToken::In), bitwise_or, |value| {
        AstNode::In(value)
    })
}

#[inline]
fn not_in_bitwise_or<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, AstObject> {
    parse_unary(
        stream,
        terminated(expect_(PyToken::Not), expect_(PyToken::In)),
        bitwise_or,
        |value| AstNode::NotIn(value),
    )
}

#[inline]
fn gt_bitwise_or<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, AstObject> {
    parse_unary(stream, expect_(PyToken::GreaterThan), bitwise_or, |value| {
        AstNode::Gt(value)
    })
}

#[inline]
fn gte_bitwise_or<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, AstObject> {
    parse_unary(
        stream,
        terminated(expect_(PyToken::GreaterThan), expect_(PyToken::Equal)),
        gt_bitwise_or,
        |value| AstNode::Gte(value),
    )
}

#[inline]
fn lt_bitwise_or<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, AstObject> {
    parse_unary(stream, expect_(PyToken::LessThan), bitwise_or, |value| {
        AstNode::Lt(value)
    })
}

#[inline]
fn lte_bitwise_or<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, AstObject> {
    parse_unary(
        stream,
        terminated(expect_(PyToken::LessThan), expect_(PyToken::Equal)),
        bitwise_or,
        |value| AstNode::Lte(value),
    )
}

#[inline]
fn noteq_bitwise_or<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, AstObject> {
    parse_unary(
        stream,
        terminated(expect_(PyToken::Bang), expect_(PyToken::Equal)),
        bitwise_or,
        |value| AstNode::NotEq(value),
    )
}

#[inline]
fn eq_bitwise_or<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, AstObject> {
    parse_unary(
        stream,
        terminated(expect_(PyToken::Equal), expect_(PyToken::Equal)),
        bitwise_or,
        |value| AstNode::Eq(value),
    )
}

#[inline]
fn compare_op_bitwise_or_pair<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, AstObject> {
    alt((
        eq_bitwise_or,
        noteq_bitwise_or,
        lte_bitwise_or,
        lt_bitwise_or,
        gte_bitwise_or,
        not_in_bitwise_or,
        in_bitwise_or,
        not_in_bitwise_or,
        is_bitwise_or,
        is_not_bitwise_or,
    ))(stream)
}

#[inline]
fn comparison<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, AstObject> {
    alt((
        terminated(bitwise_or, many1(compare_op_bitwise_or_pair)),
        bitwise_or,
    ))(stream)
}

#[inline]
fn inversion<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, AstObject> {
    alt((terminated(expect_(PyToken::Not), inversion), comparison))(stream)
}

#[inline]
fn conjunction<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, AstObject> {
    alt((
        terminated(
            inversion,
            many1(terminated(expect_(PyToken::And), inversion)),
        ),
        inversion,
    ))(stream)
}

#[inline]
fn disjunction<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, AstObject> {
    alt((
        terminated(
            conjunction,
            many1(terminated(expect_(PyToken::Or), conjunction)),
        ),
        conjunction,
    ))(stream)
}

#[inline]
pub(crate) fn expression<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, AstObject> {
    alt((
        terminated(
            disjunction,
            terminated(
                expect_(PyToken::If),
                terminated(disjunction, terminated(expect_(PyToken::Else), expression)),
            ),
        ),
        disjunction,
    ))(stream)
}

// -- Statement parser

#[inline]
fn return_stmt<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, AstObject> {
    let (stream, (ret, value)) = tuple((expect_(PyToken::Return), expression))(stream)?;

    let span = ret.span.start..value.span.end;
    let inner = AstNode::Return(Box::new(value));

    let obj = AstObject { span, inner };

    Ok((stream, obj))
}

#[inline]
fn assignment<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, AstObject> {
    let (stream, (_, name)) =
        terminated(expect_ident, expect_many_n::<0>(PyToken::Whitespace))(stream)?;

    let kind = if let Ok((stream, _)) = terminated(
        expect_(PyToken::Colon),
        expect_many_n::<0>(PyToken::Whitespace),
    )(stream)
    {
        let (stream, (_, kind)) =
            terminated(expect_ident, expect_many_n::<0>(PyToken::Whitespace))(stream)?;

        Some(Box::new(kind))
    } else {
        None
    };

    let (stream, _) = terminated(
        expect_(PyToken::Equal),
        expect_many_n::<0>(PyToken::Whitespace),
    )(stream)?;

    let (stream, value) = expression(stream)?;

    let span = name.span.start..value.span.end;

    let name = Box::new(name);
    let value = Box::new(value);

    let obj = AstObject {
        span,
        inner: AstNode::Assign { name, value, kind },
    };

    Ok((stream, obj))
}

#[inline]
fn small_stmt<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, AstObject> {
    alt((assignment, return_stmt, expect_(PyToken::Pass)))(stream)
}

#[inline]
fn block<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, ast::Block> {
    #[inline]
    fn invalid_block<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, AstObject> {
        terminated(expect_(PyToken::Newline), not(expect_(PyToken::Whitespace)))(stream)
    }

    let mut indented_block = preceded(
        expect_(PyToken::Newline),
        preceded(
            expect_(PyToken::Whitespace),
            terminated(statement, expect_(PyToken::Newline)),
        ),
    );

    if let Ok((stream, stmt)) = indented_block(stream) {
        return Ok((stream, vec![stmt]));
    } else if let Ok((stream, stmt)) = small_stmt(stream) {
        return Ok((stream, vec![stmt]));
    } else if let Ok((_, _)) = invalid_block(stream) {
        todo!("raise an indentation error here.");
    } else {
        unreachable!()
    }
}

#[inline]
fn else_block<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, ast::Block> {
    let (stream, _) = expect_many_n::<0>(PyToken::Newline)(stream)?;
    let (stream, _) = expect_many_n::<0>(PyToken::Whitespace)(stream)?;

    preceded(
        terminated(expect_(PyToken::Else), expect_(PyToken::Colon)),
        block,
    )(stream)
}

#[inline]
fn elif_stmt<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, AstObject> {
    let (mut stream, (_, tok, _, test, _, _, body, _)) = tuple((
        expect_many_n::<0>(PyToken::Newline),
        // expect_many_n::<0>(PyToken::Whitespace),
        expect_(PyToken::Elif),
        expect_many_n::<1>(PyToken::Whitespace),
        expression,
        expect_many_n::<0>(PyToken::Whitespace),
        expect_(PyToken::Colon),
        expect_many_n::<0>(PyToken::Whitespace),
        block,
    ))(stream)?;

    let mut if_node = ast::IfStmt {
        test: Box::new(test),
        body,
        orelse: None,
    };

    if let Ok((s, stmt)) = elif_stmt(stream) {
        stream = s;
        if_node.orelse = Some(vec![stmt]);
    } else if let Ok((s, stmt)) = else_block(stream) {
        stream = s;
        if_node.orelse = Some(stmt);
    }

    let if_obj = AstObject {
        span: tok.span,
        inner: AstNode::If(Box::new(if_node)),
    };

    return Ok((stream, if_obj));
}

#[inline]
fn if_stmt<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, AstObject> {
    let (mut stream, (tok, test, _, body)) = tuple((
        expect_(PyToken::If),
        expression,
        expect_(PyToken::Colon),
        block,
    ))(stream)?;

    let mut if_node = ast::IfStmt {
        test: Box::new(test),
        body,
        orelse: None,
    };

    if let Ok((s, stmt)) = elif_stmt(stream) {
        stream = s;
        if_node.orelse = Some(vec![stmt]);
    } else if let Ok((s, stmt)) = else_block(stream) {
        stream = s;
        if_node.orelse = Some(stmt);
    }

    let if_obj = AstObject {
        span: tok.span,
        inner: AstNode::If(Box::new(if_node)),
    };

    return Ok((stream, if_obj));
}

#[inline]
fn function_def<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, AstObject> {
    let (stream, (_, def)) = expect(stream, PyToken::FnDef)?;
    let (stream, _) = expect_many_n::<0>(PyToken::Whitespace)(stream)?;
    let (stream, _ident) = expect_ident(stream)?;
    let (stream, _) = expect_many_n::<0>(PyToken::Whitespace)(stream)?;
    let (stream, _) = expect(stream, PyToken::LParen)?;
    let (stream, _) = expect_many_n::<0>(PyToken::Whitespace)(stream)?;
    let (stream, _) = expect(stream, PyToken::RParen)?;
    let (stream, _) = expect_many_n::<0>(PyToken::Whitespace)(stream)?;
    let (stream, _) = expect(stream, PyToken::Colon)?;
    let (stream, _) = expect_many_n::<0>(PyToken::Whitespace)(stream)?;
    let (stream, stmt) = block(stream)?;

    let def_span_end = def.span.end;
    let span_end = stmt
        .iter()
        .last()
        .map(|o| o.span.end)
        .unwrap_or(def_span_end);

    let obj = AstObject {
        span: def_span_end..span_end,
        inner: AstNode::BareToken(PyToken::FnDef),
    };

    Ok((stream, obj))
}

#[inline]
fn compound_stmt<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, AstObject> {
    alt((
        function_def,
        if_stmt,
        // class_def,
        // with_stmt,
        // for_stmt,
        // try_stmt,
        // while_stmt,
    ))(stream)
}

#[inline]
fn simple_stmt<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, AstObject> {
    let (stream, block) = alt((
        many1(preceded(small_stmt, expect_(PyToken::Newline))),
        |stream| small_stmt(stream).map(|(stream, r)| (stream, vec![r])),
    ))(stream)?;

    let obj = AstObject {
        span: block.iter().next().map(|o| o.span.start).unwrap_or(0)
            ..block.iter().last().map(|o| o.span.end).unwrap_or(0),
        inner: AstNode::Block(block),
    };

    Ok((stream, obj))
}

#[inline]
pub(crate) fn statement<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, AstObject> {
    alt((simple_stmt, compound_stmt))(stream)
}

#[inline]
fn statements<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, AstObject> {
    let (stream, block) = many1(statement)(stream)?;

    let obj = AstObject {
        span: Default::default(),
        inner: AstNode::Block(block),
    };

    Ok((stream, obj))
}
