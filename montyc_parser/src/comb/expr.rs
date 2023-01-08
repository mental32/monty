use montyc_ast::expr::{Expr, InfixOp, UnaryOp};
use montyc_ast::spanned::Spanned;
use nom::{branch::alt, IResult};

use crate::TokenStreamRef;
use montyc_lexer::PyToken;

use super::core::{expect, expect_any_of, expect_any_token, expect_many_n, expect_wrapped_values};

use super::primary::await_primary;
use super::whitespace;

#[inline]
fn power<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Expr>> {
    let (stream, left) = await_primary(stream)?;
    let left = left.replace_with(Expr::Primary);

    if let Ok((stream, _)) = expect_wrapped_values([PyToken::Star; 2], PyToken::Whitespace)(stream)
    {
        let (stream, right) = factor(stream)?;

        let left = Box::new(left);
        let right = Box::new(right);

        let obj = Spanned {
            span: left.span.start..right.span.end,
            inner: Expr::BinOp {
                left,
                op: InfixOp::Power,
                right,
            },
        };

        Ok((stream, obj))
    } else {
        Ok((stream, left))
    }
}

#[inline]
fn factor<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Expr>> {
    use PyToken::{Minus, Plus, Tilde};

    if let Ok((stream, token)) = expect_any_token([Plus, Minus, Tilde])(stream) {
        let (stream, value) = factor(stream)?;
        let (span, value) = (value.span.clone(), value);

        let op = match token.inner {
            Plus => UnaryOp::Add,
            Minus => UnaryOp::Sub,
            Tilde => UnaryOp::Invert,
            _ => unreachable!(),
        };

        let inner = Expr::Unary {
            op,
            value: Box::new(value),
        };

        let wrapped = Spanned {
            inner,
            span: token.span.start..span.end,
        };

        Ok((stream, wrapped))
    } else {
        power(stream)
    }
}

#[inline]
fn term<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Expr>> {
    #[inline]
    fn term_<'this, 'source, 'data>(
        stream: TokenStreamRef<'this, 'source, 'data>,
        left: &Spanned<Expr>,
    ) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Expr>> {
        use PyToken::{At, Div, Modulo, Star};

        let (stream, _) = whitespace(stream)?;
        let (stream, tok) = expect_any_token([Star, Div, Modulo, At])(stream)?;

        if let Ok((stream, _)) = expect(Div)(stream) {
            let (stream, _) = whitespace(stream)?;

            let (stream, right) = factor(stream)?;

            let right = Box::new(right);
            let left = Box::new(left.clone());

            let obj = Spanned {
                span: left.span.start..right.span.end,
                inner: Expr::BinOp {
                    left,
                    op: InfixOp::FloorDiv,
                    right,
                },
            };

            Ok((stream, obj))
        } else {
            let (stream, _) = whitespace(stream)?;

            let (stream, right) = factor(stream)?;
            let left = Box::new(left.clone());
            let span = left.span.start..right.span.end;
            let right = Box::new(right);

            let op = match tok.inner {
                At => InfixOp::MatMult,
                Modulo => InfixOp::Mod,
                Star => InfixOp::Mult,
                Div => InfixOp::Div,
                _ => unreachable!(),
            };

            let inner = Expr::BinOp { left, right, op };

            let obj = Spanned { span, inner };

            Ok((stream, obj))
        }
    }

    let (stream, _) = whitespace(stream)?;

    let (stream, base) = factor(stream)?;

    term_(stream, &base).or(Ok((stream, base)))
}

#[inline]
fn sum<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Expr>> {
    use PyToken::{Minus, Plus};

    #[inline]
    fn sum_<'this, 'source, 'data>(
        stream: TokenStreamRef<'this, 'source, 'data>,
        left: &Spanned<Expr>,
    ) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Expr>> {
        let (stream, _) = whitespace(stream)?;
        let (stream, tok) = expect_any_token([Plus, Minus])(stream)?;
        let (stream, _) = whitespace(stream)?;
        let (stream, value) = term(stream)?;

        let span = left.span.start..value.span.end;

        let left = Box::new(left.clone());
        let right = Box::new(value);

        let op = match tok.inner {
            Plus => InfixOp::Add,
            Minus => InfixOp::Sub,
            _ => unreachable!(),
        };

        let inner = Expr::BinOp { left, right, op };

        let obj = Spanned { span, inner };

        Ok((stream, obj))
    }

    let (stream, _) = whitespace(stream)?;
    let (mut stream, mut obj) = term(stream)?;

    while let Ok((s, o)) = sum_(stream, &obj) {
        stream = s;
        obj = o;
    }

    Ok((stream, obj))
}

#[inline]
fn shift_expr<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Expr>> {
    use PyToken::{GreaterThan, LessThan};

    let (stream, _) = whitespace(stream)?;
    let (stream, base) = sum(stream)?;

    if let Ok((stream, tok)) = expect_any_token([LessThan, GreaterThan])(stream) {
        let (stream, _) = whitespace(stream)?;
        let (stream, _) = expect(LessThan)(stream)?;

        let op = if matches!(tok.inner, LessThan) {
            InfixOp::LeftShift
        } else {
            InfixOp::RightShift
        };

        let (stream, value) = expression(stream)?;

        let span = base.span.start..value.span.end;

        let left = Box::new(base);
        let right = Box::new(value);

        let inner = Expr::BinOp { left, right, op };

        let obj = Spanned { span, inner };

        Ok((stream, obj))
    } else {
        Ok((stream, base))
    }
}

#[inline]
fn bitwise_and<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Expr>> {
    let (stream, _) = whitespace(stream)?;
    let (stream, base) = shift_expr(stream)?;
    let (stream, _) = whitespace(stream)?;

    if let Ok((stream, _)) = expect_any_token([PyToken::And])(stream) {
        let (stream, _) = whitespace(stream)?;

        let (stream, value) = bitwise_and(stream)?;

        let span = base.span.start..value.span.end;

        let left = Box::new(base);
        let right = Box::new(value);

        let inner = Expr::BinOp {
            left,
            right,
            op: InfixOp::And,
        };

        let obj = Spanned { span, inner };

        Ok((stream, obj))
    } else {
        Ok((stream, base))
    }
}

#[inline]
fn bitwise_xor<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Expr>> {
    let (stream, _) = whitespace(stream)?;
    let (stream, base) = bitwise_and(stream)?;
    let (stream, _) = whitespace(stream)?;

    if let Ok((stream, _)) = expect_any_token([PyToken::Caret])(stream) {
        let (stream, _) = whitespace(stream)?;

        let (stream, value) = bitwise_xor(stream)?;

        let span = base.span.start..value.span.end;

        let left = Box::new(base);
        let right = Box::new(value);

        let inner = Expr::BinOp {
            left,
            right,
            op: InfixOp::Xor,
        };

        let obj = Spanned { span, inner };

        Ok((stream, obj))
    } else {
        Ok((stream, base))
    }
}

#[inline]
fn bitwise_or<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Expr>> {
    let (stream, _) = whitespace(stream)?;
    let (stream, base) = bitwise_xor(stream)?;
    let (stream, _) = whitespace(stream)?;

    if let Ok((stream, _)) = expect_any_token([PyToken::Pipe])(stream) {
        let (stream, _) = whitespace(stream)?;

        let (stream, value) = bitwise_or(stream)?;

        let span = base.span.start..value.span.end;

        let left = Box::new(base);
        let right = Box::new(value);

        let inner = Expr::BinOp {
            left,
            right,
            op: InfixOp::Or,
        };

        let obj = Spanned { span, inner };

        Ok((stream, obj))
    } else {
        Ok((stream, base))
    }
}
#[inline]
fn equality<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Expr>> {
    use PyToken::{Bang, Equal, Whitespace};

    let (stream, _) = expect_many_n::<0>(Whitespace)(stream)?;
    let (stream, left) = bitwise_or(stream)?;

    let (stream, _) = expect_many_n::<0>(Whitespace)(stream)?;
    let (stream, token) = expect_any_of([Equal, Bang])(stream)?;
    let (stream, _) = expect(Equal)(stream)?;

    let op = match token.inner {
        Bang => InfixOp::NotEq,
        Equal => InfixOp::Eq,
        _ => unreachable!(),
    };

    let (stream, _) = expect_many_n::<0>(Whitespace)(stream)?;
    let (stream, right) = comparison(stream)?;

    let cmp = Spanned {
        span: left.span.start..right.span.end,
        inner: Expr::BinOp {
            left: Box::new(left),
            right: Box::new(right),
            op,
        },
    };

    Ok((stream, cmp))
}

#[inline]
fn comparison<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Expr>> {
    alt((equality, bitwise_or))(stream)
}

#[inline]
fn inversion<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Expr>> {
    let not_comparison = |stream| {
        let (stream, _) = whitespace(stream)?;
        let (stream, tok) = expect(PyToken::Not)(stream)?;
        let (stream, value) = inversion(stream)?;

        let obj = Spanned {
            span: tok.span.start..value.span.end,
            inner: Expr::Unary {
                value: Box::new(value),
                op: UnaryOp::Not,
            },
        };

        Ok((stream, obj))
    };

    alt((not_comparison, comparison))(stream)
}

#[inline]
fn conjunction<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Expr>> {
    let and_inversion = |stream| {
        let (stream, left) = inversion(stream)?;
        let (stream, _) = whitespace(stream)?;
        let (stream, _) = expect(PyToken::And)(stream)?;
        let (stream, _) = whitespace(stream)?;
        let (stream, right) = conjunction(stream)?;

        let left = Box::new(left);
        let right = Box::new(right);

        let obj = Spanned {
            span: left.span.start..right.span.end,
            inner: Expr::BinOp {
                left,
                right,
                op: InfixOp::And,
            },
        };

        Ok((stream, obj))
    };

    alt((and_inversion, inversion))(stream)
}

#[inline]
fn disjunction<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Expr>> {
    let or_conjunction = |stream| {
        let (stream, left) = conjunction(stream)?;
        let (stream, _) = whitespace(stream)?;
        let (stream, _) = expect(PyToken::Or)(stream)?;
        let (stream, _) = whitespace(stream)?;
        let (stream, right) = disjunction(stream)?;

        let left = Box::new(left);
        let right = Box::new(right);

        let obj = Spanned {
            span: left.span.start..right.span.end,
            inner: Expr::BinOp {
                left,
                right,
                op: InfixOp::Or,
            },
        };

        Ok((stream, obj))
    };

    alt((or_conjunction, conjunction))(stream)
}

#[inline]
pub fn expression_unspanned<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Expr> {
    let (stream, result) = expression(stream)?;
    Ok((stream, result.inner))
}

#[inline]
pub fn expression<'this, 'source, 'data>(
    stream: TokenStreamRef<'this, 'source, 'data>,
) -> IResult<TokenStreamRef<'this, 'source, 'data>, Spanned<Expr>> {
    let ternary_disjunction = |stream| {
        let (stream, body) = disjunction(stream)?;
        let (stream, _) = whitespace(stream)?;
        let (stream, _) = expect(PyToken::If)(stream)?;
        let (stream, _) = whitespace(stream)?;
        let (stream, test) = disjunction(stream)?;
        let (stream, _) = whitespace(stream)?;
        let (stream, _) = expect(PyToken::Else)(stream)?;
        let (stream, _) = whitespace(stream)?;
        let (stream, orelse) = expression(stream)?;

        let obj = Spanned {
            span: body.span.start..orelse.span.end,
            inner: Expr::If {
                test: Box::new(test),
                body: Box::new(body),
                orelse: Box::new(orelse),
            },
        };

        Ok((stream, obj))
    };

    let (stream, expr) = alt((ternary_disjunction, disjunction))(stream)?;

    let (stream, _) = whitespace(stream)?;

    Ok((stream, expr))
}
