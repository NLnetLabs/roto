use std::num::ParseIntError;

use crate::{
    ast::{
        BinOp, Block, Expr, Identifier, IpAddress, Literal, Match, MatchArm,
        Prefix, PrefixLengthRange, PrefixMatchExpr, PrefixMatchType,
    },
    parser::ParseError,
};

use super::{
    span::{Spanned, WithSpan},
    token::Token,
    ParseResult, Parser,
};

/// Contextual restrictions on the expression parsing
///
/// This is used to resolve ambiguities in the grammar.
#[derive(Clone, Copy)]
struct Restrictions {
    forbid_records: bool,
}

/// # Parsing value expressions
impl<'source> Parser<'source> {
    pub fn block(&mut self) -> ParseResult<Block> {
        self.take(Token::CurlyLeft)?;

        let mut exprs = Vec::new();
        while !self.next_is(Token::CurlyRight) {
            exprs.push(self.expr()?);
            self.take(Token::SemiColon)?;
        }

        Ok(Block { exprs })
    }

    pub fn expr(&mut self) -> ParseResult<Spanned<Expr>> {
        self.expr_inner(Restrictions {
            forbid_records: false,
        })
    }

    fn expr_no_records(&mut self) -> ParseResult<Spanned<Expr>> {
        self.expr_inner(Restrictions {
            forbid_records: true,
        })
    }

    fn expr_inner(&mut self, r: Restrictions) -> ParseResult<Spanned<Expr>> {
        self.logical_expr(r)
    }

    fn logical_expr(
        &mut self,
        r: Restrictions,
    ) -> ParseResult<Spanned<Expr>> {
        let expr = self.comparison(r)?;

        if self.peek_is(Token::AmpAmp) {
            let mut exprs = vec![expr];
            while self.next_is(Token::AmpAmp) {
                exprs.push(self.comparison(r)?);
            }
            if self.peek_is(Token::PipePipe) {
                let pipe = self.take(Token::PipePipe)?;
                return Err(ParseError::custom(
                    "`||` cannot be chained with `&&`",
                    "cannot be chained with `&&`",
                    pipe,
                ));
            }
            Ok(exprs
                .into_iter()
                .rev()
                .reduce(|acc, e| {
                    let span = e.span.merge(acc.span);
                    Expr::BinOp(Box::new(e), BinOp::And, Box::new(acc))
                        .with_span(span)
                })
                .unwrap())
        } else if self.peek_is(Token::PipePipe) {
            let mut exprs = vec![expr];
            while self.next_is(Token::PipePipe) {
                exprs.push(self.comparison(r)?);
            }
            if self.peek_is(Token::AmpAmp) {
                let amp = self.take(Token::AmpAmp)?;
                return Err(ParseError::custom(
                    "`&&` cannot be chained with `||`",
                    "cannot be chained with `||`",
                    amp,
                ));
            }
            Ok(exprs
                .into_iter()
                .rev()
                .reduce(|acc, e| {
                    let span = e.span.merge(acc.span);
                    Expr::BinOp(Box::new(e), BinOp::Or, Box::new(acc))
                        .with_span(span)
                })
                .unwrap())
        } else {
            Ok(expr)
        }
    }

    fn comparison(&mut self, r: Restrictions) -> ParseResult<Spanned<Expr>> {
        let expr = self.access(r)?;

        if let Some(op) = self.try_compare_operator()? {
            let right = self.access(r)?;
            let span = expr.span.merge(right.span);
            Ok(Expr::BinOp(Box::new(expr), op.inner, Box::new(right))
                .with_span(span))
        } else {
            Ok(expr)
        }
    }

    /// Optionally parse a compare operator
    ///
    /// This method returns [`Option`], because we are never sure that there is
    /// going to be a comparison operator. A span is included with the operator
    /// to allow error messages to be attached to the parsed operator.
    ///
    /// ```ebnf
    /// CompareOp ::= '==' | '!=' | '<' | '<=' | '>' | '>=' | 'not'? 'in'
    /// ```
    fn try_compare_operator(
        &mut self,
    ) -> ParseResult<Option<Spanned<BinOp>>> {
        let Some(tok) = self.peek() else {
            return Ok(None);
        };

        let op = match tok {
            Token::EqEq => BinOp::Eq,
            Token::BangEq => BinOp::Ne,
            Token::AngleLeft => BinOp::Lt,
            Token::AngleRight => BinOp::Gt,
            Token::AngleLeftEq => BinOp::Le,
            Token::AngleRightEq => BinOp::Ge,
            Token::In => BinOp::In,
            Token::Not => {
                let span1 = self.take(Token::Not)?;
                let span2 = self.take(Token::In)?;
                return Ok(Some(BinOp::NotIn.with_span(span1.merge(span2))));
            }
            _ => return Ok(None),
        };

        let (_, span) = self.next()?;
        Ok(Some(op.with_span(span)))
    }

    fn access(&mut self, r: Restrictions) -> ParseResult<Spanned<Expr>> {
        let mut expr = self.atom(r)?;

        while self.next_is(Token::Period) {
            let ident = self.identifier()?;
            if self.peek_is(Token::RoundLeft) {
                let args = self.args()?;
                let span = expr.span.merge(args.span);
                expr = Expr::MethodCall(Box::new(expr), ident, args)
                    .with_span(span);
            } else {
                let span = expr.span.merge(ident.span);
                expr = Expr::Access(Box::new(expr), ident).with_span(span);
            }
        }

        Ok(expr)
    }

    fn atom(&mut self, r: Restrictions) -> ParseResult<Spanned<Expr>> {
        if self.peek_is(Token::RoundLeft) {
            self.take(Token::RoundLeft)?;
            let expr = self.expr()?;
            self.take(Token::RoundRight)?;
            return Ok(expr);
        }

        if self.peek_is(Token::SquareLeft) {
            let values = self.separated(
                Token::SquareLeft,
                Token::SquareRight,
                Token::Comma,
                Self::expr,
            )?;
            let span = values.span;
            return Ok(Expr::List(values.inner).with_span(span));
        }

        if self.peek_is(Token::CurlyLeft) {
            let key_values = self.record()?;
            let span = key_values.span;
            return Ok(Expr::Record(key_values.inner).with_span(span));
        }

        if self.peek_is(Token::Match) {
            return self.match_expr();
        }

        if let Some(Token::Ident(_)) = self.peek() {
            let ident = self.identifier()?;
            if !r.forbid_records && self.peek_is(Token::CurlyLeft) {
                let key_values = self.record()?;
                let span = ident.span.merge(key_values.span);
                return Ok(
                    Expr::TypedRecord(ident, key_values).with_span(span)
                );
            }
            if self.peek_is(Token::RoundLeft) {
                let args = self.args()?;
                let span = ident.span.merge(args.span);
                return Ok(Expr::FunctionCall(ident, args).with_span(span));
            }
            let span = ident.span;
            return Ok(Expr::Var(ident).with_span(span));
        }

        let literal = self.literal()?;

        // If we parsed a prefix, it may be followed by a prefix match
        // If not, it can be an access expression
        if let Literal::Prefix(prefix) = &literal.inner {
            if let Some(ty) = self.try_prefix_match_type()? {
                return Ok(Expr::PrefixMatch(PrefixMatchExpr {
                    prefix: prefix.clone(),
                    ty,
                })
                .with_span(literal.span));
            }
        }

        let span = literal.span;
        Ok(Expr::Literal(literal).with_span(span))
    }

    fn match_expr(&mut self) -> ParseResult<Spanned<Expr>> {
        let start = self.take(Token::Match)?;
        let expr = self.expr_no_records()?;

        let mut arms = Vec::new();
        self.take(Token::CurlyLeft)?;
        while !self.peek_is(Token::CurlyRight) {
            let variant_id = self.identifier()?;

            let data_field = if self.peek_is(Token::RoundLeft) {
                self.take(Token::RoundLeft)?;
                let ident = self.identifier()?;
                self.take(Token::RoundRight)?;
                Some(ident)
            } else {
                None
            };

            let guard = if self.next_is(Token::Pipe) {
                Some(self.expr()?)
            } else {
                None
            };

            self.take(Token::Arrow)?;

            let body = if self.peek_is(Token::CurlyLeft) {
                let exprs = self.block()?;
                self.next_is(Token::Comma);
                exprs
            } else {
                let expr = self.expr()?;
                self.take(Token::Comma)?;
                Block { exprs: vec![expr] }
            };

            arms.push(MatchArm {
                variant_id,
                data_field,
                guard,
                body,
            })
        }

        let end = self.take(Token::CurlyRight)?;
        let span = start.merge(end);
        Ok(Expr::Match(Box::new(Match { expr, arms })).with_span(span))
    }

    /// Parse any literal, including prefixes, ip addresses and communities
    fn literal(&mut self) -> ParseResult<Spanned<Literal>> {
        // A prefix length, it requires two tokens
        if let Some(Token::PrefixLength(..)) = self.peek() {
            let prefix_length = self.prefix_length()?;
            let len = prefix_length.inner;
            return Ok(
                Literal::PrefixLength(len).with_span(prefix_length.span)
            );
        }

        // If we see an IpAddress, we need to check whether it is followed by a
        // slash and is therefore a prefix instead.
        if matches!(self.peek(), Some(Token::IpV4(_) | Token::IpV6(_))) {
            let addr = self.ip_address()?;
            if let Some(Token::PrefixLength(..)) = self.peek() {
                let len = self.prefix_length()?;
                let span = addr.span.merge(len.span);
                return Ok(
                    Literal::Prefix(Prefix { addr, len }).with_span(span)
                );
            } else {
                return Ok(
                    Literal::IpAddress(addr.inner).with_span(addr.span)
                );
            }
        }

        self.simple_literal()
    }

    fn ip_address(&mut self) -> ParseResult<Spanned<IpAddress>> {
        let (token, span) = self.next()?;
        let addr = match token {
            Token::IpV4(s) => IpAddress::Ipv4(
                s.parse::<std::net::Ipv4Addr>().map_err(|e| {
                    ParseError::invalid_literal("Ipv4 addresss", s, e, span)
                })?,
            ),
            Token::IpV6(s) => IpAddress::Ipv6(
                s.parse::<std::net::Ipv6Addr>().map_err(|e| {
                    ParseError::invalid_literal("Ipv6 addresss", s, e, span)
                })?,
            ),
            _ => {
                return Err(ParseError::expected(
                    "an IP address",
                    token,
                    span,
                ))
            }
        };
        Ok(addr.with_span(span))
    }

    /// Parse literals that need no complex parsing, just one token
    fn simple_literal(&mut self) -> ParseResult<Spanned<Literal>> {
        // TODO: Make proper errors using the spans
        let (token, span) = self.next()?;
        let literal = match token {
            Token::Accept => Literal::Accept,
            Token::Reject => Literal::Reject,
            Token::String(s) => {
                // Trim the quotes from the string literal
                let trimmed = &s[1..s.len() - 1];
                Literal::String(trimmed.into())
            }
            Token::Integer(s) => Literal::Integer(
                // This parse fails if the literal is too big,
                // it should be handled properly
                s.parse::<i64>().map_err(|e| {
                    ParseError::invalid_literal("integer", token, e, span)
                })?,
            ),
            Token::Hex(s) => Literal::Integer(
                i64::from_str_radix(&s[2..], 16).map_err(|e| {
                    ParseError::invalid_literal(
                        "hexadecimal integer",
                        token,
                        e,
                        span,
                    )
                })?,
            ),
            Token::Asn(s) => {
                Literal::Asn(s[2..].parse::<u32>().map_err(|e| {
                    ParseError::invalid_literal("AS number", token, e, span)
                })?)
            }
            Token::Bool(b) => Literal::Bool(b),
            Token::Float => {
                unimplemented!("Floating point numbers are not supported yet")
            }
            Token::Community(s) => {
                // We offload the validation of the community to routecore
                // but routecore doesn't do all the hex numbers correctly,
                // so we transform those first.

                // TODO: Change the AST so that it doesn't contain strings, but
                // routecore communities.
                use routecore::bgp::communities::Community;

                let parts = s
                    .split(':')
                    .map(|p| {
                        if let Some(hex) = p.strip_prefix("0x") {
                            Ok(u32::from_str_radix(hex, 16)?.to_string())
                        } else {
                            Ok(p.to_string())
                        }
                    })
                    .collect::<Result<Vec<_>, _>>()
                    .map_err(|e: ParseIntError| {
                        ParseError::invalid_literal(
                            "community",
                            s,
                            e,
                            span.clone(),
                        )
                    })?;

                let transformed = parts.join(":");

                let c: Community =
                    transformed.parse::<Community>().map_err(|e| {
                        ParseError::invalid_literal(
                            "community",
                            token,
                            e,
                            span,
                        )
                    })?;
                match c {
                    Community::Standard(x) => Literal::StandardCommunity(x),
                    Community::Extended(x) => Literal::ExtendedCommunity(x),
                    Community::Large(x) => Literal::LargeCommunity(x),
                    Community::Ipv6Extended(_) => {
                        unimplemented!(
                            "IPv6 extended communities are not supported yet"
                        )
                    }
                }
            }
            t => return Err(ParseError::expected("a literal", t, span)),
        };
        Ok(literal.with_span(span))
    }

    /// Parse an (anonymous) record
    ///
    /// ```ebnf
    /// Record      ::= '{' (RecordField (',' RecordField)* ','? )? '}'
    /// RecordField ::= Identifier ':' ValueExpr
    /// ```
    fn record(
        &mut self,
    ) -> ParseResult<Spanned<Vec<(Spanned<Identifier>, Spanned<Expr>)>>> {
        self.separated(
            Token::CurlyLeft,
            Token::CurlyRight,
            Token::Comma,
            |parser| {
                let key = parser.identifier()?;
                parser.take(Token::Colon)?;
                let value = parser.expr()?;
                Ok((key, value))
            },
        )
    }

    /// Parse a list of arguments to a method
    ///
    /// ```ebnf
    /// ArgExprList ::= '(' ( ValueExpr (',' ValueExpr)* ','? )? ')'
    /// ```
    pub(super) fn args(
        &mut self,
    ) -> ParseResult<Spanned<Vec<Spanned<Expr>>>> {
        let args = self.separated(
            Token::RoundLeft,
            Token::RoundRight,
            Token::Comma,
            Self::expr,
        )?;

        Ok(args)
    }

    /// Parse a prefix match type, which can follow a prefix in some contexts
    ///
    /// ```ebnf
    /// PrefixMatchType ::= 'longer'
    ///                   | 'orlonger'
    ///                   | 'prefix-length-range' PrefixLengthRange
    ///                   | 'upto' PrefixLength
    ///                   | 'netmask' IpAddress
    /// ```
    fn try_prefix_match_type(
        &mut self,
    ) -> ParseResult<Option<PrefixMatchType>> {
        let match_type = if self.next_is(Token::Exact) {
            PrefixMatchType::Exact
        } else if self.next_is(Token::Longer) {
            PrefixMatchType::Longer
        } else if self.next_is(Token::OrLonger) {
            PrefixMatchType::OrLonger
        } else if self.next_is(Token::PrefixLengthRange) {
            PrefixMatchType::PrefixLengthRange(
                self.prefix_length_range()?.inner,
            )
        } else if self.next_is(Token::UpTo) {
            PrefixMatchType::UpTo(self.prefix_length()?.inner)
        } else if self.next_is(Token::NetMask) {
            PrefixMatchType::NetMask(self.ip_address()?.inner)
        } else {
            return Ok(None);
        };

        Ok(Some(match_type))
    }

    /// Parse a prefix length range
    ///
    /// ```ebnf
    /// PrefixLengthRange ::= PrefixLength '-' PrefixLength
    /// ```
    fn prefix_length_range(
        &mut self,
    ) -> ParseResult<Spanned<PrefixLengthRange>> {
        let start = self.prefix_length()?;
        self.take(Token::Hyphen)?;
        let end = self.prefix_length()?;
        let span = start.span.merge(end.span);
        Ok(PrefixLengthRange {
            start: start.inner,
            end: end.inner,
        }
        .with_span(span))
    }

    /// Parse a prefix length
    ///
    /// ```ebnf
    /// PrefixLength ::= '/' Integer
    /// ```
    fn prefix_length(&mut self) -> ParseResult<Spanned<u8>> {
        let (token, span) = self.next()?;
        let Token::PrefixLength(s) = token else {
            return Err(ParseError::invalid_literal(
                "prefix length",
                token,
                "",
                span,
            ));
        };
        let len = s[1..].parse::<u8>().map_err(|e| {
            ParseError::invalid_literal("prefix length", token, e, span)
        })?;
        Ok(len.with_span(span))
    }
}
