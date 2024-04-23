use std::num::ParseIntError;

use crate::{
    ast::{
        BinOp, Block, Expr, IpAddress, Literal, Match, MatchArm, Prefix,
        PrefixLengthRange, PrefixMatchExpr, PrefixMatchType, Record,
        ReturnKind,
    },
    parser::ParseError,
};

use super::{meta::Meta, token::Token, ParseResult, Parser};

/// Contextual restrictions on the expression parsing
///
/// This is used to resolve ambiguities in the grammar.
#[derive(Clone, Copy)]
struct Restrictions {
    forbid_records: bool,
}

/// # Parsing value expressions
impl<'source> Parser<'source, '_> {
    pub fn block(&mut self) -> ParseResult<Meta<Block>> {
        let start = self.take(Token::CurlyLeft)?;

        let mut exprs = Vec::new();

        loop {
            if self.peek_is(Token::CurlyRight) {
                let end = self.take(Token::CurlyRight)?;
                return Ok(self
                    .spans
                    .add(start.merge(end), Block { exprs, last: None }));
            }

            // Edge case: if and match don't have to end in a semicolon
            // but if they appear at the end, they are the last
            // expression. This is what Rust does too and while it looks
            // hacky, it works really well in practice.
            if self.peek_is(Token::If) {
                let expr = self.if_else()?;
                if self.peek_is(Token::CurlyRight) {
                    let end = self.take(Token::CurlyRight)?;
                    let span = start.merge(end);
                    return Ok(self.spans.add(
                        span,
                        Block {
                            exprs,
                            last: Some(Box::new(expr)),
                        },
                    ));
                }
                exprs.push(expr);

                // Semicolon is allowed but not mandatory after if
                self.next_is(Token::SemiColon);
            } else if self.peek_is(Token::Match) {
                let expr = self.match_expr()?;
                if self.peek_is(Token::CurlyRight) {
                    let end = self.take(Token::CurlyRight)?;
                    let span = start.merge(end);
                    return Ok(self.spans.add(
                        span,
                        Block {
                            exprs,
                            last: Some(Box::new(expr)),
                        },
                    ));
                }
                exprs.push(expr);

                // Semicolon is allowed but not mandatory after match
                self.next_is(Token::SemiColon);
            } else {
                let expr = self.expr()?;
                if self.next_is(Token::SemiColon) {
                    exprs.push(expr);
                } else {
                    let end = self.take(Token::CurlyRight)?;
                    let span = start.merge(end);
                    return Ok(self.spans.add(
                        span,
                        Block {
                            exprs,
                            last: Some(Box::new(expr)),
                        },
                    ));
                }
            };
        }
    }

    pub fn expr(&mut self) -> ParseResult<Meta<Expr>> {
        self.expr_inner(Restrictions {
            forbid_records: false,
        })
    }

    fn expr_no_records(&mut self) -> ParseResult<Meta<Expr>> {
        self.expr_inner(Restrictions {
            forbid_records: true,
        })
    }

    fn expr_inner(&mut self, r: Restrictions) -> ParseResult<Meta<Expr>> {
        self.logical_expr(r)
    }

    fn logical_expr(&mut self, r: Restrictions) -> ParseResult<Meta<Expr>> {
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
                    let s = self.spans.merge(&e, &acc);
                    self.spans.add(
                        s,
                        Expr::BinOp(Box::new(e), BinOp::And, Box::new(acc)),
                    )
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
                    let span = self.spans.merge(&e, &acc);
                    self.spans.add(
                        span,
                        Expr::BinOp(Box::new(e), BinOp::Or, Box::new(acc)),
                    )
                })
                .unwrap())
        } else {
            Ok(expr)
        }
    }

    fn comparison(&mut self, r: Restrictions) -> ParseResult<Meta<Expr>> {
        let expr = self.access(r)?;

        if let Some(op) = self.try_compare_operator()? {
            let right = self.access(r)?;
            let span = self.merge_spans(&expr, &right);
            Ok(self.spans.add(
                span,
                Expr::BinOp(Box::new(expr), op.node, Box::new(right)),
            ))
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
    fn try_compare_operator(&mut self) -> ParseResult<Option<Meta<BinOp>>> {
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
                let span = span1.merge(span2);
                let x = self.spans.add(span, BinOp::NotIn);
                return Ok(Some(x));
            }
            _ => return Ok(None),
        };

        let (_, span) = self.next()?;
        Ok(Some(self.spans.add(span, op)))
    }

    fn access(&mut self, r: Restrictions) -> ParseResult<Meta<Expr>> {
        let mut expr = self.atom(r)?;

        while self.next_is(Token::Period) {
            let ident = self.identifier()?;
            if self.peek_is(Token::RoundLeft) {
                let args = self.args()?;
                let span = self.merge_spans(&expr, &args);
                expr = self
                    .spans
                    .add(span, Expr::MethodCall(Box::new(expr), ident, args));
            } else {
                let span = self.merge_spans(&expr, &ident);
                expr =
                    self.spans.add(span, Expr::Access(Box::new(expr), ident));
            }
        }

        Ok(expr)
    }

    fn atom(&mut self, r: Restrictions) -> ParseResult<Meta<Expr>> {
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
            return Ok(Meta {
                id: values.id,
                node: Expr::List(values.node),
            });
        }

        if self.peek_is(Token::CurlyLeft) {
            let key_values = self.record()?;
            let span = self.spans.get(&key_values);
            return Ok(self.spans.add(span, Expr::Record(key_values)));
        }

        if let Some(Token::Accept | Token::Reject | Token::Return) =
            self.peek()
        {
            let (t, mut span) = self.next()?;

            let kind = match t {
                Token::Accept => ReturnKind::Accept,
                Token::Reject => ReturnKind::Reject,
                Token::Return => ReturnKind::Return,
                _ => unreachable!(),
            };

            let val = match self.peek() {
                Some(tok) if Self::can_start_expression(tok) => {
                    let expr = self.expr()?;
                    span = span.merge(self.spans.get(expr.id));
                    Some(Box::new(expr))
                }
                _ => None,
            };

            return Ok(self.spans.add(span, Expr::Return(kind, val)));
        }

        if self.peek_is(Token::If) {
            return self.if_else();
        }

        if self.peek_is(Token::Match) {
            return self.match_expr();
        }

        if let Some(Token::Ident(_)) = self.peek() {
            let ident = self.identifier()?;
            if !r.forbid_records && self.peek_is(Token::CurlyLeft) {
                let key_values = self.record()?;
                let span = self.merge_spans(&ident, &key_values);
                return Ok(self
                    .spans
                    .add(span, Expr::TypedRecord(ident, key_values)));
            }
            if self.peek_is(Token::RoundLeft) {
                let args = self.args()?;
                let span = self.merge_spans(&ident, &args);
                return Ok(self
                    .spans
                    .add(span, Expr::FunctionCall(ident, args)));
            }
            return Ok(Meta {
                id: ident.id,
                node: Expr::Var(ident),
            });
        }

        let literal = self.literal()?;

        // If we parsed a prefix, it may be followed by a prefix match
        // If not, it can be an access expression
        if let Literal::Prefix(prefix) = &literal.node {
            if let Some(ty) = self.try_prefix_match_type()? {
                return Ok(Meta {
                    id: literal.id,
                    node: Expr::PrefixMatch(PrefixMatchExpr {
                        prefix: prefix.clone(),
                        ty,
                    }),
                });
            }
        }

        Ok(Meta {
            id: literal.id,
            node: Expr::Literal(literal),
        })
    }

    fn can_start_expression(tok: &Token) -> bool {
        match tok {
            Token::RoundLeft
            | Token::CurlyLeft
            | Token::SquareLeft
            | Token::Ident(..)
            | Token::Bang
            | Token::Bool(_)
            | Token::Integer(_)
            | Token::Hyphen => true,
            _ => false,
        }
    }

    fn if_else(&mut self) -> ParseResult<Meta<Expr>> {
        let start = self.take(Token::If)?;
        let cond = self.expr_no_records()?;
        let then_block = self.block()?;

        if self.next_is(Token::Else) {
            let else_block = if self.peek_is(Token::If) {
                let expr = self.if_else()?;
                Meta {
                    id: expr.id,
                    node: Block {
                        exprs: Vec::new(),
                        last: Some(Box::new(expr)),
                    },
                }
            } else {
                self.block()?
            };
            let span = start.merge(self.spans.get(&else_block));
            Ok(self.spans.add(
                span,
                Expr::IfElse(Box::new(cond), then_block, Some(else_block)),
            ))
        } else {
            let span = start.merge(self.spans.get(&then_block));
            Ok(self
                .spans
                .add(span, Expr::IfElse(Box::new(cond), then_block, None)))
        }
    }

    fn match_expr(&mut self) -> ParseResult<Meta<Expr>> {
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
                Meta {
                    id: expr.id,
                    node: Block {
                        exprs: Vec::new(),
                        last: Some(Box::new(expr)),
                    },
                }
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
        let match_expr = self.spans.add(span, Match { expr, arms });
        Ok(self.spans.add(span, Expr::Match(Box::new(match_expr))))
    }

    /// Parse any literal, including prefixes, ip addresses and communities
    fn literal(&mut self) -> ParseResult<Meta<Literal>> {
        // A prefix length, it requires two tokens
        if let Some(Token::PrefixLength(..)) = self.peek() {
            let prefix_length = self.prefix_length()?;
            let len = prefix_length.node;
            return Ok(Meta {
                id: prefix_length.id,
                node: Literal::PrefixLength(len),
            });
        }

        // If we see an IpAddress, we need to check whether it is followed by a
        // slash and is therefore a prefix instead.
        if matches!(self.peek(), Some(Token::IpV4(_) | Token::IpV6(_))) {
            let addr = self.ip_address()?;
            if let Some(Token::PrefixLength(..)) = self.peek() {
                let len = self.prefix_length()?;
                let span = self.merge_spans(&addr, &len);
                return Ok(self
                    .spans
                    .add(span, Literal::Prefix(Prefix { addr, len })));
            } else {
                return Ok(Meta {
                    id: addr.id,
                    node: Literal::IpAddress(addr.node),
                });
            }
        }

        self.simple_literal()
    }

    fn ip_address(&mut self) -> ParseResult<Meta<IpAddress>> {
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
        Ok(self.spans.add(span, addr))
    }

    /// Parse literals that need no complex parsing, just one token
    fn simple_literal(&mut self) -> ParseResult<Meta<Literal>> {
        // TODO: Make proper errors using the spans
        let (token, span) = self.next()?;
        let literal = match token {
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
                        ParseError::invalid_literal("community", s, e, span)
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
        Ok(self.spans.add(span, literal))
    }

    /// Parse an (anonymous) record
    ///
    /// ```ebnf
    /// Record      ::= '{' (RecordField (',' RecordField)* ','? )? '}'
    /// RecordField ::= Identifier ':' ValueExpr
    /// ```
    fn record(&mut self) -> ParseResult<Meta<Record>> {
        let fields = self.separated(
            Token::CurlyLeft,
            Token::CurlyRight,
            Token::Comma,
            |parser| {
                let key = parser.identifier()?;
                parser.take(Token::Colon)?;
                let value = parser.expr()?;
                Ok((key, value))
            },
        )?;

        Ok(Meta {
            id: fields.id,
            node: Record {
                fields: fields.node,
            },
        })
    }

    /// Parse a list of arguments to a method
    ///
    /// ```ebnf
    /// ArgExprList ::= '(' ( ValueExpr (',' ValueExpr)* ','? )? ')'
    /// ```
    pub(super) fn args(&mut self) -> ParseResult<Meta<Vec<Meta<Expr>>>> {
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
                self.prefix_length_range()?.node,
            )
        } else if self.next_is(Token::UpTo) {
            PrefixMatchType::UpTo(self.prefix_length()?.node)
        } else if self.next_is(Token::NetMask) {
            PrefixMatchType::NetMask(self.ip_address()?.node)
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
    ) -> ParseResult<Meta<PrefixLengthRange>> {
        let start = self.prefix_length()?;
        self.take(Token::Hyphen)?;
        let end = self.prefix_length()?;
        let span = self.merge_spans(&start, &end);
        Ok(self.spans.add(
            span,
            PrefixLengthRange {
                start: start.node,
                end: end.node,
            },
        ))
    }

    /// Parse a prefix length
    ///
    /// ```ebnf
    /// PrefixLength ::= '/' Integer
    /// ```
    fn prefix_length(&mut self) -> ParseResult<Meta<u8>> {
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
        Ok(self.spans.add(span, len))
    }
}
