use std::num::ParseIntError;

use crate::{
    ast::{
        AccessExpr, AccessReceiver, AnonymousRecordValueExpr, ArgExprList,
        AsnLiteral, BooleanLiteral, ComputeExpr, ExtendedCommunityLiteral,
        FieldAccessExpr, HexLiteral, Identifier, IntegerLiteral, IpAddress,
        Ipv4Addr, Ipv6Addr, LargeCommunityLiteral, ListValueExpr,
        LiteralAccessExpr, LiteralExpr, MethodComputeExpr, Prefix,
        PrefixLength, PrefixLengthLiteral, PrefixLengthRange,
        PrefixMatchExpr, PrefixMatchType, StandardCommunityLiteral,
        StringLiteral, TypeIdentifier, TypedRecordValueExpr, ValueExpr,
    },
    parser::ParseError,
};

use super::{
    span::{Spanned, WithSpan},
    token::Token,
    ParseResult, Parser,
};

/// # Parsing value expressions
impl<'source> Parser<'source> {
    /// Parse a value expr
    ///
    /// ```ebnf
    /// ValueExpr ::= '[' ValueExpr* ']
    ///             | Identifier? Record
    ///             | MethodCall
    ///             | Identifier AccessExpr
    ///             | PrefixMatchExpr
    ///             | Literal AccessExpr
    /// ```
    pub(super) fn value_expr(&mut self) -> ParseResult<ValueExpr> {
        if self.peek_is(Token::SquareLeft) {
            let values = self.separated(
                Token::SquareLeft,
                Token::SquareRight,
                Token::Comma,
                Self::value_expr,
            )?;
            return Ok(ValueExpr::ListExpr(ListValueExpr { values }));
        }

        if self.peek_is(Token::CurlyLeft) {
            return Ok(ValueExpr::AnonymousRecordExpr(
                AnonymousRecordValueExpr {
                    key_values: self.record()?,
                },
            ));
        }

        if let Some(Token::Ident(_)) = self.peek() {
            let id = self.identifier()?;
            if self.peek_is(Token::CurlyLeft) {
                let Identifier { ident: s } = id.inner;
                let type_id = TypeIdentifier { ident: s }.with_span(id.span);
                let key_values = self.record()?;
                let span = id.span.merge(key_values.span);

                return Ok(ValueExpr::TypedRecordExpr(
                    TypedRecordValueExpr {
                        type_id,
                        key_values,
                    }
                    .with_span(span),
                ));
            }

            if self.peek_is(Token::RoundLeft) {
                let args = self.arg_expr_list()?;
                return Ok(ValueExpr::RootMethodCallExpr(
                    MethodComputeExpr { ident: id, args },
                ));
            }

            let receiver = AccessReceiver::Ident(id);
            let access_expr = self.access_expr()?;

            return Ok(ValueExpr::ComputeExpr(ComputeExpr {
                receiver,
                access_expr,
            }));
        }

        let literal = self.literal()?;

        // If we parsed a prefix, it may be followed by a prefix match
        // If not, it can be an access expression
        if let LiteralExpr::PrefixLiteral(prefix) = &literal {
            if let Some(ty) = self.try_prefix_match_type()? {
                return Ok(ValueExpr::PrefixMatchExpr(PrefixMatchExpr {
                    prefix: prefix.clone(),
                    ty,
                }));
            }
        }

        let access_expr = self.access_expr()?;
        Ok(ValueExpr::LiteralAccessExpr(LiteralAccessExpr {
            literal,
            access_expr,
        }))
    }

    /// Parse an access expresion
    ///
    /// ```ebnf
    /// AccessExpr ::= ( '.' ( MethodCallExpr | FieldAccessExpr ) )*
    /// ```
    fn access_expr(&mut self) -> ParseResult<Vec<AccessExpr>> {
        let mut access_expr = Vec::new();

        while self.next_is(Token::Period) {
            let ident = self.identifier()?;
            if self.peek_is(Token::RoundLeft) {
                let args = self.arg_expr_list()?;
                access_expr.push(AccessExpr::MethodComputeExpr(
                    MethodComputeExpr { ident, args },
                ))
            } else if let Some(AccessExpr::FieldAccessExpr(
                FieldAccessExpr { field_names },
            )) = access_expr.last_mut()
            {
                field_names.push(ident);
            } else {
                access_expr.push(AccessExpr::FieldAccessExpr(
                    FieldAccessExpr {
                        field_names: vec![ident],
                    },
                ))
            }
        }

        Ok(access_expr)
    }

    /// Parse any literal, including prefixes, ip addresses and communities
    fn literal(&mut self) -> ParseResult<LiteralExpr> {
        // A prefix length, it requires two tokens
        if let Some(Token::PrefixLength(..)) = self.peek() {
            let PrefixLength(len) = self.prefix_length()?;
            return Ok(LiteralExpr::PrefixLengthLiteral(
                PrefixLengthLiteral(len),
            ));
        }

        // If we see an IpAddress, we need to check whether it is followed by a
        // slash and is therefore a prefix instead.
        if matches!(self.peek(), Some(Token::IpV4(_) | Token::IpV6(_))) {
            let addr = self.ip_address()?;
            if let Some(Token::PrefixLength(..)) = self.peek() {
                let len = self.prefix_length()?;
                return Ok(LiteralExpr::PrefixLiteral(Prefix { addr, len }));
            } else {
                return Ok(LiteralExpr::IpAddressLiteral(addr));
            }
        }

        self.simple_literal()
    }

    fn ip_address(&mut self) -> ParseResult<IpAddress> {
        let (token, span) = self.next()?;
        Ok(match token {
            Token::IpV4(s) => IpAddress::Ipv4(Ipv4Addr(
                s.parse::<std::net::Ipv4Addr>().map_err(|e| {
                    ParseError::InvalidLiteral {
                        description: "Ipv4 addresss".into(),
                        token: s.to_string(),
                        span,
                        inner_error: e.to_string(),
                    }
                })?,
            )),
            Token::IpV6(s) => IpAddress::Ipv6(Ipv6Addr(
                s.parse::<std::net::Ipv6Addr>().map_err(|e| {
                    ParseError::InvalidLiteral {
                        description: "Ipv6 addresss".into(),
                        token: s.to_string(),
                        span,
                        inner_error: e.to_string(),
                    }
                })?,
            )),
            _ => {
                return Err(ParseError::Expected {
                    expected: "an IP address".into(),
                    got: token.to_string(),
                    span,
                })
            }
        })
    }

    /// Parse literals that need no complex parsing, just one token
    fn simple_literal(&mut self) -> ParseResult<LiteralExpr> {
        // TODO: Make proper errors using the spans
        let (token, span) = self.next()?;
        Ok(match token {
            Token::String(s) => {
                // Trim the quotes from the string literal
                let trimmed = &s[1..s.len() - 1];
                LiteralExpr::StringLiteral(StringLiteral(trimmed.into()))
            }
            Token::Integer(s) => LiteralExpr::IntegerLiteral(IntegerLiteral(
                // This parse fails if the literal is too big,
                // it should be handled properly
                s.parse::<i64>().map_err(|e| ParseError::InvalidLiteral {
                    description: "integer".into(),
                    token: token.to_string(),
                    span,
                    inner_error: e.to_string(),
                })?,
            )),
            Token::Hex(s) => LiteralExpr::HexLiteral(HexLiteral(
                u64::from_str_radix(&s[2..], 16).map_err(|e| {
                    ParseError::InvalidLiteral {
                        description: "hexadecimal integer".into(),
                        token: token.to_string(),
                        span,
                        inner_error: e.to_string(),
                    }
                })?,
            )),
            Token::Asn(s) => LiteralExpr::AsnLiteral(AsnLiteral(
                s[2..].parse::<u32>().map_err(|e| {
                    ParseError::InvalidLiteral {
                        description: "AS number".into(),
                        token: token.to_string(),
                        span,
                        inner_error: e.to_string(),
                    }
                })?,
            )),
            Token::Bool(b) => LiteralExpr::BooleanLiteral(BooleanLiteral(b)),
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
                        ParseError::InvalidLiteral {
                            description: "community".into(),
                            token: s.to_string(),
                            span: span.clone(),
                            inner_error: e.to_string(),
                        }
                    })?;

                let transformed = parts.join(":");

                let c: Community =
                    transformed.parse::<Community>().map_err(|e| {
                        ParseError::InvalidLiteral {
                            description: "community".into(),
                            token: token.to_string(),
                            span,
                            inner_error: e.to_string(),
                        }
                    })?;
                match c {
                    Community::Standard(x) => {
                        LiteralExpr::StandardCommunityLiteral(
                            StandardCommunityLiteral(x),
                        )
                    }
                    Community::Extended(x) => {
                        LiteralExpr::ExtendedCommunityLiteral(
                            ExtendedCommunityLiteral(x),
                        )
                    }
                    Community::Large(x) => {
                        LiteralExpr::LargeCommunityLiteral(
                            LargeCommunityLiteral(x),
                        )
                    }
                    Community::Ipv6Extended(_) => {
                        unimplemented!(
                            "IPv6 extended communities are not supported yet"
                        )
                    }
                }
            }
            t => {
                return Err(ParseError::Expected {
                    expected: "a literal".into(),
                    got: t.to_string(),
                    span,
                })
            }
        })
    }

    /// Parse an (anonymous) record
    ///
    /// ```ebnf
    /// Record      ::= '{' (RecordField (',' RecordField)* ','? )? '}'
    /// RecordField ::= Identifier ':' ValueExpr
    /// ```
    fn record(
        &mut self,
    ) -> ParseResult<Spanned<Vec<(Spanned<Identifier>, ValueExpr)>>> {
        self.separated(
            Token::CurlyLeft,
            Token::CurlyRight,
            Token::Comma,
            |parser| {
                let key = parser.identifier()?;
                parser.take(Token::Colon)?;
                let value = parser.value_expr()?;
                Ok((key, value))
            },
        )
    }

    /// Parse a list of arguments to a method
    ///
    /// ```ebnf
    /// ArgExprList ::= '(' ( ValueExpr (',' ValueExpr)* ','? )? ')'
    /// ```
    pub(super) fn arg_expr_list(&mut self) -> ParseResult<ArgExprList> {
        let args = self.separated(
            Token::RoundLeft,
            Token::RoundRight,
            Token::Comma,
            Self::value_expr,
        )?;

        Ok(ArgExprList { args })
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
            PrefixMatchType::PrefixLengthRange(self.prefix_length_range()?)
        } else if self.next_is(Token::UpTo) {
            PrefixMatchType::UpTo(self.prefix_length()?)
        } else if self.next_is(Token::NetMask) {
            PrefixMatchType::NetMask(self.ip_address()?)
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
    fn prefix_length_range(&mut self) -> ParseResult<PrefixLengthRange> {
        let start = self.prefix_length()?;
        self.take(Token::Hyphen)?;
        let end = self.prefix_length()?;
        Ok(PrefixLengthRange { start, end })
    }

    /// Parse a prefix length
    ///
    /// ```ebnf
    /// PrefixLength ::= '/' Integer
    /// ```
    fn prefix_length(&mut self) -> ParseResult<PrefixLength> {
        let (token, span) = self.next()?;
        let Token::PrefixLength(s) = token else {
            return Err(ParseError::InvalidLiteral {
                description: "prefix length".into(),
                token: token.to_string(),
                span,
                inner_error: String::new(),
            });
        };
        let len =
            s[1..]
                .parse::<u8>()
                .map_err(|e| ParseError::InvalidLiteral {
                    description: "prefix length".into(),
                    token: token.to_string(),
                    span,
                    inner_error: e.to_string(),
                })?;
        Ok(PrefixLength(len))
    }
}
