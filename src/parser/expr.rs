use std::{net::IpAddr, ops::Range};

use inetnum::asn::Asn;

use crate::{
    ast::{
        BinOp, Block, Expr, FStringPart, Identifier, Literal, Match,
        MatchArm, Path, Pattern, Record, RecordType, ReturnKind, Stmt,
        TypeExpr,
    },
    parser::ParseError,
};

use super::{
    error::ParseErrorKind,
    meta::{Meta, Span},
    token::{FStringToken, Keyword, Token},
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
impl Parser<'_, '_> {
    /// Parse a block expression
    ///
    /// ```ebnf
    /// Block      ::= '{' Stmt* Expr? '}'
    /// Stmt       ::= ImportStmt | ExprStmt | IfStmt | MatchStmt
    /// ImportStmt ::= 'import' Path ';'
    /// ExprStmt   ::= Expr ';'
    /// IfStmt     ::= If ';'?
    /// MatchStmt  ::= Match ';'?
    /// ```
    pub fn block(&mut self) -> ParseResult<Meta<Block>> {
        let start = self.take(Token::CurlyLeft)?;

        let mut imports = Vec::new();
        let mut stmts = Vec::new();

        loop {
            if self.peek_is(Token::CurlyRight) {
                let end = self.take(Token::CurlyRight)?;
                return Ok(self.spans.add(
                    start.merge(end),
                    Block {
                        imports,
                        stmts,
                        last: None,
                    },
                ));
            }

            if self.peek_is(Token::Keyword(Keyword::Import)) {
                let mut paths = self.import()?;
                imports.append(&mut paths);
            } else if self.peek_is(Token::Keyword(Keyword::Let)) {
                let start = self.take(Token::Keyword(Keyword::Let))?;
                let identifier = self.identifier()?;
                let ty = if self.peek_is(Token::Colon) {
                    self.take(Token::Colon)?;
                    Some(self.type_expr()?)
                } else {
                    None
                };
                self.take(Token::Eq)?;
                let expr = self.expr()?;
                let end = self.take(Token::SemiColon)?;
                stmts.push(
                    self.spans.add(
                        start.merge(end),
                        Stmt::Let(identifier, ty, expr),
                    ),
                )
            }
            // Edge case: if and match don't have to end in a semicolon
            // but if they appear at the end, they are the last
            // expression. This is what Rust does too and while it looks
            // hacky, it works really well in practice.
            else if self.peek_is(Token::Keyword(Keyword::If)) {
                let expr = self.if_else()?;
                if self.peek_is(Token::CurlyRight) {
                    let end = self.take(Token::CurlyRight)?;
                    let span = start.merge(end);
                    return Ok(self.spans.add(
                        span,
                        Block {
                            imports,
                            stmts,
                            last: Some(Box::new(expr)),
                        },
                    ));
                }

                let stmt = Meta {
                    id: expr.id,
                    node: Stmt::Expr(expr),
                };
                stmts.push(stmt);

                // Semicolon is allowed but not mandatory after if
                self.next_is(Token::SemiColon);
            } else if self.peek_is(Token::Keyword(Keyword::Match)) {
                let expr = self.match_expr()?;
                if self.peek_is(Token::CurlyRight) {
                    let end = self.take(Token::CurlyRight)?;
                    let span = start.merge(end);
                    return Ok(self.spans.add(
                        span,
                        Block {
                            imports,
                            stmts,
                            last: Some(Box::new(expr)),
                        },
                    ));
                }
                let stmt = Meta {
                    id: expr.id,
                    node: Stmt::Expr(expr),
                };
                stmts.push(stmt);

                // Semicolon is allowed but not mandatory after match
                self.next_is(Token::SemiColon);
            } else if self.peek_is(Token::Keyword(Keyword::While)) {
                let expr = self.while_expr()?;
                if self.peek_is(Token::CurlyRight) {
                    let end = self.take(Token::CurlyRight)?;
                    let span = start.merge(end);
                    return Ok(self.spans.add(
                        span,
                        Block {
                            imports,
                            stmts,
                            last: Some(Box::new(expr)),
                        },
                    ));
                }
                let stmt = Meta {
                    id: expr.id,
                    node: Stmt::Expr(expr),
                };
                stmts.push(stmt);

                // Semicolon is allowed but not mandatory after while
                self.next_is(Token::SemiColon);
            } else {
                let expr = self.expr()?;
                if self.next_is(Token::SemiColon) {
                    let stmt = Meta {
                        id: expr.id,
                        node: Stmt::Expr(expr),
                    };
                    stmts.push(stmt);
                } else {
                    let end = self.take(Token::CurlyRight)?;
                    let span = start.merge(end);
                    return Ok(self.spans.add(
                        span,
                        Block {
                            imports,
                            stmts,
                            last: Some(Box::new(expr)),
                        },
                    ));
                }
            };
        }
    }

    /// Parse an expression while allowing record literals
    ///
    /// ```ebnf
    /// Expr ::= LogicalExpr
    /// ```
    pub fn expr(&mut self) -> ParseResult<Meta<Expr>> {
        self.expr_inner(Restrictions {
            forbid_records: false,
        })
    }

    /// Parse an expression without allowing record literals
    ///
    /// ```ebnf
    /// Expr ::= LogicalExpr
    /// ```
    fn expr_no_records(&mut self) -> ParseResult<Meta<Expr>> {
        self.expr_inner(Restrictions {
            forbid_records: true,
        })
    }

    fn expr_inner(&mut self, r: Restrictions) -> ParseResult<Meta<Expr>> {
        self.assign_expr(r)
    }

    /// Parse an assignment expression
    fn assign_expr(&mut self, r: Restrictions) -> ParseResult<Meta<Expr>> {
        let left = self.logical_expr(r)?;
        if self.next_is(Token::Eq) {
            let Expr::Path(path) = &*left else {
                return Err(ParseError::custom(
                    "left-hand side of an expression must be a single identifier",
                    "cannot assign to this expression",
                    self.get_span(&left)));
            };
            let right = self.logical_expr(r)?;
            let span = self.merge_spans(&left, &right);
            Ok(self
                .spans
                .add(span, Expr::Assign(path.clone(), Box::new(right))))
        } else {
            Ok(left)
        }
    }

    /// Parse a logical expression
    ///
    /// To avoid confusion, we don't allow `&&` and `||` to be chained together.
    ///
    /// ```ebnf
    /// LogicalExpr ::= Comparison ( ('&&' Comparison )* | ('||' Comparison)* )
    /// ```
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

    /// Parse a comparison expression
    ///
    /// ```ebnf
    /// Comparison ::= Sum (CompareOp Sum)?
    /// ```
    fn comparison(&mut self, r: Restrictions) -> ParseResult<Meta<Expr>> {
        let expr = self.sum(r)?;

        if let Some(op) = self.try_compare_operator()? {
            let right = self.sum(r)?;
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
            Token::Keyword(Keyword::In) => BinOp::In,
            Token::Keyword(Keyword::Not) => {
                let span1 = self.take(Token::Keyword(Keyword::Not))?;
                let span2 = self.take(Token::Keyword(Keyword::In))?;
                let span = span1.merge(span2);
                let x = self.spans.add(span, BinOp::NotIn);
                return Ok(Some(x));
            }
            _ => return Ok(None),
        };

        let (_, span) = self.next()?;
        Ok(Some(self.spans.add(span, op)))
    }

    /// Parse a sum expression
    ///
    /// ```ebnf
    /// Sum ::= Term (('+' | '-') Sum)?
    /// ```
    fn sum(&mut self, r: Restrictions) -> ParseResult<Meta<Expr>> {
        let left = self.term(r)?;
        if self.next_is(Token::Plus) {
            let right = self.sum(r)?;
            let span = self.merge_spans(&left, &right);
            Ok(self.spans.add(
                span,
                Expr::BinOp(Box::new(left), BinOp::Add, Box::new(right)),
            ))
        } else if self.next_is(Token::Hyphen) {
            let right = self.sum(r)?;
            let span = self.merge_spans(&left, &right);
            Ok(self.spans.add(
                span,
                Expr::BinOp(Box::new(left), BinOp::Sub, Box::new(right)),
            ))
        } else {
            Ok(left)
        }
    }

    /// Parse a term expression
    ///
    /// ```ebnf
    /// Term ::= Negation (('*' | '/') Term)?
    /// ```
    fn term(&mut self, r: Restrictions) -> ParseResult<Meta<Expr>> {
        let left = self.negation(r)?;
        if self.next_is(Token::Star) {
            let right = self.term(r)?;
            let span = self.merge_spans(&left, &right);
            Ok(self.spans.add(
                span,
                Expr::BinOp(Box::new(left), BinOp::Mul, Box::new(right)),
            ))
        } else if self.next_is(Token::Slash) {
            let right = self.term(r)?;
            let span = self.merge_spans(&left, &right);
            Ok(self.spans.add(
                span,
                Expr::BinOp(Box::new(left), BinOp::Div, Box::new(right)),
            ))
        } else {
            Ok(left)
        }
    }

    /// Parse a negation expression
    ///
    /// ```ebnf
    /// Negation ::= 'not' Access
    /// ```
    fn negation(&mut self, r: Restrictions) -> ParseResult<Meta<Expr>> {
        // TODO: A negation should be a unary `-`. The `not` operator should
        // have much lower precedence.
        if self.peek_is(Token::Keyword(Keyword::Not)) {
            let span = self.take(Token::Keyword(Keyword::Not))?;
            let expr = self.negation(r)?;
            let span = span.merge(self.get_span(&expr));
            Ok(self.spans.add(span, Expr::Not(Box::new(expr))))
        } else if self.peek_is(Token::Hyphen) {
            let span = self.take(Token::Hyphen)?;
            let expr = self.access(r)?;
            let span = span.merge(self.get_span(&expr));
            Ok(self.spans.add(span, Expr::Negate(Box::new(expr))))
        } else {
            self.access(r)
        }
    }

    /// Parse an access expression
    ///
    /// ```ebnf
    /// Access ::= Atom ('.' Atom)* Args?
    /// ```
    fn access(&mut self, r: Restrictions) -> ParseResult<Meta<Expr>> {
        let mut expr = self.atom(r)?;

        loop {
            if self.peek_is(Token::QuestionMark) {
                let span = self.take(Token::QuestionMark)?;
                let span = span.merge(self.get_span(&expr));
                expr =
                    self.spans.add(span, Expr::QuestionMark(Box::new(expr)));
            } else if self.peek_is(Token::RoundLeft) {
                let args = self.args()?;
                let span = self.merge_spans(&expr, &args);
                expr = self
                    .spans
                    .add(span, Expr::FunctionCall(Box::new(expr), args));
            } else if self.next_is(Token::Period) {
                let ident = self.identifier()?;
                let span = self.merge_spans(&expr, &ident);
                expr =
                    self.spans.add(span, Expr::Access(Box::new(expr), ident));
            } else {
                break;
            }
        }

        Ok(expr)
    }

    /// Parse an atom expression
    ///
    /// ```ebnf
    /// Atom ::= '(' Expr ')'
    ///        | ListExpr
    ///        | 'accept' Expr?
    ///        | 'reject' Expr?
    ///        | 'return' Expr?
    ///        | IfExpr
    ///        | MatchExpr
    ///        | Path RecordExpr?
    /// ```
    fn atom(&mut self, r: Restrictions) -> ParseResult<Meta<Expr>> {
        if self.peek_is(Token::RoundLeft) {
            let span_left = self.take(Token::RoundLeft)?;

            // If the next token is `)` then this is a unit expression
            // otherwise it's just parentheses.
            if self.peek_is(Token::RoundRight) {
                let span_right = self.take(Token::RoundRight)?;
                let span = span_left.merge(span_right);
                let literal = self.spans.add(span, Literal::Unit);
                return Ok(Meta {
                    id: literal.id,
                    node: Expr::Literal(literal),
                });
            }

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

        if let Some(Token::Keyword(
            Keyword::Accept | Keyword::Reject | Keyword::Return,
        )) = self.peek()
        {
            let (t, mut span) = self.next()?;

            let kind = match t {
                Token::Keyword(Keyword::Accept) => ReturnKind::Accept,
                Token::Keyword(Keyword::Reject) => ReturnKind::Reject,
                Token::Keyword(Keyword::Return) => ReturnKind::Return,
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

        if self.peek_is(Token::Keyword(Keyword::If)) {
            return self.if_else();
        }

        if self.peek_is(Token::Keyword(Keyword::Match)) {
            return self.match_expr();
        }

        if self.peek_is(Token::Keyword(Keyword::While)) {
            return self.while_expr();
        }

        if matches!(
            self.peek(),
            Some(
                Token::Ident(_)
                    | Token::Keyword(
                        Keyword::Super
                            | Keyword::Pkg
                            | Keyword::Dep
                            | Keyword::Std
                    )
            )
        ) {
            let path = self.path()?;
            if !r.forbid_records && self.peek_is(Token::CurlyLeft) {
                let key_values = self.record()?;
                let span = self.merge_spans(&path, &key_values);
                return Ok(self
                    .spans
                    .add(span, Expr::TypedRecord(path, key_values)));
            } else {
                return Ok(Meta {
                    id: path.id,
                    node: Expr::Path(path),
                });
            }
        }

        if self.peek_is(Token::FStringStart) {
            return self.f_string();
        }

        let literal = self.literal()?;

        Ok(Meta {
            id: literal.id,
            node: Expr::Literal(literal),
        })
    }

    fn can_start_expression(tok: &Token) -> bool {
        matches!(
            tok,
            Token::RoundLeft
                | Token::CurlyLeft
                | Token::SquareLeft
                | Token::Ident(..)
                | Token::Bang
                | Token::Bool(_)
                | Token::Integer(_)
                | Token::Float(_)
                | Token::Hyphen
                | Token::IpV4(_)
                | Token::IpV6(_)
                | Token::Asn(_)
                | Token::String(_)
        )
    }

    /// Parse an if expression
    ///
    /// ```ebnf
    /// IfExpr ::= 'if' Expr Block ('else' (IfExpr | Block))
    /// ```
    fn if_else(&mut self) -> ParseResult<Meta<Expr>> {
        let start = self.take(Token::Keyword(Keyword::If))?;
        let cond = self.expr_no_records()?;
        let then_block = self.block()?;

        if self.next_is(Token::Keyword(Keyword::Else)) {
            let else_block = if self.peek_is(Token::Keyword(Keyword::If)) {
                let expr = self.if_else()?;
                Meta {
                    id: expr.id,
                    node: Block {
                        imports: Vec::new(),
                        stmts: Vec::new(),
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

    /// Parse a while expression
    fn while_expr(&mut self) -> ParseResult<Meta<Expr>> {
        let start = self.take(Token::Keyword(Keyword::While))?;
        let cond = self.expr_no_records()?;
        let block = self.block()?;

        let span = start.merge(self.spans.get(&block));
        Ok(self.spans.add(span, Expr::While(Box::new(cond), block)))
    }

    /// Parse a match expression
    ///
    /// ```ebnf
    /// MatchExpr    ::= 'match' Expr '{' MatchArm* MatchArmLast? '}'
    /// MatchArm     ::= Pattern ('|' Expr)? '->' (Block | (Expr ','))
    /// MatchArmLast ::= Pattern ('|' Expr)? '->' Expr
    /// ```
    fn match_expr(&mut self) -> ParseResult<Meta<Expr>> {
        let start = self.take(Token::Keyword(Keyword::Match))?;
        let expr = self.expr_no_records()?;

        let mut arms = Vec::new();
        self.take(Token::CurlyLeft)?;
        while !self.peek_is(Token::CurlyRight) {
            let variant = self.identifier()?;
            let mut span = self.get_span(&variant);

            let resolved_variant = variant.as_str();

            let pattern = if resolved_variant == "_" {
                Pattern::Underscore
            } else {
                let data_field = if self.peek_is(Token::RoundLeft) {
                    let fields = self.separated(
                        Token::RoundLeft,
                        Token::RoundRight,
                        Token::Comma,
                        Self::identifier,
                    )?;
                    span = self.merge_spans(&variant, &fields);
                    Some(fields)
                } else {
                    None
                };

                Pattern::EnumVariant {
                    variant,
                    fields: data_field,
                }
            };

            let pattern = self.add_span(span, pattern);

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

                // If this is not the last item, we require a comma, if it is
                // the last one, (i.e. if we get a `}` next), we don't need it.
                if !self.peek_is(Token::CurlyRight) {
                    self.take(Token::Comma)?;
                }

                Meta {
                    id: expr.id,
                    node: Block {
                        imports: Vec::new(),
                        stmts: Vec::new(),
                        last: Some(Box::new(expr)),
                    },
                }
            };

            arms.push(MatchArm {
                pattern,
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
        // If we see an IpAddress, we need to check whether it is followed by a
        // slash and is therefore a prefix instead.
        if matches!(self.peek(), Some(Token::IpV4(_) | Token::IpV6(_))) {
            let addr = self.ip_address()?;
            return Ok(Meta {
                id: addr.id,
                node: Literal::IpAddress(addr.node),
            });
        }

        self.simple_literal()
    }

    fn ip_address(&mut self) -> ParseResult<Meta<IpAddr>> {
        let (token, span) = self.next()?;
        let addr = match token {
            Token::IpV4(s) => {
                IpAddr::V4(s.parse::<std::net::Ipv4Addr>().map_err(|e| {
                    ParseError::invalid_literal("Ipv4 addresses", s, e, span)
                })?)
            }
            Token::IpV6(s) => {
                IpAddr::V6(s.parse::<std::net::Ipv6Addr>().map_err(|e| {
                    ParseError::invalid_literal("Ipv6 addresses", s, e, span)
                })?)
            }
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
                // The span starts at the quote so we add one to get to the
                // string content.
                let span = Span {
                    start: span.start + 1,
                    ..span
                };
                let unescaped = unescape(trimmed, span)?;
                Literal::String(unescaped)
            }
            Token::Integer(s) => {
                Literal::Integer(s.parse::<i64>().map_err(|e| {
                    ParseError::invalid_literal("integer", token, e, span)
                })?)
            }
            Token::Float(s) => {
                Literal::Float(s.parse::<f64>().map_err(|e| {
                    ParseError::invalid_literal("float", token, e, span)
                })?)
            }
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
            Token::Asn(s) => match s[2..].parse::<u32>() {
                Ok(x) => Literal::Asn(Asn::from_u32(x)),
                Err(e) => {
                    return Err(ParseError::invalid_literal(
                        "AS number",
                        token,
                        e,
                        span,
                    ))
                }
            },
            Token::Bool(b) => Literal::Bool(b),
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

    pub(super) fn type_expr(&mut self) -> ParseResult<Meta<TypeExpr>> {
        let mut type_expr = self.type_expr_atom()?;

        while self.peek_is(Token::QuestionMark) {
            let span = self.spans.get(&type_expr);
            let span2 = self.take(Token::QuestionMark).unwrap();
            let span = span.merge(span2);
            let new_type_expr = TypeExpr::Option(Box::new(type_expr));
            type_expr = self.spans.add(span, new_type_expr);
        }

        Ok(type_expr)
    }

    fn type_expr_atom(&mut self) -> ParseResult<Meta<TypeExpr>> {
        if self.peek_is(Token::Bang) {
            let span = self.take(Token::Bang)?;
            return Ok(self.spans.add(span, TypeExpr::Never));
        }

        if self.peek_is(Token::RoundLeft) {
            let span_left = self.take(Token::RoundLeft)?;
            let span_right = self.take(Token::RoundRight)?;
            let span = span_left.merge(span_right);
            return Ok(self.spans.add(span, TypeExpr::Unit));
        }

        if self.peek_is(Token::CurlyLeft) {
            let record_type = self.record_type()?;
            let span = self.get_span(&record_type.fields);
            return Ok(self.spans.add(span, TypeExpr::Record(record_type)));
        }

        let path = self.path()?;
        let path_span = self.get_span(&path);

        Ok(if self.peek_is(Token::SquareLeft) {
            let params = self.separated(
                Token::SquareLeft,
                Token::SquareRight,
                Token::Comma,
                Self::type_expr,
            )?;
            let span = path_span.merge(self.spans.get(&params));
            self.spans.add(span, TypeExpr::Path(path, Some(params)))
        } else {
            self.spans.add(path_span, TypeExpr::Path(path, None))
        })
    }

    pub(super) fn record_type(&mut self) -> ParseResult<RecordType> {
        let fields = self.separated(
            Token::CurlyLeft,
            Token::CurlyRight,
            Token::Comma,
            Self::record_field,
        )?;
        Ok(RecordType { fields })
    }

    fn record_field(
        &mut self,
    ) -> ParseResult<(Meta<Identifier>, Meta<TypeExpr>)> {
        let key = self.identifier()?;
        self.take(Token::Colon)?;

        let field_type = self.type_expr()?;

        Ok((key, field_type))
    }

    pub(super) fn path(&mut self) -> ParseResult<Meta<Path>> {
        let mut idents = Vec::new();
        idents.push(self.path_item()?);
        while self.next_is(Token::Period) {
            idents.push(self.path_item()?);
        }
        let span =
            self.merge_spans(idents.first().unwrap(), idents.last().unwrap());
        Ok(self.add_span(span, Path { idents }))
    }

    pub(super) fn path_expr(&mut self) -> ParseResult<Vec<Meta<Path>>> {
        let mut paths: Vec<Meta<Path>> = Vec::new();
        let mut root: Path = Path { idents: Vec::new() };
        loop {
            if self.peek_is(Token::CurlyLeft) {
                let sub_paths = self.path_list()?;

                for mut sp in sub_paths {
                    let mut new_idents = root.idents.clone();
                    new_idents.append(&mut sp.idents);
                    let new_path = Meta {
                        node: Path { idents: new_idents },
                        id: sp.id,
                    };

                    paths.push(new_path);
                }
                break;
            }

            let ident: Meta<Identifier> = self.path_item()?;
            root.idents.push(ident);

            if !self.next_is(Token::Period) {
                let span = self.merge_spans(
                    root.idents.first().unwrap(),
                    root.idents.last().unwrap(),
                );
                paths.push(self.add_span(span, root));
                break;
            }
        }

        Ok(paths)
    }

    fn path_list(&mut self) -> ParseResult<Vec<Meta<Path>>> {
        let mut paths: Vec<Meta<Path>> = Vec::new();
        self.take(Token::CurlyLeft)?;
        loop {
            let mut path = self.path_expr()?;
            paths.append(&mut path);
            if !self.next_is(Token::Comma) {
                break;
            }
        }
        self.take(Token::CurlyRight)?;
        Ok(paths)
    }

    fn path_item(&mut self) -> ParseResult<Meta<Identifier>> {
        let (tok, span) = self.next()?;
        let ident: Identifier = match tok {
            Token::Keyword(Keyword::Pkg) => "pkg".into(),
            Token::Keyword(Keyword::Dep) => "dep".into(),
            Token::Keyword(Keyword::Super) => "super".into(),
            Token::Ident(s) => s.into(),
            _ => {
                return Err(ParseError::expected(
                    "an identifier, `super`, `pkg` or `dep`",
                    &tok,
                    span,
                )
                .with_note(format!(
                "`{tok}` is a keyword and cannot be used as an identifier."
            )))
            }
        };
        Ok(self.spans.add(span, ident))
    }

    fn f_string(&mut self) -> ParseResult<Meta<Expr>> {
        let mut parts = Vec::new();

        let start_span = self.take(Token::FStringStart)?;

        // TODO: we need to properly unescape the `{{` and `}}`
        while let Some((part, span)) = self.lexer.f_string_part() {
            let (FStringToken::StringEnd(s)
            | FStringToken::StringIntermediate(s)) = &part;

            if !s.is_empty() {
                let span = Span {
                    file: self.file,
                    start: span.start,
                    end: span.end,
                };
                let s = unescape(s, span)?;
                let s = s.replace("{{", "{").replace("}}", "}");
                parts.push(self.spans.add(span, FStringPart::String(s)));
            }

            if matches!(part, FStringToken::StringEnd(_)) {
                let span = start_span.merge(Span::new(self.file, span));
                return Ok(self.spans.add(span, Expr::FString(parts)));
            }

            self.take(Token::CurlyLeft)?;
            let expr = self.expr()?;

            // Do not use the same id as expr here. We need separate ids
            // to associate the proper function data to it.
            let span = self.spans.get(&expr);

            parts.push(self.spans.add(span, FStringPart::Expr(expr)));
            self.take(Token::CurlyRight)?;
        }

        Err(ParseError {
            kind: ParseErrorKind::EndOfInput,
            location: Span::new(
                self.file,
                self.file_length..self.file_length,
            ),
            note: None,
        })
    }
}

fn unescape(s: &str, span: Span) -> ParseResult<String> {
    let mut unescaped = String::new();
    let mut errors = Vec::new();
    rustc_literal_escaper::unescape_str(s, |range: Range<usize>, res| {
        match res {
            Ok(ch) => unescaped.push(ch),
            Err(e) => errors.push((range, e)),
        }
    });

    // We don't care about these because they are not errors but
    // warnings.
    // TODO: Print these warnings
    errors.retain(|(_, e)| e.is_fatal());

    if let Some((range, e)) = errors.first() {
        let start = span.start + range.start;
        let end = span.start + range.end;
        let span = Span::new(span.file, start..end);
        Err(ParseError::escape(e, span))
    } else {
        Ok(unescaped)
    }
}
