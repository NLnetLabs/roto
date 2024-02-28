use crate::ast::{
    AcceptReject, AccessExpr, AccessReceiver, AnonymousRecordValueExpr,
    ApplyBody, ApplyScope, ArgExprList, AsnLiteral, BooleanLiteral,
    ComputeExpr, DefineBody, ExtendedCommunityLiteral, FieldAccessExpr,
    FilterMatchActionExpr, HexLiteral, Identifier, IntegerLiteral, IpAddress,
    Ipv4Addr, Ipv6Addr, LargeCommunityLiteral, ListValueExpr,
    LiteralAccessExpr, LiteralExpr, MatchActionExpr, MatchOperator,
    MethodComputeExpr, Prefix, PrefixLength, PrefixLengthLiteral,
    PrefixLengthRange, PrefixMatchExpr, PrefixMatchType, RootExpr, RxTxType,
    StandardCommunityLiteral, StringLiteral, SyntaxTree, TypeIdentField,
    TypeIdentifier, TypedRecordValueExpr, ValueExpr,
};
use crate::token::Token;
use logos::{Lexer, Span, SpannedIter};
use miette::Diagnostic;
use std::iter::Peekable;

mod filter_map;
mod rib_like;

type ParseResult<T> = Result<T, ParseError>;

#[derive(Clone, Debug, Diagnostic)]
pub enum ParseError {
    EndOfInput,
    InvalidToken(Span),
    /// Dummy variant where more precise messages should be made
    ///
    /// The argument is just a unique identifier
    Todo(usize),
    Expected {
        expected: String,
        got: String,
        #[label("expected {expected}")]
        span: Span,
    },
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::EndOfInput => write!(f, "unexpected end of input"),
            Self::InvalidToken(_) => write!(f, "invalid token"),
            Self::Todo(n) => write!(f, "add a nice message here {n}"),
            Self::Expected {
                expected,
                got,
                span,
            } => {
                write!(f, "expected '{expected}' but got '{got}' at {span:?}")
            }
        }
    }
}

impl std::error::Error for ParseError {}

pub struct Parser<'source> {
    lexer: Peekable<SpannedIter<'source, Token<'source>>>,
}

/// # Helper methods
impl<'source> Parser<'source> {
    /// Move the lexer forward and return the token
    fn next(&mut self) -> ParseResult<(Token<'source>, Span)> {
        match self.lexer.next() {
            None => Err(ParseError::EndOfInput),
            Some((Err(()), span)) => Err(ParseError::InvalidToken(span)),
            Some((Ok(token), span)) => Ok((token, span)),
        }
    }

    /// Peek the next token
    fn peek(&mut self) -> Option<&Token<'source>> {
        match self.lexer.peek() {
            Some((Ok(token), _span)) => Some(token),
            _ => None,
        }
    }

    /// Peek the next token and return whether it matches the given token
    fn peek_is(&mut self, token: Token) -> bool {
        let Some(lexed_token) = self.peek() else {
            return false;
        };

        &token == lexed_token
    }

    /// Check for an optional token
    fn accept_optional(&mut self, token: Token) -> ParseResult<Option<Span>> {
        if self.peek_is(token) {
            // TODO: this should probably be an unwrap?
            Ok(Some(self.next()?.1))
        } else {
            Ok(None)
        }
    }

    /// Move the lexer forward and assert that it matches the token
    fn accept_required(&mut self, token: Token) -> ParseResult<()> {
        let (next, span) = self.next()?;
        if next == token {
            Ok(())
        } else {
            Err(ParseError::Expected {
                expected: token.to_string(),
                got: next.to_string(),
                span,
            })
        }
    }

    /// Parse a separated and delimited list of items
    ///
    /// Assuming that `{`, `}` and `,` are the opening, closing and separating
    /// tokens, respectively. And the given parser passes `FOO`, then this
    /// function corrsponds to the following grammar rule:
    ///
    /// ```ebnf
    /// '{' (FOO (',' FOO)* ',')? '}'
    /// ```
    ///
    /// So, the list is allowed to be empty and a trailing separator is allowed.
    fn separated<T>(
        &mut self,
        open: Token,
        close: Token,
        sep: Token,
        mut parser: impl FnMut(&mut Self) -> ParseResult<T>,
    ) -> ParseResult<Vec<T>> {
        self.accept_required(open)?;

        let mut items = Vec::new();

        // If there are no fields, return the empty vec.
        if self.accept_optional(close.clone())?.is_some() {
            return Ok(items);
        }

        // Parse the first field
        items.push(parser(self)?);

        // Now each field must be separated by a comma
        while self.accept_optional(sep.clone())?.is_some() {
            // If we have found the curly right, we have just
            // parsed the trailing comma.
            if self.peek_is(close.clone()) {
                break;
            }

            items.push(parser(self)?);
        }

        self.accept_required(close)?;

        Ok(items)
    }
}

/// # Parsing complex expressions
impl<'source> Parser<'source> {
    pub fn parse(input: &'source str) -> ParseResult<SyntaxTree> {
        Self {
            lexer: Lexer::new(input).spanned().peekable(),
        }
        .tree()
    }

    fn tree(&mut self) -> ParseResult<SyntaxTree> {
        let mut expressions = Vec::new();

        while self.peek().is_some() {
            expressions.push(self.root()?);
        }

        Ok(SyntaxTree { expressions })
    }

    /// Parse a root expression
    ///
    /// ```ebnf
    /// Root ::= Rib | Table | OutputStream | FilterMap | Type
    /// ```
    fn root(&mut self) -> ParseResult<RootExpr> {
        let expr = match self.peek().ok_or(ParseError::EndOfInput)? {
            Token::Rib => RootExpr::Rib(self.rib()?),
            Token::Table => RootExpr::Table(self.table()?),
            Token::OutputStream => {
                RootExpr::OutputStream(self.output_stream()?)
            }
            Token::FilterMap | Token::Filter => {
                RootExpr::FilterMap(Box::new(self.filter_map()?))
            }
            Token::Type => RootExpr::Ty(self.record_type_assignment()?),
            _ => {
                let (token, span) = self.next()?;
                return Err(ParseError::Expected {
                    expected:
                        "a rib, table, output-stream, filter or filter-map"
                            .into(),
                    got: token.to_string(),
                    span,
                });
            }
        };
        Ok(expr)
    }

    /// Parse the body of a define section
    ///
    /// ```ebnf
    /// DefineBody ::= '{' RxTxType Use? Assignment* '}'
    ///
    /// RxTxType   ::= ( 'rx_tx' TypeIdentField ';'
    ///                | 'rx' TypeIdentField ';' 'tx' TypeIdentField ';'
    ///                | 'rx' TypeIdentField )
    ///
    /// Use        ::= 'use' Identifier Identifier
    ///
    /// Assignment ::= Identifier '=' ValueExpr ';'
    /// ```
    fn define_body(&mut self) -> ParseResult<DefineBody> {
        self.accept_required(Token::CurlyLeft)?;

        let rx_tx_type = match self.next()?.0 {
            Token::RxTx => {
                let field = self.type_ident_field()?;
                self.accept_required(Token::SemiColon)?;
                RxTxType::PassThrough(field)
            }
            Token::Rx => {
                let rx_field = self.type_ident_field()?;
                self.accept_required(Token::SemiColon)?;
                if self.accept_optional(Token::Tx)?.is_some() {
                    let tx_field = self.type_ident_field()?;
                    self.accept_required(Token::SemiColon)?;
                    RxTxType::Split(rx_field, tx_field)
                } else {
                    RxTxType::RxOnly(rx_field)
                }
            }
            _ => return Err(ParseError::Todo(9)),
        };

        let mut use_ext_data = Vec::new();
        while self.accept_optional(Token::Use)?.is_some() {
            use_ext_data.push((self.identifier()?, self.identifier()?));
            self.accept_required(Token::SemiColon)?;
        }

        let mut assignments = Vec::new();
        while self.accept_optional(Token::CurlyRight)?.is_none() {
            let id = self.identifier()?;
            self.accept_required(Token::Eq)?;
            let value = self.value_expr()?;
            self.accept_required(Token::SemiColon)?;
            assignments.push((id, value));
        }

        Ok(DefineBody {
            rx_tx_type,
            use_ext_data,
            assignments,
        })
    }

    /// Parse the body of an apply section
    ///
    /// ```ebnf
    /// ApplyBody ::= ApplyScope* (AcceptReject ';')?
    /// ```
    fn apply_body(&mut self) -> ParseResult<ApplyBody> {
        self.accept_required(Token::CurlyLeft)?;
        let mut scopes = Vec::new();

        while !(self.peek_is(Token::Return)
            || self.peek_is(Token::CurlyRight))
        {
            scopes.push(self.apply_scope()?);
        }

        let accept_reject = self.accept_reject()?;
        self.accept_required(Token::CurlyRight)?;
        Ok(ApplyBody {
            scopes,
            accept_reject,
        })
    }

    /// Parse a scope of the body of apply
    ///
    /// ```ebnf
    /// ApplyScope ::= 'filter' 'match' ValueExpr
    ///                'not'? 'matching'
    ///                Actions ';'
    ///
    /// Actions    ::= '{' (ValueExpr ';')* ( AcceptReject ';' )? '}'
    /// ```
    fn apply_scope(&mut self) -> ParseResult<ApplyScope> {
        self.accept_required(Token::Filter)?;
        self.accept_required(Token::Match)?;

        let filter_ident = self.value_expr()?;
        let negate = self.accept_optional(Token::Not)?.is_some();
        self.accept_required(Token::Matching)?;
        self.accept_required(Token::CurlyLeft)?;

        let mut actions = Vec::new();
        while self.accept_optional(Token::CurlyRight)?.is_none() {
            if let Some(accept_reject) = self.accept_reject()? {
                self.accept_required(Token::CurlyRight)?;
                actions.push((None, Some(accept_reject)));
                break;
            }

            let val = self.value_expr()?;
            self.accept_required(Token::SemiColon)?;
            actions.push((Some(val), None));
        }

        self.accept_required(Token::SemiColon)?;

        Ok(ApplyScope {
            scope: None,
            match_action: MatchActionExpr::FilterMatchAction(
                FilterMatchActionExpr {
                    operator: MatchOperator::Match,
                    negate,
                    actions,
                    filter_ident,
                },
            ),
        })
    }

    /// Parse a statement returning accept or reject
    ///
    /// ```ebnf
    /// AcceptReject ::= ('return' ( 'accept' | 'reject' ) ';')?
    /// ```
    fn accept_reject(&mut self) -> ParseResult<Option<AcceptReject>> {
        // Note: this is different from the original parser
        // In the original, the return is optional, but all the examples seem to
        // require it, so it is now required,
        // We could choose to remove it entirely as well.
        if self.accept_optional(Token::Return)?.is_some() {
            let value = match self.next()?.0 {
                Token::Accept => AcceptReject::Accept,
                Token::Reject => AcceptReject::Reject,
                _ => return Err(ParseError::Todo(10)),
            };
            self.accept_required(Token::SemiColon)?;
            Ok(Some(value))
        } else {
            Ok(None)
        }
    }

    /// Parse a match operator
    ///
    /// ```ebnf
    /// MatchOperator ::= 'match' ( Identifier 'with' )?
    ///                 | 'some' | 'exactly-one' | 'all'
    /// ```
    fn match_operator(&mut self) -> ParseResult<MatchOperator> {
        let op = match self.next()?.0 {
            Token::Match => {
                if matches!(self.peek(), Some(Token::Ident(_))) {
                    let ident = self.identifier()?;
                    self.accept_required(Token::With)?;
                    MatchOperator::MatchValueWith(ident)
                } else {
                    MatchOperator::Match
                }
            }
            Token::Some => MatchOperator::Some,
            Token::ExactlyOne => MatchOperator::ExactlyOne,
            Token::All => MatchOperator::All,
            _ => return Err(ParseError::Todo(11)),
        };

        Ok(op)
    }

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
    fn value_expr(&mut self) -> ParseResult<ValueExpr> {
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
                let Identifier { ident: s } = id;
                return Ok(ValueExpr::TypedRecordExpr(
                    TypedRecordValueExpr {
                        type_id: TypeIdentifier { ident: s },
                        key_values: self.record()?,
                    },
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
            if let Some(ty) = self.prefix_match_type()? {
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

        while self.accept_optional(Token::Period)?.is_some() {
            let ident = self.identifier()?;
            if self.peek_is(Token::RoundLeft) {
                let args = self.arg_expr_list()?;
                access_expr.push(AccessExpr::MethodComputeExpr(
                    MethodComputeExpr { ident, args },
                ))
            } else {
                // TODO: This is technically different from the nom
                // parser, because the nom parser will eagerly get
                // multiple fields. This is not necessary and the
                // Vec in FieldAccessExpr can probably be removed.
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
        // TODO: Implement the following literals:
        //  - StandardCommunity
        //  - LargeCommunity
        //  - ExtendedCommunity

        // A prefix length, it requires two tokens
        if self.accept_optional(Token::Slash)?.is_some() {
            let PrefixLength(len) = self.prefix_length()?;
            return Ok(LiteralExpr::PrefixLengthLiteral(
                PrefixLengthLiteral(len),
            ));
        }

        // If we see an IpAddress, we need to check whether it is followed by a
        // slash and is therefore a prefix instead.
        if matches!(self.peek(), Some(Token::IpV4(_) | Token::IpV6(_))) {
            let addr = self.ip_address()?;
            if self.peek_is(Token::Slash) {
                let len = self.prefix_length()?;
                return Ok(LiteralExpr::PrefixLiteral(Prefix { addr, len }));
            } else {
                return Ok(LiteralExpr::IpAddressLiteral(addr));
            }
        }

        self.simple_literal()
    }

    /// Parse literals that need no complex parsing, just one token
    fn simple_literal(&mut self) -> ParseResult<LiteralExpr> {
        // TODO: Make proper errors using the spans
        let (token, span) = self.next()?;
        Ok(match token {
            Token::String(s) => {
                LiteralExpr::StringLiteral(StringLiteral(s.into()))
            }
            Token::Integer(s) => LiteralExpr::IntegerLiteral(IntegerLiteral(
                // This parse fails if the literal is too big,
                // it should be handled properly
                s.parse().unwrap(),
            )),
            Token::Hex(s) => LiteralExpr::HexLiteral(HexLiteral(
                u64::from_str_radix(&s[2..], 16).unwrap(),
            )),
            Token::Asn(s) => LiteralExpr::AsnLiteral(AsnLiteral(
                u32::from_str_radix(&s[2..], 16).unwrap(),
            )),
            Token::Bool(b) => LiteralExpr::BooleanLiteral(BooleanLiteral(b)),
            Token::Float => {
                unimplemented!("Floating point numbers are not supported yet")
            }
            Token::Community(s) => {
                // We offload the validation of the community to routecore
                // TODO: Change the AST so that it doesn't contain strings, but
                // routecore communities.
                use routecore::bgp::communities::Community;
                let c: Community =
                    s.parse().map_err(|_| ParseError::Todo(12))?;
                match c {
                    Community::Standard(x) => {
                        LiteralExpr::StandardCommunityLiteral(
                            StandardCommunityLiteral(x.to_string()),
                        )
                    }
                    Community::Extended(x) => {
                        LiteralExpr::ExtendedCommunityLiteral(
                            ExtendedCommunityLiteral(x.to_string()),
                        )
                    }
                    Community::Large(x) => {
                        LiteralExpr::LargeCommunityLiteral(
                            LargeCommunityLiteral(x.to_string()),
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
    fn record(&mut self) -> ParseResult<Vec<(Identifier, ValueExpr)>> {
        self.separated(
            Token::CurlyLeft,
            Token::CurlyRight,
            Token::Comma,
            |parser| {
                let key = parser.identifier()?;
                parser.accept_required(Token::Colon)?;
                let value = parser.value_expr()?;
                Ok((key, value))
            },
        )
    }

    /// Parse a method call: an identifier followed by an argument list
    ///
    /// ```ebnf
    /// MethodCall ::= Identifier ArgExprList
    /// ```
    fn method_call(&mut self) -> ParseResult<MethodComputeExpr> {
        let ident = self.identifier()?;
        let args = self.arg_expr_list()?;
        Ok(MethodComputeExpr { ident, args })
    }

    /// Parse a list of arguments to a method
    ///
    /// ```ebnf
    /// ArgExprList ::= '(' ( ValueExpr (',' ValueExpr)* ','? )? ')'
    /// ```
    fn arg_expr_list(&mut self) -> ParseResult<ArgExprList> {
        let args = self.separated(
            Token::RoundLeft,
            Token::RoundRight,
            Token::Comma,
            Self::value_expr,
        )?;

        Ok(ArgExprList { args })
    }

    /// Parse an optional for clause for filter-map, define and apply
    ///
    /// ```ebnf
    /// For ::= ( 'for' TypeIdentField)?
    /// ```
    fn for_statement(&mut self) -> ParseResult<Option<TypeIdentField>> {
        if self.accept_optional(Token::For)?.is_some() {
            Ok(Some(self.type_ident_field()?))
        } else {
            Ok(None)
        }
    }

    /// Parase an optional with clause for filter-map, define and apply
    ///
    /// ```ebnf
    /// With ::= ( 'with' TypeIdentField (',' TypeIdentField)*)?
    /// ```
    fn with_statement(&mut self) -> ParseResult<Vec<TypeIdentField>> {
        let mut key_values = Vec::new();

        if self.accept_optional(Token::With)?.is_none() {
            return Ok(key_values);
        }

        key_values.push(self.type_ident_field()?);
        while self.accept_optional(Token::Comma)?.is_some() {
            key_values.push(self.type_ident_field()?);
        }

        Ok(key_values)
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
    fn prefix_match_type(&mut self) -> ParseResult<Option<PrefixMatchType>> {
        let match_type = if self.accept_optional(Token::Exact)?.is_some() {
            PrefixMatchType::Exact
        } else if self.accept_optional(Token::Longer)?.is_some() {
            PrefixMatchType::Longer
        } else if self.accept_optional(Token::OrLonger)?.is_some() {
            PrefixMatchType::OrLonger
        } else if self.accept_optional(Token::PrefixLengthRange)?.is_some() {
            PrefixMatchType::PrefixLengthRange(self.prefix_length_range()?)
        } else if self.accept_optional(Token::UpTo)?.is_some() {
            PrefixMatchType::UpTo(self.prefix_length()?)
        } else if self.accept_optional(Token::NetMask)?.is_some() {
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
        self.accept_required(Token::Hyphen)?;
        let end = self.prefix_length()?;
        Ok(PrefixLengthRange { start, end })
    }

    /// Parse a prefix length
    ///
    /// ```ebnf
    /// PrefixLength ::= '/' Integer
    /// ```
    fn prefix_length(&mut self) -> ParseResult<PrefixLength> {
        self.accept_required(Token::Slash)?;
        return match self.next()?.0 {
            Token::Integer(s) => Ok(PrefixLength(s.parse().unwrap())),

            _ => Err(ParseError::Todo(14)),
        };
    }

    /// Parse an identifier and a type identifier separated by a colon
    ///
    /// ```ebnf
    /// TypeIdentField ::= Identifier ':' TypeIdentifier
    /// ```
    fn type_ident_field(&mut self) -> ParseResult<TypeIdentField> {
        let field_name = self.identifier()?;
        self.accept_required(Token::Colon)?;
        let ty = self.type_identifier()?;
        Ok(TypeIdentField { field_name, ty })
    }
}

/// # Parsing single items
impl<'source> Parser<'source> {
    fn ip_address(&mut self) -> ParseResult<IpAddress> {
        Ok(match self.next()?.0 {
            Token::IpV4(s) => IpAddress::Ipv4(Ipv4Addr(s.parse().unwrap())),
            Token::IpV6(s) => IpAddress::Ipv6(Ipv6Addr(s.parse().unwrap())),
            _ => return Err(ParseError::Todo(14)),
        })
    }

    fn identifier(&mut self) -> ParseResult<Identifier> {
        let (token, span) = self.next()?;
        match token {
            Token::Ident(s) => Ok(Identifier { ident: s.into() }),
            _ => Err(ParseError::Expected {
                expected: "an identifier".into(),
                got: token.to_string(),
                span,
            }),
        }
    }

    fn type_identifier(&mut self) -> ParseResult<TypeIdentifier> {
        let (token, span) = self.next()?;
        match token {
            Token::Ident(s) => Ok(TypeIdentifier { ident: s.into() }),
            _ => Err(ParseError::Expected {
                expected: "an identifier".into(),
                got: token.to_string(),
                span,
            }),
        }
    }
}