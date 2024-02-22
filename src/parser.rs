use crate::ast::{
    AcceptReject, AccessExpr, AccessReceiver, AnonymousRecordValueExpr,
    ApplyBody, ApplyScope, ApplySection, ArgExprList, AsnLiteral,
    BooleanLiteral, ComputeExpr, Define, DefineBody, FieldAccessExpr,
    FilterMap, FilterMapBody, FilterMapExpr, FilterMatchActionExpr,
    FilterType, HexLiteral, Identifier, IntegerLiteral, IpAddress, Ipv4Addr,
    Ipv6Addr, ListTypeIdentifier, ListValueExpr, LiteralAccessExpr,
    LiteralExpr, MatchActionExpr, MatchOperator, MethodComputeExpr,
    OutputStream, PrefixLength, PrefixLengthLiteral,
    PrefixLengthRange, PrefixMatchExpr, PrefixMatchType,
    RecordTypeAssignment, RecordTypeIdentifier, Rib, RibBody, RibField,
    RootExpr, RxTxType, StringLiteral, SyntaxTree, Table, TypeIdentField,
    TypeIdentifier, TypedRecordValueExpr, ValueExpr,
};
use crate::token::Token;
use logos::{Lexer, Span, SpannedIter};
use std::iter::Peekable;

type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug)]
pub enum ParseError {
    EndOfInput,
    InvalidToken(Span),
    /// Dummy variant where more precise messages should be made
    Todo,
    Expected(Span),
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::EndOfInput => write!(f, "reached end of input"),
            Self::InvalidToken(_) => write!(f, "invalid token"),
            Self::Todo => write!(f, "add a nice message here"),
            Self::Expected(s) => write!(f, "expected at {s:?}"),
        }
    }
}

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
            Err(ParseError::Expected(span))
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
            _ => return Err(ParseError::Todo),
        };
        Ok(expr)
    }

    /// Parse a rib expression
    ///
    /// ```ebnf
    /// Rib ::= 'rib' Identifier
    ///         'contains' TypeIdentifier
    ///         RibBody
    /// ```
    fn rib(&mut self) -> ParseResult<Rib> {
        self.accept_required(Token::Rib)?;
        let ident = self.identifier()?;
        self.accept_required(Token::Contains)?;
        let contain_ty = self.type_identifier()?;
        let body = self.rib_body()?;

        Ok(Rib {
            ident,
            contain_ty,
            body,
        })
    }

    /// Parse a table expression
    ///
    /// ```ebnf
    /// Table ::= 'table' Identifier
    ///           'contains' TypeIdentifier
    ///           RibBody
    /// ```
    fn table(&mut self) -> ParseResult<Table> {
        self.accept_required(Token::Table)?;
        let ident = self.identifier()?;
        self.accept_required(Token::Contains)?;
        let contain_ty = self.type_identifier()?;
        let body = self.rib_body()?;

        Ok(Table {
            ident,
            contain_ty,
            body,
        })
    }

    /// Parse an output stream expression
    ///
    /// ```ebnf
    /// OutputStream ::= 'output-stream' Identifier
    ///                  'contains' TypeIdentifier
    ///                  RibBody
    /// ```
    fn output_stream(&mut self) -> ParseResult<OutputStream> {
        self.accept_required(Token::OutputStream)?;
        let ident = self.identifier()?;
        self.accept_required(Token::Contains)?;
        let contain_ty = self.type_identifier()?;
        let body = self.rib_body()?;

        Ok(OutputStream {
            ident,
            contain_ty,
            body,
        })
    }

    /// Parse a rib body
    ///
    /// A rib body is enclosed in curly braces and the fields are separated
    /// by commas. A trailing comma is allowed.
    ///
    /// ```ebnf
    /// RibBody ::= '{' ( RibField ( ',' RibField )* ','? ) '}'
    /// ```
    fn rib_body(&mut self) -> ParseResult<RibBody> {
        let key_values = self.separated(
            Token::CurlyLeft,
            Token::CurlyRight,
            Token::Comma,
            Self::rib_field,
        )?;

        Ok(RibBody { key_values })
    }

    /// Parse a rib field
    ///
    /// ```ebnf
    /// RibField ::= Identifier ':'
    ///              (RibBody | '[' TypeIdentifier ']' | TypeIdentifier)
    /// ```
    fn rib_field(&mut self) -> ParseResult<RibField> {
        let key = self.identifier()?;
        self.accept_required(Token::Colon)?;

        let field = if self.peek_is(Token::CurlyLeft) {
            // TODO: This recursion seems to be the right thing to do, maybe
            // the syntax tree should reflect that.
            let RibBody { key_values } = self.rib_body()?;
            RibField::RecordField(Box::new((
                key,
                RecordTypeIdentifier { key_values },
            )))
        } else if self.accept_optional(Token::SquareLeft)?.is_some() {
            let inner_type = self.type_identifier()?;
            self.accept_required(Token::SquareRight)?;
            RibField::ListField(Box::new((
                key,
                ListTypeIdentifier { inner_type },
            )))
        } else {
            RibField::PrimitiveField(TypeIdentField {
                field_name: key,
                ty: self.type_identifier()?,
            })
        };

        Ok(field)
    }

    /// Parse a filter-map or filter expression
    ///
    /// ```ebnf
    /// FilterMap ::= ( 'filter-map' | 'filter' ) Identifier
    ///               For With FilterMapBody
    /// ```
    fn filter_map(&mut self) -> ParseResult<FilterMap> {
        let (token, _span) = self.next()?;
        let ty = match token {
            Token::FilterMap => FilterType::FilterMap,
            Token::Filter => FilterType::Filter,
            _ => return Err(ParseError::Todo),
        };

        let ident = self.identifier()?;
        let for_ident = self.for_statement()?;
        let with_kv = self.with_statement()?;
        let body = self.filter_map_body()?;

        Ok(FilterMap {
            ty,
            ident,
            for_ident,
            with_kv,
            body,
        })
    }

    /// Parse the body of a filter-map or filter
    ///
    /// ```ebnf
    /// FilterMapBody ::= '{' Define? FilterMapExpr+ Apply? '}'
    /// Define        ::= 'define' For With DefineBody
    /// Apply         ::= 'apply' For With ApplyBody
    /// ```
    ///
    /// Not shown in the EBNF above, but the location of the define and apply
    /// sections doesn't matter, but they can both only appear once.
    fn filter_map_body(&mut self) -> ParseResult<FilterMapBody> {
        let mut define = None;
        let mut _expressions: Option<Vec<FilterMapExpr>> = None;
        let mut apply = None;

        self.accept_required(Token::CurlyLeft)?;

        while self.accept_optional(Token::CurlyRight)?.is_none() {
            if self.accept_optional(Token::Define)?.is_some() {
                if define.is_some() {
                    // Cannot have multiple define sections
                    return Err(ParseError::Todo);
                }
                let for_kv = self.for_statement()?;
                let with_kv = self.with_statement()?;
                let body = self.define_body()?;
                define = Some(Define {
                    for_kv,
                    with_kv,
                    body,
                });
            } else if self.accept_optional(Token::Apply)?.is_some() {
                if apply.is_some() {
                    // Cannot have multiple apply sections
                    return Err(ParseError::Todo);
                }
                let for_kv = self.for_statement()?;
                let with_kv = self.with_statement()?;
                let body = self.apply_body()?;
                apply = Some(ApplySection {
                    for_kv,
                    with_kv,
                    body,
                });
            } else {
                todo!("parse expr")
            }
        }

        Ok(FilterMapBody {
            define: define.ok_or(ParseError::Todo)?,
            expressions: _expressions.unwrap_or_default(),
            apply,
        })
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
            _ => return Err(ParseError::Todo),
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

        Ok(ApplyBody {
            scopes,
            accept_reject,
        })
    }

    /// Parse a scope of the body of apply
    ///
    /// ```ebnf
    /// ApplyScope ::= Use?
    ///                'filter' MatchOperator ValueExpr
    ///                'not'? 'matching'
    ///                Actions ';'
    ///
    /// ApplyUse   ::= 'use' Identifier ';'
    /// Actions    ::= '{' Action* '}'
    /// Action     ::= ValueExpr ';' ( AcceptReject ';' )?
    /// ```
    fn apply_scope(&mut self) -> ParseResult<ApplyScope> {
        let scope = self
            .accept_optional(Token::Use)?
            .map(|_| {
                let id = self.identifier()?;
                self.accept_required(Token::SemiColon)?;
                Ok(id)
            })
            .transpose()?;

        self.accept_required(Token::Filter)?;
        let operator = self.match_operator()?;
        let filter_ident = self.value_expr()?;
        let negate = self.accept_optional(Token::Not)?.is_some();
        self.accept_required(Token::Matching)?;
        self.accept_required(Token::CurlyLeft)?;

        // TODO: This part needs to be checked by Jasper
        // The original seems to have been:
        // (ValueExpr ';' (AcceptReject ';')? )+ | AcceptReject ';'
        // That does not seem to make a whole lot of sense. Probably,
        // some filter is applied later, but we could probably be more
        // precise while parsing.
        let mut actions = Vec::new();
        while self.accept_optional(Token::CurlyRight)?.is_none() {
            // We can try to parse a value expr first
            let val = if !matches!(self.peek(), Some(Token::Return)) {
                let val = self.value_expr()?;
                self.accept_required(Token::SemiColon)?;
                Some(val)
            } else {
                None
            };

            let accept_reject = self.accept_reject()?;
            actions.push((val, accept_reject));
        }

        self.accept_required(Token::SemiColon)?;

        Ok(ApplyScope {
            scope,
            match_action: MatchActionExpr::FilterMatchAction(
                FilterMatchActionExpr {
                    operator,
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
                _ => return Err(ParseError::Todo),
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
            _ => return Err(ParseError::Todo),
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
            if self.peek_is(Token::CurlyLeft) {
                return Ok(ValueExpr::TypedRecordExpr(
                    TypedRecordValueExpr {
                        type_id: self.type_identifier()?,
                        key_values: self.record()?,
                    },
                ));
            }

            if self.peek_is(Token::RoundLeft) {
                return Ok(ValueExpr::RootMethodCallExpr(
                    self.method_call()?,
                ));
            }

            let id = self.identifier()?;
            let receiver = AccessReceiver::Ident(id);
            let access_expr = self.access_expr()?;

            return Ok(ValueExpr::ComputeExpr(ComputeExpr {
                receiver,
                access_expr,
            }));
        }

        let literal = self.literal()?;

        // If we parsed a prefix, it may be followed by a prefix match
        if let LiteralExpr::PrefixLiteral(prefix) = literal {
            if let Some(ty) = self.prefix_match_type()? {
                return Ok(ValueExpr::PrefixMatchExpr(PrefixMatchExpr {
                    prefix,
                    ty,
                }));
            }
            todo!()
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
            if self.peek_is(Token::CurlyLeft) {
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

    fn record_type_assignment(
        &mut self,
    ) -> ParseResult<RecordTypeAssignment> {
        todo!()
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
            let _ip = self.ip_address()?;
            if self.accept_optional(Token::Slash)?.is_some() {
                let _prefix_len = self.prefix_length()?;
                todo!("return prefix literal")
            } else {
                todo!("return ip literal")
            }
        }

        self.simple_literal()
    }

    /// Parse literals that need no complex parsing, just one token
    fn simple_literal(&mut self) -> ParseResult<LiteralExpr> {
        // TODO: Make proper errors using the spans
        Ok(match self.next()?.0 {
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
            _ => return Err(ParseError::Todo),
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

            _ => Err(ParseError::Todo),
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
            _ => return Err(ParseError::Todo),
        })
    }

    // TODO: I'm ignoring the grammar difference between identifiers and type
    // identifiers because it doesn't really make sense, I think. But this
    // comment is here to remind me of that before I put this thing up for
    // review. Otherwise the lexer will get a bit more complicated.
    fn identifier(&mut self) -> ParseResult<Identifier> {
        let (token, span) = self.next()?;
        match token {
            Token::Ident(s) => Ok(Identifier { ident: s.into() }),
            _ => Err(ParseError::Expected(span)),
        }
    }

    fn type_identifier(&mut self) -> ParseResult<TypeIdentifier> {
        let (token, span) = self.next()?;
        match token {
            Token::Ident(s) => Ok(TypeIdentifier { ident: s.into() }),
            _ => Err(ParseError::Expected(span)),
        }
    }
}
