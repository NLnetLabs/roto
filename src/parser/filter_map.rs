use crate::ast::{
    AcceptReject, AccessExpr, AccessReceiver, ActionCallExpr, ActionSection,
    ActionSectionBody, AndExpr, ApplyBody, ApplyScope, ApplySection,
    BooleanExpr, CompareArg, CompareExpr, CompareOp, ComputeExpr, Define,
    DefineBody, FilterMap, FilterMapBody, FilterMapExpr,
    FilterMatchActionExpr, FilterType, GroupedLogicalExpr, ListCompareExpr,
    LogicalExpr, MatchActionExpr, MatchOperator, NotExpr, OrExpr,
    PatternMatchActionArm, PatternMatchActionExpr, RxTxType, TermBody,
    TermCallExpr, TermPatternMatchArm, TermScope, TermSection,
    TypeIdentField, ValueExpr,
};

use super::{span::Span, token::Token, ParseError, ParseResult, Parser};

/// # Parsing `filter-map` and `filter` sections
impl<'source> Parser<'source> {
    /// Parse a filter-map or filter expression
    ///
    /// ```ebnf
    /// FilterMap ::= ( 'filter-map' | 'filter' ) Identifier
    ///               For? With? FilterMapBody
    /// ```
    pub(super) fn filter_map(&mut self) -> ParseResult<FilterMap> {
        let (token, span) = self.next()?;
        let ty = match token {
            Token::FilterMap => FilterType::FilterMap,
            Token::Filter => FilterType::Filter,
            _ => {
                return Err(ParseError::Expected {
                    expected: "'filter-map' or 'filter'".into(),
                    got: token.to_string(),
                    span,
                })
            }
        };

        let ident = self.identifier()?;
        let for_ident = self.try_for_clause()?;
        let with_kv = self.try_with_clause()?;
        let body = self.filter_map_body(span)?;

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
    /// FilterMapBody ::= '{' Define FilterMapExpr+ Apply? '}'
    /// Define        ::= 'define' For? With? DefineBody
    /// Apply         ::= 'apply' For? With? ApplyBody
    /// ```
    ///
    /// Not shown in the EBNF above, but the location of the define and apply
    /// sections doesn't matter, but they can both only appear once.
    fn filter_map_body(&mut self, span: Span) -> ParseResult<FilterMapBody> {
        let mut define = None;
        let mut expressions: Vec<FilterMapExpr> = Vec::new();
        let mut apply = None;

        self.take(Token::CurlyLeft)?;

        while !self.next_is(Token::CurlyRight) {
            if self.peek_is(Token::Define) {
                let (_, span) = self.next()?;
                if define.is_some() {
                    // Cannot have multiple define sections
                    return Err(ParseError::Custom {
                        description: "a filter or filter-map cannot have multiple define sections".into(),
                        label: "merge this define section with the previous one".into(),
                        span,
                    });
                }
                let for_kv = self.try_for_clause()?;
                let with_kv = self.try_with_clause()?;
                let body = self.define_body()?;
                define = Some(Define {
                    for_kv,
                    with_kv,
                    body,
                });
            } else if self.peek_is(Token::Apply) {
                let (_, span) = self.next()?;
                if apply.is_some() {
                    // Cannot have multiple apply sections
                    return Err(ParseError::Custom {
                        description: "a filter or filter-map cannot have multiple apply sections".into(),
                        label: "merge this apply section with the previous one".into(),
                        span,
                    });
                }
                let for_kv = self.try_for_clause()?;
                let with_kv = self.try_with_clause()?;
                let body = self.apply_body()?;
                apply = Some(ApplySection {
                    for_kv,
                    with_kv,
                    body,
                });
            } else {
                expressions.push(self.filter_map_expr()?);
            }
        }

        let Some(define) = define else {
            return Err(ParseError::Custom {
                description: "a filter or filter-map requires at\
                    least one define section"
                    .into(),
                label:
                    "this filter or filter-map is missing a define section"
                        .into(),
                span,
            });
        };
        Ok(FilterMapBody {
            define,
            expressions,
            apply,
        })
    }

    /// Parse the body of a define section
    ///
    /// ```ebnf
    /// DefineBody ::= '{' RxTxType Use? Assignment* '}'
    ///
    /// RxTxType   ::= 'rx_tx' TypeIdentField ';'
    ///              | 'rx' TypeIdentField ';' 'tx' TypeIdentField ';'
    ///              | 'rx' TypeIdentField ';'
    ///
    /// Use        ::= 'use' Identifier Identifier
    ///
    /// Assignment ::= Identifier '=' ValueExpr ';'
    /// ```
    fn define_body(&mut self) -> ParseResult<DefineBody> {
        self.take(Token::CurlyLeft)?;

        let (token, span) = self.next()?;
        let rx_tx_type = match token {
            Token::RxTx => {
                let field = self.type_ident_field()?;
                self.take(Token::SemiColon)?;
                RxTxType::PassThrough(field)
            }
            Token::Rx => {
                let rx_field = self.type_ident_field()?;
                self.take(Token::SemiColon)?;
                if self.next_is(Token::Tx) {
                    let tx_field = self.type_ident_field()?;
                    self.take(Token::SemiColon)?;
                    RxTxType::Split(rx_field, tx_field)
                } else {
                    RxTxType::RxOnly(rx_field)
                }
            }
            _ => {
                return Err(ParseError::Expected {
                    expected: "'rx' or 'rx_tx'".into(),
                    got: token.to_string(),
                    span,
                })
            }
        };

        let mut use_ext_data = Vec::new();
        while self.next_is(Token::Use) {
            use_ext_data
                .push((self.identifier()?.inner, self.identifier()?.inner));
            self.take(Token::SemiColon)?;
        }

        let mut assignments = Vec::new();
        while !self.next_is(Token::CurlyRight) {
            let id = self.identifier()?;
            self.take(Token::Eq)?;
            let value = self.value_expr()?;
            self.take(Token::SemiColon)?;
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
        self.take(Token::CurlyLeft)?;
        let mut scopes = Vec::new();

        while !(self.peek_is(Token::Return)
            || self.peek_is(Token::Accept)
            || self.peek_is(Token::Reject)
            || self.peek_is(Token::CurlyRight))
        {
            scopes.push(self.apply_scope()?);
        }

        let accept_reject = self.try_accept_reject()?;
        self.take(Token::CurlyRight)?;
        Ok(ApplyBody {
            scopes,
            accept_reject,
        })
    }

    /// Parse a scope of the body of apply
    ///
    /// ```ebnf
    /// ApplyScope ::= ApplyFilter | ApplyMatch
    /// ```
    fn apply_scope(&mut self) -> ParseResult<ApplyScope> {
        let match_action = if self.peek_is(Token::Match) {
            MatchActionExpr::PatternMatchAction(self.apply_match()?)
        } else if self.peek_is(Token::Filter) {
            MatchActionExpr::FilterMatchAction(self.apply_filter()?)
        } else {
            let (token, span) = self.next()?;
            return Err(ParseError::Expected {
                expected: "'match' or 'filter'".to_string(),
                got: token.to_string(),
                span,
            });
        };

        Ok(ApplyScope {
            scope: None,
            match_action,
        })
    }

    /// Parse a match block in an apply section
    ///
    /// This differs from the other match construct because it can have
    /// guards and the expressions are only action call expressions.
    ///
    /// ```ebnf
    /// ApplyMatch        ::= MatchOperator '{' ApplyMatchArm* '}'
    /// ApplyMatchArm     ::= Identifier ( '(' Identifier ')' )? Guard? '->'
    /// Guard             ::= '|' Identifier '(' (ValueExpr (',' ValueExpr)* ',')? ')'
    /// ApplyMatchActions ::= ActionCallExpr ','
    ///                     | '{' (ActionCallExpr ';')* (AcceptReject ';' )? '}' ','?
    /// ```
    fn apply_match(&mut self) -> ParseResult<PatternMatchActionExpr> {
        let operator = self.match_operator()?;
        let mut match_arms = Vec::new();
        self.take(Token::CurlyLeft)?;
        while !self.next_is(Token::CurlyRight) {
            let variant_id = self.identifier()?;
            let data_field = if self.next_is(Token::RoundLeft) {
                let id = self.identifier()?;
                self.take(Token::RoundRight)?;
                Some(id)
            } else {
                None
            };

            let guard = if self.next_is(Token::Pipe) {
                let term_id = self.identifier()?;
                let args = if self.peek_is(Token::RoundLeft) {
                    Some(self.arg_expr_list()?)
                } else {
                    None
                };
                Some(TermCallExpr { term_id, args })
            } else {
                None
            };

            self.take(Token::Arrow)?;

            let mut actions = Vec::new();
            if self.next_is(Token::CurlyLeft) {
                while !self.next_is(Token::CurlyRight) {
                    if let Some(ar) = self.try_accept_reject()? {
                        self.take(Token::CurlyRight)?;
                        actions.push((None, Some(ar)));
                        break;
                    }
                    actions.push((Some(self.action_call_expr()?), None));
                    self.take(Token::SemiColon)?;
                }
                self.next_is(Token::Comma);
            } else {
                let expr = self.action_call_expr()?;
                self.take(Token::Comma)?;
                actions.push((Some(expr), None));
            }

            match_arms.push(PatternMatchActionArm {
                variant_id,
                data_field,
                guard,
                actions,
            });
        }
        Ok(PatternMatchActionExpr {
            operator,
            match_arms,
        })
    }

    /// Parse a filter block in an apply section
    ///
    /// ```ebnf
    /// ApplyFilter        ::= 'filter' 'match' ValueExpr
    ///                        'not'? 'matching'
    ///                        ApplyFilterActions ';'
    /// ApplyFilterActions ::= '{' (ValueExpr ';')* ( AcceptReject ';' )? '}'
    /// ```
    fn apply_filter(&mut self) -> ParseResult<FilterMatchActionExpr> {
        self.take(Token::Filter)?;

        // This is not exactly self.match_operator because match ... with is
        // not allowed.
        let operator = if self.next_is(Token::Match) {
            MatchOperator::Match
        } else if self.next_is(Token::ExactlyOne) {
            MatchOperator::ExactlyOne
        } else if self.next_is(Token::Some) {
            MatchOperator::Some
        } else if self.next_is(Token::All) {
            MatchOperator::All
        } else {
            let (token, span) = self.next()?;
            return Err(ParseError::Expected {
                expected: "'match', 'exactly-one', 'some' or 'all'".into(),
                got: token.to_string(),
                span,
            });
        };

        let filter_ident = self.value_expr()?;
        let negate = self.next_is(Token::Not);
        self.take(Token::Matching)?;
        self.take(Token::CurlyLeft)?;

        let mut actions = Vec::new();
        while !self.next_is(Token::CurlyRight) {
            if let Some(accept_reject) = self.try_accept_reject()? {
                self.take(Token::CurlyRight)?;
                actions.push((None, Some(accept_reject)));
                break;
            }

            let val = self.value_expr()?;
            self.take(Token::SemiColon)?;
            actions.push((Some(val), None));
        }

        self.take(Token::SemiColon)?;

        Ok(FilterMatchActionExpr {
            operator,
            negate,
            actions,
            filter_ident,
        })
    }

    /// Parse an action call expr, resembling a method call
    ///
    /// ```ebnf
    /// ActionCallExpr ::= Identifier '(' (ValueExpr (',' ValueExpr)* ',')? ')'
    /// ```
    fn action_call_expr(&mut self) -> ParseResult<ActionCallExpr> {
        let action_id = self.identifier()?;
        let args = if self.peek_is(Token::RoundLeft) {
            Some(self.arg_expr_list()?)
        } else {
            None
        };
        Ok(ActionCallExpr { action_id, args })
    }

    /// Parse a statement returning accept or reject
    ///
    /// ```ebnf
    /// AcceptReject ::= ('return'? ( 'accept' | 'reject' ) ';')?
    /// ```
    fn try_accept_reject(&mut self) -> ParseResult<Option<AcceptReject>> {
        if self.next_is(Token::Return) {
            let (token, span) = self.next()?;
            let value = match token {
                Token::Accept => AcceptReject::Accept,
                Token::Reject => AcceptReject::Reject,
                _ => {
                    return Err(ParseError::Expected {
                        expected: "'accept' or 'reject' after 'return'"
                            .into(),
                        got: token.to_string(),
                        span,
                    })
                }
            };
            self.take(Token::SemiColon)?;
            return Ok(Some(value));
        }

        if self.next_is(Token::Accept) {
            self.take(Token::SemiColon)?;
            return Ok(Some(AcceptReject::Accept));
        }

        if self.next_is(Token::Reject) {
            self.take(Token::SemiColon)?;
            return Ok(Some(AcceptReject::Reject));
        }

        Ok(None)
    }

    /// Parse a match operator
    ///
    /// ```ebnf
    /// MatchOperator ::= 'match' ( Identifier 'with' )?
    ///                 | 'some' | 'exactly-one' | 'all'
    /// ```
    fn match_operator(&mut self) -> ParseResult<MatchOperator> {
        let (token, span) = self.next()?;
        let op = match token {
            Token::Match => {
                if matches!(self.peek(), Some(Token::Ident(_))) {
                    let ident = self.identifier()?;
                    self.take(Token::With)?;
                    MatchOperator::MatchValueWith(ident)
                } else {
                    MatchOperator::Match
                }
            }
            Token::Some => MatchOperator::Some,
            Token::ExactlyOne => MatchOperator::ExactlyOne,
            Token::All => MatchOperator::All,
            _ => {
                return Err(ParseError::Expected {
                    expected: "'match', 'exactly-one', 'some' or 'all'"
                        .into(),
                    got: token.to_string(),
                    span,
                })
            }
        };

        Ok(op)
    }

    /// Parse a filter map section, which is a term or an action
    ///
    /// ```ebnf
    /// FilterMapExpr ::= Term | Action
    /// ```
    fn filter_map_expr(&mut self) -> ParseResult<FilterMapExpr> {
        if self.peek_is(Token::Term) {
            Ok(FilterMapExpr::Term(self.term()?))
        } else if self.peek_is(Token::Action) {
            Ok(FilterMapExpr::Action(self.action()?))
        } else {
            let (token, span) = self.next()?;
            Err(ParseError::Expected {
                expected: "'term' or 'action'".into(),
                got: token.to_string(),
                span,
            })
        }
    }

    /// Parse a term section
    ///
    /// ```ebnf
    /// Term ::= Identifier For? With? '{' TermScope '}'
    /// ```
    fn term(&mut self) -> ParseResult<TermSection> {
        self.take(Token::Term)?;
        let ident = self.identifier()?.inner;
        let for_kv = self.try_for_clause()?;
        let with_kv = self.try_with_clause()?;

        let mut scopes = Vec::new();
        self.take(Token::CurlyLeft)?;
        while !self.next_is(Token::CurlyRight) {
            scopes.push(self.term_scope()?);
        }

        Ok(TermSection {
            ident,
            for_kv,
            with_kv,
            body: TermBody { scopes },
        })
    }

    /// Parse a term scope
    ///
    /// ```ebnf
    /// TermScope ::= 'match' Identifier 'with' '{' MatchArm '}'
    ///             | 'match' '{' ( LogicalExpr ';' )* '}'
    /// ```
    fn term_scope(&mut self) -> ParseResult<TermScope> {
        let operator = self.match_operator()?;

        // In this case, we'll start pattern matching, otherwise,
        // the match will contain logical expressions.
        if let MatchOperator::MatchValueWith(_) = operator {
            let mut match_arms = Vec::new();
            self.take(Token::CurlyLeft)?;
            while !self.next_is(Token::CurlyRight) {
                let (pattern, expr) = self.match_arm()?;
                match_arms.push((Some(pattern), expr));
            }

            Ok(TermScope {
                // TODO: remove the scope field (and rename this type
                // probably
                scope: None,
                operator,
                match_arms,
            })
        } else {
            self.take(Token::CurlyLeft)?;
            let mut match_arms = Vec::new();
            while !self.next_is(Token::CurlyRight) {
                match_arms.push((None, vec![self.logical_expr()?]));
                self.take(Token::SemiColon)?;
            }
            Ok(TermScope {
                scope: None,
                operator,
                match_arms,
            })
        }
    }

    /// Parse a match arm
    ///
    /// ```ebnf
    /// MatchArm     ::= Identifier ( '(' Identifier ')' )? '->' MatchArmExpr
    /// MatchArmExpr ::= LogicalExpr ','
    ///                | '{' ( LogicalExpr ';' )* '}' ','?
    /// ```
    pub(super) fn match_arm(
        &mut self,
    ) -> ParseResult<(TermPatternMatchArm, Vec<LogicalExpr>)> {
        let variant_id = self.identifier()?;

        let data_field = if self.next_is(Token::RoundLeft) {
            let field = self.identifier()?;
            self.take(Token::RoundRight)?;
            Some(field)
        } else {
            None
        };

        self.take(Token::Arrow)?;

        let mut expr = Vec::new();
        if self.next_is(Token::CurlyLeft) {
            while !self.next_is(Token::CurlyRight) {
                expr.push(self.logical_expr()?);
                self.take(Token::SemiColon)?;
            }
            self.next_is(Token::Comma);
        } else {
            expr.push(self.logical_expr()?);
            // This comma might need to be optional, but it's probably good
            // practice to require it.
            self.take(Token::Comma)?;
        }

        Ok((
            TermPatternMatchArm {
                variant_id,
                data_field,
            },
            expr,
        ))
    }

    /// Parse a logical expression
    ///
    /// ```ebnf
    /// LogicalExpr ::= '!' BooleanExpr
    ///               | BooleanExpr '||' BooleanExpr
    ///               | BooleanExpr '&&' BooleanExpr
    ///               | BooleanExpr
    /// ```
    pub(super) fn logical_expr(&mut self) -> ParseResult<LogicalExpr> {
        if self.next_is(Token::Bang) {
            let expr = self.boolean_expr()?;
            return Ok(LogicalExpr::NotExpr(NotExpr { expr }));
        }

        let left = self.boolean_expr()?;

        Ok(if self.next_is(Token::PipePipe) {
            let right = self.boolean_expr()?;
            LogicalExpr::OrExpr(OrExpr { left, right })
        } else if self.next_is(Token::AmpAmp) {
            let right = self.boolean_expr()?;
            LogicalExpr::AndExpr(AndExpr { left, right })
        } else {
            LogicalExpr::BooleanExpr(left)
        })
    }

    /// Parse a boolean expression
    ///
    /// ```ebnf
    /// BooleanExpr ::= GroupedLogicalExpr
    ///               | CompareExpr
    ///               | ComputeExpr
    ///               | LiteralAccessExpr
    ///               | PrefixMatchExpr
    ///
    /// CompareExpr ::= CompareArg CompareOp CompareArg
    /// CompareArg  ::= ValueExpr | GroupedLogicalExpr
    /// ```
    fn boolean_expr(&mut self) -> ParseResult<BooleanExpr> {
        let left = self.logical_or_value_expr()?;

        if let Some((op, span)) = self.try_compare_operator()? {
            if op == CompareOp::In || op == CompareOp::NotIn {
                let CompareArg::ValueExpr(left) = left else {
                    let op = if op == CompareOp::NotIn {
                        "not in"
                    } else {
                        "in"
                    };
                    return Err(ParseError::Custom {
                        description: format!("the {op} operator cannot take a logical expression as an argument"),
                        label: "this operator only acts on lists".into(),
                        span,
                    });
                };
                let right = self.value_expr()?;
                return Ok(BooleanExpr::ListCompareExpr(Box::new(
                    ListCompareExpr { left, op, right },
                )));
            } else {
                let right = self.logical_or_value_expr()?;
                return Ok(BooleanExpr::CompareExpr(Box::new(CompareExpr {
                    left,
                    op,
                    right,
                })));
            }
        }

        // If it's not a compare expression, we need to filter out some
        // possibilities.
        // - A grouped logical expr does not appear in any of the other
        //   production rules, so we can return it directly.
        // - A value expr is too general and needs to be asserted to be one
        //   of the allowed constructs.
        let v = match left {
            CompareArg::GroupedLogicalExpr(l) => {
                return Ok(BooleanExpr::GroupedLogicalExpr(l))
            }
            CompareArg::ValueExpr(v) => v,
        };

        Ok(match v {
            ValueExpr::LiteralAccessExpr(x) => {
                BooleanExpr::LiteralAccessExpr(x)
            }
            ValueExpr::PrefixMatchExpr(x) => BooleanExpr::PrefixMatchExpr(x),
            ValueExpr::ComputeExpr(x) => BooleanExpr::ComputeExpr(x),
            ValueExpr::RootMethodCallExpr(_)
            | ValueExpr::AnonymousRecordExpr(_)
            | ValueExpr::TypedRecordExpr(_)
            | ValueExpr::ListExpr(_) => {
                // TODO: the span information should be better here.
                let (_token, span) = self.next()?;
                return Err(ParseError::Custom {
                    description: "not a valid boolean expression".into(),
                    label: "the expression ending here is not a valid boolean expression".into(),
                    span,
                });
            }
        })
    }

    fn logical_or_value_expr(&mut self) -> ParseResult<CompareArg> {
        Ok(if self.peek_is(Token::RoundLeft) {
            CompareArg::GroupedLogicalExpr(self.grouped_logical_expr()?)
        } else {
            CompareArg::ValueExpr(self.value_expr()?)
        })
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
    ) -> ParseResult<Option<(CompareOp, Span)>> {
        let Some(tok) = self.peek() else {
            return Ok(None);
        };

        let op = match tok {
            Token::EqEq => CompareOp::Eq,
            Token::BangEq => CompareOp::Ne,
            Token::AngleLeft => CompareOp::Lt,
            Token::AngleRight => CompareOp::Gt,
            Token::AngleLeftEq => CompareOp::Le,
            Token::AngleRightEq => CompareOp::Ge,
            Token::In => CompareOp::In,
            Token::Not => {
                let span1 = self.take(Token::Not)?;
                let span2 = self.take(Token::In)?;
                return Ok(Some((CompareOp::NotIn, span1.merge(span2))));
            }
            _ => return Ok(None),
        };

        let (_, span) = self.next()?;
        Ok(Some((op, span)))
    }

    fn grouped_logical_expr(&mut self) -> ParseResult<GroupedLogicalExpr> {
        self.take(Token::RoundLeft)?;
        let expr = self.logical_expr()?;
        self.take(Token::RoundRight)?;
        Ok(GroupedLogicalExpr {
            expr: Box::new(expr),
        })
    }

    pub(super) fn action(&mut self) -> ParseResult<ActionSection> {
        self.take(Token::Action)?;
        let ident = self.identifier()?.inner;
        let with_kv = self.try_with_clause()?;

        let mut expressions = Vec::new();
        self.take(Token::CurlyLeft)?;
        while !self.next_is(Token::CurlyRight) {
            let value_expr = self.value_expr()?;
            let span1 = self.take(Token::SemiColon)?;
            match value_expr {
                ValueExpr::ComputeExpr(x) => expressions.push(x),
                ValueExpr::RootMethodCallExpr(x) => {
                    expressions.push(ComputeExpr {
                        receiver: AccessReceiver::GlobalScope,
                        access_expr: vec![AccessExpr::MethodComputeExpr(x)],
                    })
                }
                _ => {
                    // TODO: span information could be better
                    let (_, span2) = self.next()?;
                    return Err(ParseError::Custom {
                        description: "an action can only be a compute epression or root method call".into(),
                        label: "invalid action".into(),
                        span: span1.merge(span2),
                    });
                }
            }
        }

        Ok(ActionSection {
            ident,
            with_kv,
            body: ActionSectionBody { expressions },
        })
    }

    /// Parse an optional for clause for filter-map, define and apply
    ///
    /// ```ebnf
    /// For ::= ( 'for' TypeIdentField)?
    /// ```
    fn try_for_clause(&mut self) -> ParseResult<Option<TypeIdentField>> {
        if self.next_is(Token::For) {
            Ok(Some(self.type_ident_field()?))
        } else {
            Ok(None)
        }
    }

    /// Parse an optional with clause for filter-map, define and apply
    ///
    /// ```ebnf
    /// With ::= ( 'with' TypeIdentField (',' TypeIdentField)*)?
    /// ```
    fn try_with_clause(&mut self) -> ParseResult<Vec<TypeIdentField>> {
        let mut key_values = Vec::new();

        if !self.next_is(Token::With) {
            return Ok(key_values);
        }

        key_values.push(self.type_ident_field()?);
        while self.next_is(Token::Comma) {
            key_values.push(self.type_ident_field()?);
        }

        Ok(key_values)
    }

    /// Parse an identifier and a type identifier separated by a colon
    ///
    /// ```ebnf
    /// TypeIdentField ::= Identifier ':' TypeIdentifier
    /// ```
    fn type_ident_field(&mut self) -> ParseResult<TypeIdentField> {
        let field_name = self.identifier()?;
        self.take(Token::Colon)?;
        let ty = self.type_identifier()?;
        Ok(TypeIdentField { field_name, ty })
    }
}
