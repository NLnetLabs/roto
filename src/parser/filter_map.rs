use crate::{
    ast::{
        AcceptReject, AccessExpr, AccessReceiver, ActionCallExpr, ActionSection, ActionSectionBody, AndExpr, ApplyBody, ApplyScope, ApplySection, BooleanExpr, CompareArg, CompareExpr, CompareOp, ComputeExpr, Define, DefineBody, FilterMap, FilterMapBody, FilterMapExpr, FilterMatchActionExpr, FilterType, GroupedLogicalExpr, ListCompareExpr, LogicalExpr, MatchActionExpr, MatchOperator, NotExpr, OrExpr, PatternMatchActionArm, PatternMatchActionExpr, RxTxType, TermBody, TermCallExpr, TermPatternMatchArm, TermScope, TermSection, TypeIdentField, ValueExpr
    },
    token::Token,
};

use super::{ParseError, ParseResult, Parser};

impl<'source> Parser<'source> {
    /// Parse a filter-map or filter expression
    ///
    /// ```ebnf
    /// FilterMap ::= ( 'filter-map' | 'filter' ) Identifier
    ///               For With FilterMapBody
    /// ```
    pub(super) fn filter_map(&mut self) -> ParseResult<FilterMap> {
        let (token, _span) = self.next()?;
        let ty = match token {
            Token::FilterMap => FilterType::FilterMap,
            Token::Filter => FilterType::Filter,
            _ => return Err(ParseError::Todo(1)),
        };

        let ident = self.identifier()?;
        let for_ident = self.for_clause()?;
        let with_kv = self.with_clause()?;
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
        let mut expressions: Vec<FilterMapExpr> = Vec::new();
        let mut apply = None;

        self.accept_required(Token::CurlyLeft)?;

        while self.accept_optional(Token::CurlyRight)?.is_none() {
            if self.accept_optional(Token::Define)?.is_some() {
                if define.is_some() {
                    // Cannot have multiple define sections
                    return Err(ParseError::Todo(2));
                }
                let for_kv = self.for_clause()?;
                let with_kv = self.with_clause()?;
                let body = self.define_body()?;
                define = Some(Define {
                    for_kv,
                    with_kv,
                    body,
                });
            } else if self.accept_optional(Token::Apply)?.is_some() {
                if apply.is_some() {
                    // Cannot have multiple apply sections
                    return Err(ParseError::Todo(3));
                }
                let for_kv = self.for_clause()?;
                let with_kv = self.with_clause()?;
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

        Ok(FilterMapBody {
            define: define.ok_or(ParseError::Todo(4))?,
            expressions,
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
            || self.peek_is(Token::Accept)
            || self.peek_is(Token::Reject)
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
        if self.peek_is(Token::Match) {
            return self.apply_match();
        }

        self.accept_required(Token::Filter)?;

        // This is not exactly self.match_operator because match ... with is
        // not allowed.
        let operator = if self.accept_optional(Token::Match)?.is_some() {
            MatchOperator::Match
        } else if self.accept_optional(Token::ExactlyOne)?.is_some() {
            MatchOperator::ExactlyOne
        } else if self.accept_optional(Token::Some)?.is_some() {
            MatchOperator::Some
        } else if self.accept_optional(Token::All)?.is_some() {
            MatchOperator::All
        } else {
            return Err(ParseError::Todo(20));
        };

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
                    operator,
                    negate,
                    actions,
                    filter_ident,
                },
            ),
        })
    }

    fn apply_match(&mut self) -> ParseResult<ApplyScope> {
        let operator = self.match_operator()?;
        let mut match_arms = Vec::new();
        self.accept_required(Token::CurlyLeft)?;
        while self.accept_optional(Token::CurlyRight)?.is_none() {
            let variant_id = self.identifier()?;
            let data_field =
                if self.accept_optional(Token::RoundLeft)?.is_some() {
                    let id = self.identifier()?;
                    self.accept_required(Token::RoundRight)?;
                    Some(id)
                } else {
                    None
                };

            let guard = if self.accept_optional(Token::Pipe)?.is_some() {
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

            self.accept_required(Token::Arrow)?;

            let mut actions = Vec::new();
            if self.accept_optional(Token::CurlyLeft)?.is_some() {
                while self.accept_optional(Token::CurlyRight)?.is_none() {
                    if let Some(ar) = self.accept_reject()? {
                        self.accept_required(Token::CurlyRight)?;
                        actions.push((None, Some(ar)));
                        break;
                    }
                    actions.push((Some(self.action_call_expr()?), None));
                    self.accept_required(Token::SemiColon)?;
                }
                self.accept_optional(Token::Comma)?;
            } else {
                let expr = self.action_call_expr()?;
                self.accept_required(Token::Comma)?;
                actions.push((Some(expr), None));
            }

            match_arms.push(PatternMatchActionArm {
                variant_id,
                data_field,
                guard,
                actions,
            });
        }
        Ok(ApplyScope {
            scope: None,
            match_action: MatchActionExpr::PatternMatchAction(
                PatternMatchActionExpr {
                    operator,
                    match_arms,
                },
            ),
        })
    }

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
    fn accept_reject(&mut self) -> ParseResult<Option<AcceptReject>> {
        if self.accept_optional(Token::Return)?.is_some() {
            let value = match self.next()?.0 {
                Token::Accept => AcceptReject::Accept,
                Token::Reject => AcceptReject::Reject,
                _ => return Err(ParseError::Todo(10)),
            };
            self.accept_required(Token::SemiColon)?;
            return Ok(Some(value));
        }

        if self.accept_optional(Token::Accept)?.is_some() {
            self.accept_required(Token::SemiColon)?;
            return Ok(Some(AcceptReject::Accept));
        }

        if self.accept_optional(Token::Reject)?.is_some() {
            self.accept_required(Token::SemiColon)?;
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

    /// Parse a filter map expression, which is a term or an action
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
            Err(ParseError::Todo(5))
        }
    }

    fn term(&mut self) -> ParseResult<TermSection> {
        self.accept_required(Token::Term)?;
        let ident = self.identifier()?;
        let for_kv = self.for_clause()?;
        let with_kv = self.with_clause()?;

        let mut scopes = Vec::new();
        self.accept_required(Token::CurlyLeft)?;
        while self.accept_optional(Token::CurlyRight)?.is_none() {
            scopes.push(self.term_scope()?);
        }

        Ok(TermSection {
            ident,
            for_kv,
            with_kv,
            body: TermBody { scopes },
        })
    }

    fn term_scope(&mut self) -> ParseResult<TermScope> {
        let operator = self.match_operator()?;

        // In this case, we'll start pattern matching, otherwise,
        // the match will contain logical expressions.
        if let MatchOperator::MatchValueWith(_) = operator {
            let mut match_arms = Vec::new();
            self.accept_required(Token::CurlyLeft)?;
            while self.accept_optional(Token::CurlyRight)?.is_none() {
                let (pattern, expr) = self.match_arm()?;
                match_arms.push((Some(pattern), expr));
            }

            Ok(TermScope {
                // TODO: remove the scope field (and rename this type probably)
                scope: None,
                operator,
                match_arms,
            })
        } else {
            self.accept_required(Token::CurlyLeft)?;
            let mut match_arms = Vec::new();
            while self.accept_optional(Token::CurlyRight)?.is_none() {
                match_arms.push((None, vec![self.logical_expr()?]));
                self.accept_required(Token::SemiColon)?;
            }
            Ok(TermScope {
                scope: None,
                operator,
                match_arms,
            })
        }
    }

    pub(super) fn match_arm(
        &mut self,
    ) -> ParseResult<(TermPatternMatchArm, Vec<LogicalExpr>)> {
        let variant_id = self.identifier()?;

        let data_field = self
            .accept_optional(Token::RoundLeft)?
            .map(|_| {
                let field = self.identifier()?;
                self.accept_required(Token::RoundRight)?;
                Ok(field)
            })
            .transpose()?;

        self.accept_required(Token::Arrow)?;

        let mut expr = Vec::new();
        if self.accept_optional(Token::CurlyLeft)?.is_some() {
            while self.accept_optional(Token::CurlyRight)?.is_none() {
                expr.push(self.logical_expr()?);
                self.accept_required(Token::SemiColon)?;
            }
            self.accept_optional(Token::Comma)?;
        } else {
            expr.push(self.logical_expr()?);
            // This comma might need to be optional, but it's probably good
            // practice to require it.
            self.accept_required(Token::Comma)?;
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
        if self.accept_optional(Token::Bang)?.is_some() {
            let expr = self.boolean_expr()?;
            return Ok(LogicalExpr::NotExpr(NotExpr { expr }));
        }

        let left = self.boolean_expr()?;

        Ok(if self.accept_optional(Token::PipePipe)?.is_some() {
            let right = self.boolean_expr()?;
            LogicalExpr::OrExpr(OrExpr { left, right })
        } else if self.accept_optional(Token::AmpAmp)?.is_some() {
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

        if let Some(op) = self.try_compare_operator()? {
            if op == CompareOp::In || op == CompareOp::NotIn {
                let CompareArg::ValueExpr(left) = left else {
                    return Err(ParseError::Todo(16));
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
            | ValueExpr::ListExpr(_) => return Err(ParseError::Todo(6)),
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
    /// going to be a comparison operator.
    ///
    /// ```ebnf
    /// CompareOp ::= '==' | '!=' | '<' | '<=' | '>' | '>=' | 'not'? 'in'
    /// ```
    fn try_compare_operator(&mut self) -> ParseResult<Option<CompareOp>> {
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
                self.accept_required(Token::Not)?;
                self.accept_required(Token::In)?;
                return Ok(Some(CompareOp::NotIn));
            }
            _ => return Ok(None),
        };

        self.next()?;
        Ok(Some(op))
    }

    fn grouped_logical_expr(&mut self) -> ParseResult<GroupedLogicalExpr> {
        self.accept_required(Token::RoundLeft)?;
        let expr = self.logical_expr()?;
        self.accept_required(Token::RoundRight)?;
        Ok(GroupedLogicalExpr {
            expr: Box::new(expr),
        })
    }
    
    pub(super) fn action(&mut self) -> ParseResult<ActionSection> {
        self.accept_required(Token::Action)?;
        let ident = self.identifier()?;
        let with_kv = self.with_clause()?;

        let mut expressions = Vec::new();
        self.accept_required(Token::CurlyLeft)?;
        while self.accept_optional(Token::CurlyRight)?.is_none() {
            let value_expr = self.value_expr()?;
            self.accept_required(Token::SemiColon)?;
            match value_expr {
                ValueExpr::ComputeExpr(x) => expressions.push(x),
                ValueExpr::RootMethodCallExpr(x) => {
                    expressions.push(ComputeExpr {
                        receiver: AccessReceiver::GlobalScope,
                        access_expr: vec![AccessExpr::MethodComputeExpr(x)],
                    })
                }
                _ => return Err(ParseError::Todo(7)),
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
    fn for_clause(&mut self) -> ParseResult<Option<TypeIdentField>> {
        if self.accept_optional(Token::For)?.is_some() {
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
    fn with_clause(&mut self) -> ParseResult<Vec<TypeIdentField>> {
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
