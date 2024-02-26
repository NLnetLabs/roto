use crate::{
    ast::{
        ActionSection, AndExpr, ApplySection, BooleanExpr, CompareArg,
        CompareExpr, CompareOp, Define, FilterMap, FilterMapBody,
        FilterMapExpr, FilterType, GroupedLogicalExpr, LogicalExpr,
        MatchOperator, NotExpr, OrExpr, TermBody, TermPatternMatchArm,
        TermScope, TermSection, ValueExpr,
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
        let mut expressions: Vec<FilterMapExpr> = Vec::new();
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
                expressions.push(self.filter_map_expr()?);
            }
        }

        Ok(FilterMapBody {
            define: define.ok_or(ParseError::Todo)?,
            expressions,
            apply,
        })
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
            Err(ParseError::Todo)
        }
    }

    fn term(&mut self) -> ParseResult<TermSection> {
        self.accept_required(Token::Term)?;
        let ident = self.identifier()?;
        let for_kv = self.for_statement()?;
        let with_kv = self.with_statement()?;

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
                match_arms.push(self.match_arm()?);
            }

            Ok(TermScope {
                // TODO: remove the scope field (and rename this type probably)
                scope: None,
                operator,
                match_arms,
            })
        } else {
            let expr = self.logical_expr()?;
            self.accept_required(Token::SemiColon)?;
            Ok(TermScope {
                scope: None,
                operator,
                match_arms: vec![(None, vec![expr])],
            })
        }
    }

    fn match_arm(
        &mut self,
    ) -> ParseResult<(Option<TermPatternMatchArm>, Vec<LogicalExpr>)> {
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
        } else {
            expr.push(self.logical_expr()?);
            // This comma might need to be optional, but it's probably good
            // practice to require it.
            self.accept_required(Token::Comma)?;
        }

        Ok((
            Some(TermPatternMatchArm {
                variant_id,
                data_field,
            }),
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
    fn logical_expr(&mut self) -> ParseResult<LogicalExpr> {
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
            let right = self.logical_or_value_expr()?;
            return Ok(BooleanExpr::CompareExpr(Box::new(CompareExpr {
                left,
                op,
                right,
            })));
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
            | ValueExpr::ListExpr(_) => return Err(ParseError::Todo),
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
    /// This method returns option, because we are never sure that there is
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
                self.accept_required(Token::In)?;
                CompareOp::NotIn
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

    fn action(&mut self) -> ParseResult<ActionSection> {
        todo!()
    }
}
