use crate::ast::{
    ActionDeclaration, Define, DefineBody, FilterMap, FilterMapBody,
    FilterMapExpr, FilterType, Identifier, RxTxType, TermDeclaration,
};

use super::{
    meta::{Meta, Span},
    token::Token,
    ParseError, ParseResult, Parser,
};

/// # Parsing `filter-map` and `filter` sections
impl<'source> Parser<'source, '_> {
    /// Parse a filter-map or filter expression
    ///
    /// ```ebnf
    /// FilterMap ::= ( 'filter-map' | 'filter' ) Identifier
    ///               For? With? FilterMapBody
    /// ```
    pub(super) fn filter_map(&mut self) -> ParseResult<FilterMap> {
        let (token, span) = self.next()?;
        let filter_type = match token {
            Token::FilterMap => FilterType::FilterMap,
            Token::Filter => FilterType::Filter,
            _ => {
                return Err(ParseError::expected(
                    "'filter-map' or 'filter'",
                    token,
                    span,
                ))
            }
        };

        let ident = self.identifier()?;
        let params = self.params()?;
        let body = self.filter_map_body(span)?;

        Ok(FilterMap {
            filter_type,
            ident,
            params,
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
                    return Err(ParseError::custom(
                        "a filter or filter-map cannot have multiple define sections",
                        "merge this define section with the previous one",
                        span,
                    ));
                }
                let body = self.define_body()?;
                define = Some(Define { body });
            } else if self.peek_is(Token::Apply) {
                let (_, span) = self.next()?;
                if apply.is_some() {
                    // Cannot have multiple apply sections
                    return Err(ParseError::custom(
                        "a filter or filter-map cannot have multiple apply sections",
                        "merge this apply section with the previous one",
                        span,
                    ));
                }
                apply = Some(self.block()?);
            } else {
                expressions.push(self.filter_map_expr()?);
            }
        }

        let Some(define) = define else {
            return Err(ParseError::custom(
                "a filter or filter-map requires at \
                    least one define section",
                "this filter or filter-map is missing a define section",
                span,
            ));
        };

        let Some(apply) = apply else {
            return Err(ParseError::custom(
                "a filter or filter-map requires at \
                    least one apply section",
                "this filter or filter-map is missing an apply section",
                span,
            ));
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
            Token::Rx => {
                let rx_field = self.type_ident_field()?;
                self.take(Token::SemiColon)?;
                if self.next_is(Token::Tx) {
                    let tx_field = self.type_ident_field()?;
                    self.take(Token::SemiColon)?;
                    RxTxType::Split {
                        rx: rx_field,
                        tx: tx_field,
                    }
                } else {
                    RxTxType::RxOnly(rx_field.0, rx_field.1)
                }
            }
            _ => {
                return Err(ParseError::expected(
                    "`rx` or `rx_tx`",
                    token,
                    span,
                ))
            }
        };

        let mut use_ext_data = Vec::new();
        while self.next_is(Token::Use) {
            use_ext_data
                .push((self.identifier()?.node, self.identifier()?.node));
            self.take(Token::SemiColon)?;
        }

        let mut assignments = Vec::new();
        while !self.next_is(Token::CurlyRight) {
            let id = self.identifier()?;
            self.take(Token::Eq)?;
            let value = self.expr()?;
            self.take(Token::SemiColon)?;
            assignments.push((id, value));
        }

        Ok(DefineBody {
            rx_tx_type,
            assignments,
        })
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
            Err(ParseError::expected("`term` or `action`", token, span))
        }
    }

    /// Parse a term section
    ///
    /// ```ebnf
    /// Term ::= Identifier For? With? '{' TermScope '}'
    /// ```
    fn term(&mut self) -> ParseResult<TermDeclaration> {
        self.take(Token::Term)?;
        let ident = self.identifier()?;
        let params = self.params()?;
        let body = self.block()?;

        Ok(TermDeclaration {
            ident,
            params,
            body,
        })
    }

    pub(super) fn action(&mut self) -> ParseResult<ActionDeclaration> {
        self.take(Token::Action)?;
        let ident = self.identifier()?;
        let params = self.params()?;
        let body = self.block()?;

        Ok(ActionDeclaration {
            ident,
            params,
            body,
        })
    }

    /// Parse an optional with clause for filter-map, define and apply
    ///
    /// ```ebnf
    /// With ::= ( 'with' TypeIdentField (',' TypeIdentField)*)?
    /// ```
    fn params(
        &mut self,
    ) -> ParseResult<Meta<Vec<(Meta<Identifier>, Meta<Identifier>)>>>
    {
        self.separated(
            Token::RoundLeft,
            Token::RoundRight,
            Token::Comma,
            Self::type_ident_field,
        )
    }

    /// Parse an identifier and a type identifier separated by a colon
    ///
    /// ```ebnf
    /// TypeIdentField ::= Identifier ':' TypeIdentifier
    /// ```
    fn type_ident_field(
        &mut self,
    ) -> ParseResult<(Meta<Identifier>, Meta<Identifier>)> {
        let field_name = self.identifier()?;
        self.take(Token::Colon)?;
        let ty = self.identifier()?;
        Ok((field_name, ty))
    }
}
