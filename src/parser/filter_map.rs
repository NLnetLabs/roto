use crate::ast::{
    EnumTypeDeclaration, FilterMap, FilterType, Identifier, Param, Params,
    RecordTypeDeclaration, Variant,
};

use super::{
    ParseError, ParseResult, Parser,
    meta::Meta,
    token::{Keyword, Token},
};

/// # Parsing `filtermap` and `filter` sections
impl Parser<'_, '_> {
    /// Parse a filtermap or filter expression
    ///
    /// ```ebnf
    /// FilterMap ::= ( 'filtermap' | 'filter' ) Identifier Params RetType Block
    /// ```
    pub(super) fn filter_map(&mut self) -> ParseResult<FilterMap> {
        let (token, span) = self.next()?;
        let filter_type = match token {
            Token::Keyword(Keyword::FilterMap) => FilterType::FilterMap,
            Token::Keyword(Keyword::Filter) => FilterType::Filter,
            _ => {
                return Err(ParseError::expected(
                    "`filtermap` or `filter`",
                    token,
                    span,
                )
                .into());
            }
        };

        let ident = self.identifier()?;
        let params = self.params()?;
        let body = self.block()?;

        Ok(FilterMap {
            filter_type,
            ident,
            params,
            body,
        })
    }

    /// Parse an optional with clause for filtermap, define and apply
    ///
    /// ```ebnf
    /// Params ::= '(' TypeIdentField (',' TypeIdentField)* ')'
    /// ```
    pub fn params(&mut self) -> ParseResult<Meta<Params>> {
        let m = self.separated(
            Token::RoundLeft,
            Token::RoundRight,
            Token::Comma,
            Self::type_ident_field,
        )?;
        let id = m.id;
        Ok(Meta {
            id,
            node: Params(m.node),
        })
    }

    /// Parse an identifier and a type identifier separated by a colon
    ///
    /// ```ebnf
    /// TypeIdentField ::= Identifier ':' TypeExpr
    /// ```
    fn type_ident_field(&mut self) -> ParseResult<Meta<Param>> {
        let name = self.identifier()?;
        self.take(Token::Colon)?;
        let ty = self.type_expr()?;

        let span = self.merge_spans(&name, &ty);
        Ok(self.spans.add(span, Param { name, ty }))
    }

    pub fn type_parameters(
        &mut self,
    ) -> ParseResult<Option<Meta<Vec<Meta<Identifier>>>>> {
        let params = if self.peek_is(Token::SquareLeft) {
            Some(self.separated(
                Token::SquareLeft,
                Token::SquareRight,
                Token::Comma,
                Self::identifier,
            )?)
        } else {
            None
        };
        Ok(params)
    }

    /// Parse a record type declaration
    ///
    /// ```ebnf
    /// Type ::= 'record' Identifier RecordType
    /// ```
    pub(super) fn record_type_assignment(
        &mut self,
    ) -> ParseResult<RecordTypeDeclaration> {
        self.take(Token::Keyword(Keyword::Record))?;
        let ident = self.identifier()?;
        let type_params = self.type_parameters()?;
        let record_type = self.record_type()?;

        Ok(RecordTypeDeclaration {
            ident,
            type_params,
            record_type,
        })
    }

    pub(super) fn enum_declaration(
        &mut self,
    ) -> ParseResult<EnumTypeDeclaration> {
        self.take(Token::Keyword(Keyword::Enum))?;
        let ident = self.identifier()?;

        let type_params = self.type_parameters()?;

        let variants = self.separated(
            Token::CurlyLeft,
            Token::CurlyRight,
            Token::Comma,
            Self::enum_variant,
        )?;

        Ok(EnumTypeDeclaration {
            ident,
            type_params,
            variants,
        })
    }

    fn enum_variant(&mut self) -> ParseResult<Meta<Variant>> {
        let ident = self.identifier()?;

        let mut span = self.spans.get(&ident);
        let fields = if self.peek_is(Token::RoundLeft) {
            let fields = self.separated(
                Token::RoundLeft,
                Token::RoundRight,
                Token::Comma,
                Self::type_expr,
            )?;
            span = span.merge(self.spans.get(&fields));
            fields.node
        } else {
            Vec::new()
        };

        Ok(self.spans.add(span, Variant { ident, fields }))
    }
}
