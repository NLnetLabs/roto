use crate::ast::{
    FilterMap, FilterType, Identifier, Params, RecordTypeDeclaration,
    TypeExpr, Variant, VariantTypeDeclaration,
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
                ));
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
    fn type_ident_field(
        &mut self,
    ) -> ParseResult<(Meta<Identifier>, Meta<TypeExpr>)> {
        let field_name = self.identifier()?;
        self.take(Token::Colon)?;
        let ty = self.type_expr()?;
        Ok((field_name, ty))
    }

    /// Parse a record type declaration
    ///
    /// ```ebnf
    /// Type ::= 'type' Identifier RecordType
    /// ```
    pub(super) fn record_type_assignment(
        &mut self,
    ) -> ParseResult<RecordTypeDeclaration> {
        self.take(Token::Keyword(Keyword::Record))?;
        let ident = self.identifier()?;
        let record_type = self.record_type()?;

        Ok(RecordTypeDeclaration { ident, record_type })
    }

    pub(super) fn variant_declaration(
        &mut self,
    ) -> ParseResult<VariantTypeDeclaration> {
        self.take(Token::Keyword(Keyword::Variant))?;
        let ident = self.identifier()?;

        let type_params = if self.peek_is(Token::SquareLeft) {
            self.separated(
                Token::SquareLeft,
                Token::SquareRight,
                Token::Comma,
                Self::identifier,
            )?
            .node
        } else {
            Vec::new()
        };

        let variants = self.separated(
            Token::CurlyLeft,
            Token::CurlyRight,
            Token::Comma,
            Self::enum_variant,
        )?;

        Ok(VariantTypeDeclaration {
            ident,
            type_params,
            variants,
        })
    }

    fn enum_variant(&mut self) -> ParseResult<Variant> {
        let ident = self.identifier()?;

        let fields = if self.peek_is(Token::RoundLeft) {
            self.separated(
                Token::RoundLeft,
                Token::RoundRight,
                Token::Comma,
                Self::type_expr,
            )?
            .node
        } else {
            Vec::new()
        };

        Ok(Variant { ident, fields })
    }
}
