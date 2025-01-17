use crate::ast::{FilterMap, FilterType, Identifier, Params};

use super::{meta::Meta, token::Token, ParseError, ParseResult, Parser};

/// # Parsing `filter-map` and `filter` sections
impl Parser<'_, '_> {
    /// Parse a filter-map or filter expression
    ///
    /// ```ebnf
    /// FilterMap ::= ( 'filter-map' | 'filter' ) Identifier
    ///               FilterMapBody
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
        let body = self.block()?;

        Ok(FilterMap {
            filter_type,
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
