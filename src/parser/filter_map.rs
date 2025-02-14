use crate::ast::{FilterMap, FilterType, Identifier, Params, Path};

use super::{meta::Meta, token::Token, ParseError, ParseResult, Parser};

/// # Parsing `filtermap` and `filter` sections
impl Parser<'_, '_> {
    /// Parse a filtermap or filter expression
    ///
    /// ```ebnf
    /// FilterMap ::= ( 'filtermap' | 'filter' ) Identifier
    ///               FilterMapBody
    /// ```
    pub(super) fn filter_map(&mut self) -> ParseResult<FilterMap> {
        let (token, span) = self.next()?;
        let filter_type = match token {
            Token::FilterMap => FilterType::FilterMap,
            Token::Filter => FilterType::Filter,
            _ => {
                return Err(ParseError::expected(
                    "'filtermap' or 'filter'",
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

    /// Parse an optional with clause for filtermap, define and apply
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
    ) -> ParseResult<(Meta<Identifier>, Meta<Path>)> {
        let field_name = self.identifier()?;
        self.take(Token::Colon)?;
        let ty = self.path()?;
        Ok((field_name, ty))
    }
}
