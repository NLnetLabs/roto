use crate::ast::{
    Expr, FilterMap, FilterMapBody, FilterType, Identifier, Params,
};

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
        let body = self.filter_map_body()?;

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
    fn filter_map_body(&mut self) -> ParseResult<FilterMapBody> {
        self.take(Token::CurlyLeft)?;

        let define = if self.next_is(Token::Define) {
            self.define_body()?
        } else {
            Vec::new()
        };

        self.take(Token::Apply)?;
        let apply = self.block()?;
        self.take(Token::CurlyRight)?;

        Ok(FilterMapBody { define, apply })
    }

    /// Parse the body of a define section
    ///
    /// ```ebnf
    /// DefineBody ::= '{' Assignment* '}'
    ///
    /// Assignment ::= Identifier '=' ValueExpr ';'
    /// ```
    fn define_body(
        &mut self,
    ) -> ParseResult<Vec<(Meta<Identifier>, Meta<Expr>)>> {
        self.take(Token::CurlyLeft)?;

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

        Ok(assignments)
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
