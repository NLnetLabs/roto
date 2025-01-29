//! Parsing constructs that have a syntax similar to rib declarations
//!
//! In other words, we parse the constructs that are type declarations.
//! These constructs are `rib`, `table`, `output-stream` and `type`.

use super::{meta::Meta, token::Token, ParseResult, Parser};
use crate::ast::{
    Identifier, RecordFieldType, RecordType, RecordTypeDeclaration,
};

/// # Rib-like declarations
impl Parser<'_, '_> {
    /// Parse a record type declaration
    ///
    /// ```ebnf
    /// Type ::= 'type' TypeIdentifier RibBody
    /// ```
    pub(super) fn record_type_assignment(
        &mut self,
    ) -> ParseResult<RecordTypeDeclaration> {
        self.take(Token::Type)?;
        let ident = self.identifier()?;
        let key_values = self.record_body()?;
        let record_type = RecordType { key_values };

        Ok(RecordTypeDeclaration { ident, record_type })
    }

    #[allow(clippy::type_complexity)]
    fn record_body(
        &mut self,
    ) -> ParseResult<Meta<Vec<(Meta<Identifier>, RecordFieldType)>>> {
        self.separated(
            Token::CurlyLeft,
            Token::CurlyRight,
            Token::Comma,
            Self::record_field,
        )
    }

    /// Parse a rib field
    ///
    /// ```ebnf
    /// RibField ::= Identifier ':'
    ///              (RibBody | '[' TypeIdentifier ']' | TypeIdentifier)
    /// ```
    fn record_field(
        &mut self,
    ) -> ParseResult<(Meta<Identifier>, RecordFieldType)> {
        let key = self.identifier()?;
        self.take(Token::Colon)?;

        let field_type = self.rib_field_type()?;

        Ok((key, field_type))
    }

    fn rib_field_type(&mut self) -> ParseResult<RecordFieldType> {
        Ok(if self.peek_is(Token::CurlyLeft) {
            // TODO: This recursion seems to be the right thing to do, maybe
            // the syntax tree should reflect that.
            let key_values = self.record_body()?;
            let span = self.get_span(&key_values);
            RecordFieldType::Record(
                self.add_span(span, RecordType { key_values }),
            )
        } else if self.peek_is(Token::SquareLeft) {
            let start = self.take(Token::SquareLeft)?;
            let inner_type = self.rib_field_type()?;
            let end = self.take(Token::SquareRight)?;
            let span = start.merge(end);
            RecordFieldType::List(self.add_span(span, Box::new(inner_type)))
        } else {
            RecordFieldType::Identifier(self.identifier()?)
        })
    }
}
