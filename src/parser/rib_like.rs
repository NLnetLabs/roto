//! Parsing constructs that have a syntax similar to rib declarations
//!
//! In other words, we parse the constructs that are type declarations.
//! These constructs are `rib`, `table`, `output-stream` and `type`.

use super::{token::Token, ParseResult, Parser};
use crate::ast::{
    ListTypeIdentifier, OutputStream, RecordTypeAssignment,
    RecordTypeIdentifier, Rib, RibBody, RibField, Table, TypeIdentField,
};

/// # Rib-like declarations
impl<'source> Parser<'source> {
    /// Parse a rib declaration
    ///
    /// ```ebnf
    /// Rib ::= 'rib' Identifier
    ///         'contains' TypeIdentifier
    ///         RibBody
    /// ```
    pub(super) fn rib(&mut self) -> ParseResult<Rib> {
        self.take(Token::Rib)?;
        let ident = self.identifier()?.inner;
        self.take(Token::Contains)?;
        let contain_ty = self.type_identifier()?.inner;
        let body = self.rib_body()?;

        Ok(Rib {
            ident,
            contain_ty,
            body,
        })
    }

    /// Parse a table declaration
    ///
    /// ```ebnf
    /// Table ::= 'table' Identifier
    ///           'contains' TypeIdentifier
    ///           RibBody
    /// ```
    pub(super) fn table(&mut self) -> ParseResult<Table> {
        self.take(Token::Table)?;
        let ident = self.identifier()?.inner;
        self.take(Token::Contains)?;
        let contain_ty = self.type_identifier()?.inner;
        let body = self.rib_body()?;

        Ok(Table {
            ident,
            contain_ty,
            body,
        })
    }

    /// Parse an output stream declaration
    ///
    /// ```ebnf
    /// OutputStream ::= 'output-stream' Identifier
    ///                  'contains' TypeIdentifier
    ///                  RibBody
    /// ```
    pub(super) fn output_stream(&mut self) -> ParseResult<OutputStream> {
        self.take(Token::OutputStream)?;
        let ident = self.identifier()?.inner;
        self.take(Token::Contains)?;
        let contain_ty = self.type_identifier()?.inner;
        let body = self.rib_body()?;

        Ok(OutputStream {
            ident,
            contain_ty,
            body,
        })
    }

    /// Parse a record type declaration
    ///
    /// ```ebnf
    /// Type ::= 'type' TypeIdentifier RibBody
    /// ```
    pub(super) fn record_type_assignment(
        &mut self,
    ) -> ParseResult<RecordTypeAssignment> {
        self.take(Token::Type)?;
        let ident = self.type_identifier()?.inner;
        let body = self.rib_body()?;
        let record_type = RecordTypeIdentifier {
            key_values: body.key_values,
        };

        Ok(RecordTypeAssignment { ident, record_type })
    }

    /// Parse a rib body
    ///
    /// A rib body is enclosed in curly braces and the fields are separated
    /// by commas. A trailing comma is allowed.
    ///
    /// ```ebnf
    /// RibBody ::= '{' ( RibField ( ',' RibField )* ','? ) '}'
    /// ```
    fn rib_body(&mut self) -> ParseResult<RibBody> {
        let key_values = self.separated(
            Token::CurlyLeft,
            Token::CurlyRight,
            Token::Comma,
            Self::rib_field,
        )?;

        Ok(RibBody { key_values })
    }

    /// Parse a rib field
    ///
    /// ```ebnf
    /// RibField ::= Identifier ':'
    ///              (RibBody | '[' TypeIdentifier ']' | TypeIdentifier)
    /// ```
    fn rib_field(&mut self) -> ParseResult<RibField> {
        let key = self.identifier()?;
        self.take(Token::Colon)?;

        let field = if self.peek_is(Token::CurlyLeft) {
            // TODO: This recursion seems to be the right thing to do, maybe
            // the syntax tree should reflect that.
            let RibBody { key_values } = self.rib_body()?;
            RibField::RecordField(Box::new((
                key,
                RecordTypeIdentifier { key_values },
            )))
        } else if self.next_is(Token::SquareLeft) {
            let inner_type = self.type_identifier()?.inner;
            self.take(Token::SquareRight)?;
            RibField::ListField(Box::new((
                key,
                ListTypeIdentifier { inner_type },
            )))
        } else {
            RibField::PrimitiveField(TypeIdentField {
                field_name: key,
                ty: self.type_identifier()?,
            })
        };

        Ok(field)
    }
}
