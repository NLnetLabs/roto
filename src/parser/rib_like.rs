//! Parsing constructs that have a syntax similar to rib declarations
//!
//! In other words, we parse the constructs that are type declarations.
//! These constructs are `rib`, `table`, `output-stream` and `type`.

use super::{meta::Meta, token::Token, ParseResult, Parser};
use crate::ast::{
    Identifier, OutputStream, RecordType, RecordTypeDeclaration, Rib,
    RibBody, RibFieldType, Table,
};

/// # Rib-like declarations
impl Parser<'_, '_> {
    /// Parse a rib declaration
    ///
    /// ```ebnf
    /// Rib ::= 'rib' Identifier
    ///         'contains' TypeIdentifier
    ///         RibBody
    /// ```
    pub(super) fn rib(&mut self) -> ParseResult<Rib> {
        self.take(Token::Rib)?;
        let ident = self.identifier()?;
        self.take(Token::Contains)?;
        let contain_ty = self.identifier()?;
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
        let ident = self.identifier()?;
        self.take(Token::Contains)?;
        let contain_ty = self.identifier()?;
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
        let ident = self.identifier()?;
        self.take(Token::Contains)?;
        let contain_ty = self.identifier()?;
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
    ) -> ParseResult<RecordTypeDeclaration> {
        self.take(Token::Type)?;
        let ident = self.identifier()?;
        let body = self.rib_body()?;
        let record_type = RecordType {
            key_values: body.key_values,
        };

        Ok(RecordTypeDeclaration { ident, record_type })
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
    fn rib_field(&mut self) -> ParseResult<(Meta<Identifier>, RibFieldType)> {
        let key = self.identifier()?;
        self.take(Token::Colon)?;

        let field_type = self.rib_field_type()?;

        Ok((key, field_type))
    }

    fn rib_field_type(&mut self) -> ParseResult<RibFieldType> {
        Ok(if self.peek_is(Token::CurlyLeft) {
            // TODO: This recursion seems to be the right thing to do, maybe
            // the syntax tree should reflect that.
            let RibBody { key_values } = self.rib_body()?;
            let span = self.get_span(&key_values);
            RibFieldType::Record(
                self.add_span(span, RecordType { key_values }),
            )
        } else if self.peek_is(Token::SquareLeft) {
            let start = self.take(Token::SquareLeft)?;
            let inner_type = self.rib_field_type()?;
            let end = self.take(Token::SquareRight)?;
            let span = start.merge(end);
            RibFieldType::List(self.add_span(span, Box::new(inner_type)))
        } else {
            RibFieldType::Identifier(self.identifier()?)
        })
    }
}
