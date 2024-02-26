use super::{Parser, ParseResult};
use crate::{token::Token, ast::{Rib, Table, OutputStream, RibField, RecordTypeIdentifier, RibBody, ListTypeIdentifier, TypeIdentField}};

impl<'source> Parser<'source> {
    /// Parse a rib expression
    ///
    /// ```ebnf
    /// Rib ::= 'rib' Identifier
    ///         'contains' TypeIdentifier
    ///         RibBody
    /// ```
    pub(super) fn rib(&mut self) -> ParseResult<Rib> {
        self.accept_required(Token::Rib)?;
        let ident = self.identifier()?;
        self.accept_required(Token::Contains)?;
        let contain_ty = self.type_identifier()?;
        let body = self.rib_body()?;

        Ok(Rib {
            ident,
            contain_ty,
            body,
        })
    }

    /// Parse a table expression
    ///
    /// ```ebnf
    /// Table ::= 'table' Identifier
    ///           'contains' TypeIdentifier
    ///           RibBody
    /// ```
    pub(super) fn table(&mut self) -> ParseResult<Table> {
        self.accept_required(Token::Table)?;
        let ident = self.identifier()?;
        self.accept_required(Token::Contains)?;
        let contain_ty = self.type_identifier()?;
        let body = self.rib_body()?;

        Ok(Table {
            ident,
            contain_ty,
            body,
        })
    }

    /// Parse an output stream expression
    ///
    /// ```ebnf
    /// OutputStream ::= 'output-stream' Identifier
    ///                  'contains' TypeIdentifier
    ///                  RibBody
    /// ```
    pub(super) fn output_stream(&mut self) -> ParseResult<OutputStream> {
        self.accept_required(Token::OutputStream)?;
        let ident = self.identifier()?;
        self.accept_required(Token::Contains)?;
        let contain_ty = self.type_identifier()?;
        let body = self.rib_body()?;

        Ok(OutputStream {
            ident,
            contain_ty,
            body,
        })
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
        self.accept_required(Token::Colon)?;

        let field = if self.peek_is(Token::CurlyLeft) {
            // TODO: This recursion seems to be the right thing to do, maybe
            // the syntax tree should reflect that.
            let RibBody { key_values } = self.rib_body()?;
            RibField::RecordField(Box::new((
                key,
                RecordTypeIdentifier { key_values },
            )))
        } else if self.accept_optional(Token::SquareLeft)?.is_some() {
            let inner_type = self.type_identifier()?;
            self.accept_required(Token::SquareRight)?;
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
