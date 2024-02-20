use crate::ast::{
    Identifier, RecordTypeIdentifier, Rib, RibBody, RibField, RootExpr,
    SyntaxTree, TypeIdentField, TypeIdentifier, ListTypeIdentifier
};
use crate::token::Token;
use logos::{Lexer, Span, SpannedIter};
use std::iter::Peekable;

type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug)]
pub enum ParseError {
    EndOfInput,
    InvalidToken(Span),
    /// Dummy variant where more precise messages should be made
    Todo,
    Expected(Span),
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::EndOfInput => write!(f, "reached end of input"),
            Self::InvalidToken(_) => write!(f, "invalid token"),
            Self::Todo => write!(f, "add a nice message here"),
            Self::Expected(s) => write!(f, "expected at {s:?}"),
        }
    }
}

pub struct Parser<'source> {
    lexer: Peekable<SpannedIter<'source, Token<'source>>>,
}

/// # Helper methods
impl<'source> Parser<'source> {
    /// Move the lexer forward and return the token
    fn next(&mut self) -> ParseResult<(Token<'source>, Span)> {
        match self.lexer.next() {
            None => Err(ParseError::EndOfInput),
            Some((Err(()), span)) => Err(ParseError::InvalidToken(span)),
            Some((Ok(token), span)) => Ok((token, span)),
        }
    }

    /// Peek the next token
    fn peek(&mut self) -> Option<&Token<'source>> {
        match self.lexer.peek() {
            Some((Ok(token), _span)) => Some(token),
            _ => None,
        }
    }

    /// Peek the next token and return whether it matches the given token
    fn peek_is(&mut self, token: Token) -> bool {
        let Some(lexed_token) = self.peek() else {
            return false;
        };

        &token == lexed_token
    }

    /// Check for an optional token
    fn accept_optional(&mut self, token: Token) -> ParseResult<Option<Span>> {
        if self.peek_is(token) {
            Ok(Some(self.next()?.1))
        } else {
            Ok(None)
        }
    }

    /// Move the lexer forward and assert that it matches the token
    fn accept_required(&mut self, token: Token) -> ParseResult<()> {
        let (next, span) = self.next()?;
        if next == token {
            Ok(())
        } else {
            Err(ParseError::Expected(span))
        }
    }
}

/// # Parsing items
impl<'source> Parser<'source> {
    pub fn parse(input: &'source str) -> ParseResult<SyntaxTree> {
        Self {
            lexer: Lexer::new(input).spanned().peekable(),
        }
        .tree()
    }

    fn tree(&mut self) -> ParseResult<SyntaxTree> {
        let mut expressions = Vec::new();

        while self.peek().is_some() {
            expressions.push(self.root()?);
        }

        Ok(SyntaxTree { expressions })
    }

    fn root(&mut self) -> ParseResult<RootExpr> {
        let expr = match self.peek().ok_or(ParseError::EndOfInput)? {
            Token::Rib => RootExpr::Rib(self.rib()?),
            // Token::Table => RootExpr::Table(self.table()),
            // Token::OutputStream => RootExpr::OutputStream(self.output_stream()),
            // Token::FilterMap => RootExpr::FilterMap(Box::new(self.filter_map()?)),
            // Token::Type => RootExpr::Ty(self.record_type_assignment()),
            t => panic!("{:?}", t),
        };
        Ok(expr)
    }

    fn rib(&mut self) -> ParseResult<Rib> {
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

    fn rib_body(&mut self) -> ParseResult<RibBody> {
        self.accept_required(Token::CurlyLeft)?;

        let mut key_values = Vec::new();

        let mut i = 0;
        while !self.peek_is(Token::CurlyRight) {
            if i > 0 {
                self.accept_required(Token::Colon)?;
            }
            key_values.push(self.rib_field()?);
            i += 1;
        }

        self.accept_required(Token::CurlyRight)?;

        Ok(RibBody { key_values })
    }

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

    // TODO: I'm ignoring the grammar difference between identifiers and type identifiers
    // because it doesn't really make sense, I think. But this comment is here to
    // remind me of that before I put this thing up for review.
    // Otherwise the lexer will get a bit more complicated.
    fn identifier(&mut self) -> ParseResult<Identifier> {
        let (token, span) = self.next()?;
        match token {
            Token::Ident(s) => Ok(Identifier { ident: s.into() }),
            _ => Err(ParseError::Expected(span)),
        }
    }

    fn type_identifier(&mut self) -> ParseResult<TypeIdentifier> {
        let (token, span) = self.next()?;
        match token {
            Token::Ident(s) => Ok(TypeIdentifier { ident: s.into() }),
            _ => Err(ParseError::Expected(span)),
        }
    }
}
