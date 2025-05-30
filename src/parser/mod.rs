//! Parser for Roto scripts
//!
//! The parser is a fairly standard recursive descent parser.
//!
//! There is currently no way that the parser can recover from invalid syntax.
//! Therefore, we can only report one parse error.

use crate::ast::{
    Declaration, FunctionDeclaration, Identifier, Path, SyntaxTree, Test,
};
use std::{fmt::Display, iter::Peekable};
use token::{Keyword, Lexer, Token};

use self::meta::{Meta, Span, Spans};

mod expr;
mod filter_map;
pub mod meta;
pub mod token;

#[cfg(test)]
mod test_expressions;
#[cfg(test)]
mod test_sections;

type ParseResult<'a, T> = Result<T, ParseError>;

#[derive(Clone, Debug)]
pub struct ParseError {
    pub location: Span,
    pub kind: ParseErrorKind,
    pub note: Option<String>,
}

impl ParseError {
    fn expected(
        expected: impl Display,
        got: impl Display,
        span: Span,
    ) -> Self {
        Self {
            kind: ParseErrorKind::Expected {
                expected: expected.to_string(),
                got: got.to_string(),
            },
            location: span,
            note: None,
        }
    }

    fn invalid_literal(
        description: impl Display,
        token: impl Display,
        inner: impl Display,
        span: Span,
    ) -> Self {
        Self {
            kind: ParseErrorKind::InvalidLiteral {
                description: description.to_string(),
                token: token.to_string(),
                inner_error: inner.to_string(),
            },
            location: span,
            note: None,
        }
    }

    fn custom(
        description: impl Display,
        label: impl Display,
        span: Span,
    ) -> Self {
        Self {
            kind: ParseErrorKind::Custom {
                description: description.to_string(),
                label: label.to_string(),
            },
            location: span,
            note: None,
        }
    }

    fn with_note(self, note: impl Into<String>) -> Self {
        Self {
            note: Some(note.into()),
            ..self
        }
    }
}

#[derive(Clone, Debug)]
pub enum ParseErrorKind {
    EmptyInput,
    EndOfInput,
    FailedToParseEntireInput,
    InvalidToken,
    Expected {
        expected: String,
        got: String,
    },
    InvalidLiteral {
        description: String,
        token: String,
        inner_error: String,
    },
    Custom {
        description: String,
        label: String,
    },
}

impl ParseErrorKind {
    pub fn label(&self) -> String {
        match self {
            Self::EmptyInput => "input is empty".into(),
            Self::EndOfInput => "reached end of input".into(),
            Self::FailedToParseEntireInput => "parser got stuck here".into(),
            Self::InvalidToken => "invalid token".into(),
            Self::Expected { expected, .. } => {
                format!("expected {expected}")
            }
            Self::InvalidLiteral { description, .. } => {
                format!("invalid {description}")
            }
            Self::Custom { label, .. } => label.clone(),
        }
    }
}

impl std::fmt::Display for ParseErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::EmptyInput => write!(f, "input was empty"),
            Self::EndOfInput => write!(f, "unexpected end of input"),
            Self::FailedToParseEntireInput => {
                write!(f, "failed to parse entire input")
            }
            Self::InvalidToken => write!(f, "invalid token"),
            Self::Expected { expected, got, .. } => {
                write!(f, "expected {expected} but got '{got}'")
            }
            Self::InvalidLiteral {
                description,
                token,
                inner_error,
                ..
            } => {
                write!(f, "found an invalid {description} literal '{token}': {inner_error}")
            }
            Self::Custom { description, .. } => {
                write!(f, "{description}")
            }
        }
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl std::error::Error for ParseError {}

pub struct Parser<'source, 'spans> {
    file: usize,
    file_length: usize,
    lexer: Peekable<Lexer<'source>>,
    pub spans: &'spans mut Spans,
}

/// # Helper methods
impl<'source> Parser<'source, '_> {
    /// Move the lexer forward and return the token
    fn next(&mut self) -> ParseResult<(Token<'source>, Span)> {
        match self.lexer.next() {
            None => Err(ParseError {
                kind: ParseErrorKind::EndOfInput,
                location: Span::new(
                    self.file,
                    self.file_length..self.file_length,
                ),
                note: None,
            }),
            Some((Err(()), span)) => Err(ParseError {
                kind: ParseErrorKind::InvalidToken,
                location: Span::new(self.file, span),
                note: None,
            }),
            Some((Ok(token), span)) => {
                Ok((token, Span::new(self.file, span)))
            }
        }
    }

    /// Move the lexer forward if the next token matches the given token
    fn next_is(&mut self, token: Token) -> bool {
        if self.peek_is(token) {
            self.next().unwrap();
            true
        } else {
            false
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

    /// Move the lexer forward and assert that it matches the token
    fn take(&mut self, token: Token) -> ParseResult<Span> {
        let (next, span) = self.next()?;
        if next == token {
            Ok(span)
        } else {
            Err(ParseError::expected(token, next, span))
        }
    }

    /// Parse a separated and delimited list of items
    ///
    /// Assuming that `{`, `}` and `,` are the opening, closing and separating
    /// tokens, respectively. And the given parser passes `FOO`, then this
    /// function corrsponds to the following grammar rule:
    ///
    /// ```ebnf
    /// '{' (FOO (',' FOO)* ',')? '}'
    /// ```
    ///
    /// So, the list is allowed to be empty and a trailing separator is allowed.
    fn separated<T>(
        &mut self,
        open: Token,
        close: Token,
        sep: Token,
        mut parser: impl FnMut(&mut Self) -> ParseResult<T>,
    ) -> ParseResult<Meta<Vec<T>>> {
        let start_span = self.take(open)?;

        let mut items = Vec::new();

        // If there are no fields, return the empty vec.
        if self.peek_is(close.clone()) {
            let end_span = self.take(close)?;
            let span = start_span.merge(end_span);
            return Ok(self.add_span(span, items));
        }

        // Parse the first field
        items.push(parser(self)?);

        // Now each field must be separated by a comma
        while self.next_is(sep.clone()) {
            // If we have found the curly right, we have just
            // parsed the trailing comma.
            if self.peek_is(close.clone()) {
                break;
            }

            items.push(parser(self)?);
        }

        let end_span = self.take(close)?;
        let span = start_span.merge(end_span);
        Ok(self.add_span(span, items))
    }
}

/// # Parsing the syntax tree
impl<'source, 'spans> Parser<'source, 'spans> {
    pub fn parse(
        file: usize,
        spans: &'spans mut Spans,
        input: &'source str,
    ) -> ParseResult<'source, SyntaxTree> {
        Self::run_parser(Self::tree, file, spans, input)
    }

    fn run_parser<T>(
        mut parser: impl FnMut(&mut Self) -> ParseResult<T>,
        file: usize,
        spans: &'spans mut Spans,
        input: &'source str,
    ) -> ParseResult<'source, T> {
        let mut p = Self {
            file,
            file_length: input.len(),
            lexer: Lexer::new(input).peekable(),
            spans,
        };
        let out = parser(&mut p)?;
        if let Some((_, s)) = p.lexer.next() {
            return Err(ParseError {
                kind: ParseErrorKind::FailedToParseEntireInput,
                location: Span::new(file, s),
                note: None,
            });
        }
        Ok(out)
    }

    fn tree(&mut self) -> ParseResult<SyntaxTree> {
        let mut declarations = Vec::new();

        while self.peek().is_some() {
            declarations.push(self.root()?);
        }

        Ok(SyntaxTree { declarations })
    }

    /// Parse a root expression
    ///
    /// ```ebnf
    /// Root ::= FilterMap | Function | Type
    /// ```
    fn root(&mut self) -> ParseResult<Declaration> {
        let end_of_input = ParseError {
            kind: ParseErrorKind::EndOfInput,
            location: Span::new(
                self.file,
                self.file_length..self.file_length,
            ),
            note: None,
        };
        let expr = match self.peek().ok_or(end_of_input)? {
            Token::Keyword(Keyword::FilterMap | Keyword::Filter) => {
                Declaration::FilterMap(Box::new(self.filter_map()?))
            }
            Token::Keyword(Keyword::Type) => {
                Declaration::Record(self.record_type_assignment()?)
            }
            Token::Keyword(Keyword::Function) => {
                Declaration::Function(self.function()?)
            }
            Token::Keyword(Keyword::Test) => Declaration::Test(self.test()?),
            Token::Keyword(Keyword::Import) => {
                Declaration::Import(self.import()?)
            }
            _ => {
                let (token, span) = self.next()?;
                return Err(ParseError::expected(
                    "a function, filter, filtermap or import",
                    token,
                    span,
                ));
            }
        };
        Ok(expr)
    }

    /// Parse a term section
    ///
    /// ```ebnf
    /// Function ::= 'function' Identifier '{' Body '}'
    /// ```
    fn function(&mut self) -> ParseResult<FunctionDeclaration> {
        self.take(Token::Keyword(Keyword::Function))?;
        let ident = self.identifier()?;
        let params = self.params()?;

        let ret = if self.next_is(Token::Arrow) {
            Some(self.type_expr()?)
        } else {
            None
        };
        let body = self.block()?;

        Ok(FunctionDeclaration {
            ident,
            params,
            body,
            ret,
        })
    }

    fn test(&mut self) -> ParseResult<Test> {
        self.take(Token::Keyword(Keyword::Test))?;
        let ident = self.identifier()?;
        let body = self.block()?;
        Ok(Test { ident, body })
    }

    fn import(&mut self) -> ParseResult<Meta<Path>> {
        self.take(Token::Keyword(Keyword::Import))?;
        let path = self.path()?;
        self.take(Token::SemiColon)?;
        Ok(path)
    }
}

/// # Parsing identifiers
impl Parser<'_, '_> {
    /// Parse an identifier
    ///
    /// The `contains` and `type` keywords are treated as identifiers,
    /// because we already have tests that use these as names for methods.
    fn identifier(&mut self) -> ParseResult<Meta<Identifier>> {
        let (token, span) = self.next()?;
        let ident = match token {
            Token::Ident(s) => s,
            // 'contains' and `type` is already used as both a keyword and an identifier
            Token::Keyword(Keyword::Type) => "type",
            Token::Keyword(_) => {
                return Err(ParseError::expected(
                    "an identifier",
                    &token,
                    span,
                )
                .with_note(format!(
                    "`{}` is a keyword and cannot be used as an identifier.",
                    &token
                )))
            }
            _ => {
                return Err(ParseError::expected(
                    "an identifier",
                    token,
                    span,
                ))
            }
        };
        let ident = Identifier::from(ident);
        Ok(self.add_span(span, ident))
    }
}

impl Parser<'_, '_> {
    fn add_span<T>(&mut self, span: Span, x: T) -> Meta<T> {
        self.spans.add(span, x)
    }

    fn get_span<T>(&mut self, x: &Meta<T>) -> Span {
        self.spans.get(x)
    }

    fn merge_spans<T, U>(&mut self, x: &Meta<T>, y: &Meta<U>) -> Span {
        self.spans.merge(x, y)
    }
}
