//! Parser for Roto scripts
//!
//! The parser is a fairly standard recursive descent parser.
//!
//! There is currently no way that the parser can recover from invalid syntax.
//! Therefore, we can only report one parse error.

use crate::{
    ast::{
        Declaration, FunctionDeclaration, Identifier, Path, SyntaxTree, Test,
    },
    parser::error::Hint,
};
use error::ParseErrorKind;
use token::{Keyword, Lexer, Token};

use self::meta::{Meta, Span, Spans};

mod error;
mod expr;
mod filter_map;
pub mod meta;
mod signature;
pub mod token;

pub use error::ParseError;

#[cfg(test)]
mod test_expressions;
#[cfg(test)]
mod test_sections;

type ParseResult<T> = Result<T, Box<ParseError>>;

pub struct Parser<'source, 'spans> {
    file: usize,
    file_length: usize,
    lexer: Lexer<'source>,
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
                hints: Vec::new(),
            }
            .into()),
            Some((Err(()), span)) => Err(ParseError {
                kind: ParseErrorKind::InvalidToken,
                location: Span::new(self.file, span),
                note: None,
                hints: Vec::new(),
            }
            .into()),
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
            Err(ParseError::expected(token, next, span).into())
        }
    }

    /// Parse a separated and delimited list of items
    ///
    /// Assuming that `{`, `}` and `,` are the opening, closing and separating
    /// tokens, respectively. And the given parser passes `FOO`, then this
    /// function corresponds to the following grammar rule:
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
    ) -> ParseResult<SyntaxTree> {
        Self::run_parser(Self::tree, file, spans, input)
    }

    pub fn run_parser<T>(
        mut parser: impl FnMut(&mut Self) -> ParseResult<T>,
        file: usize,
        spans: &'spans mut Spans,
        input: &'source str,
    ) -> ParseResult<T> {
        let mut p = Self {
            file,
            file_length: input.len(),
            lexer: Lexer::new(input),
            spans,
        };
        let out = match parser(&mut p) {
            Ok(out) => out,
            Err(mut err) => {
                if let Some(almost_keyword) = p.lexer.almost_keyword {
                    let ident = almost_keyword.0;
                    let range = almost_keyword.1;

                    let location = Span {
                        file,
                        start: range.start,
                        end: range.end,
                    };

                    let text = match almost_keyword.2 {
                        Some(suggestion) => format!(
                            "`{ident}` is not a valid keyword. You probably meant `{suggestion}`."
                        ),
                        None => format!("`{ident}` is not a valid keyword."),
                    };

                    err.hints.push(Hint { location, text })
                }
                return Err(err);
            }
        };
        if let Some((_, s)) = p.lexer.next() {
            return Err(ParseError {
                kind: ParseErrorKind::FailedToParseEntireInput,
                location: Span::new(file, s),
                note: None,
                hints: Vec::new(),
            }
            .into());
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
            hints: Vec::new(),
        };
        let expr = match self.peek().ok_or(end_of_input)? {
            Token::Keyword(Keyword::FilterMap | Keyword::Filter) => {
                Declaration::FilterMap(Box::new(self.filter_map()?))
            }
            Token::Keyword(Keyword::Record) => {
                Declaration::Record(self.record_type_assignment()?)
            }
            Token::Keyword(Keyword::Variant) => {
                Declaration::Enum(self.variant_declaration()?)
            }
            Token::Keyword(Keyword::Fn) => {
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
                )
                .into());
            }
        };
        Ok(expr)
    }

    /// Parse a term section
    ///
    /// ```ebnf
    /// Function ::= 'fn' Identifier '{' Body '}'
    /// ```
    fn function(&mut self) -> ParseResult<FunctionDeclaration> {
        self.take(Token::Keyword(Keyword::Fn))?;
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

    fn import(&mut self) -> ParseResult<Vec<Meta<Path>>> {
        self.take(Token::Keyword(Keyword::Import))?;
        let path = self.path_expr()?;
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
            Token::Keyword(_) => {
                let note = format!(
                    "`{token}` is a keyword and cannot be used as an identifier."
                );
                let err = ParseError::expected("an identifier", &token, span)
                    .with_note(note);
                return Err(err.into());
            }
            _ => {
                return Err(ParseError::expected(
                    "an identifier",
                    token,
                    span,
                )
                .into());
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
