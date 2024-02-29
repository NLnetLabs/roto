use crate::ast::{
    Identifier,
    RootExpr, SyntaxTree, TypeIdentifier,
};
use crate::token::Token;
use logos::{Lexer, Span, SpannedIter};
use miette::Diagnostic;
use std::iter::Peekable;

mod filter_map;
mod rib_like;
mod value;

#[cfg(test)]
mod test_expressions;
#[cfg(test)]
mod test_sections;

type ParseResult<T> = Result<T, ParseError>;

#[derive(Clone, Debug, Diagnostic)]
pub enum ParseError {
    EmptyInput,
    EndOfInput,
    FailedToParseEntireInput,
    InvalidToken(#[label("invalid token")] Span),
    /// Dummy variant where more precise messages should be made
    ///
    /// The argument is just a unique identifier
    Todo(usize),
    Expected {
        expected: String,
        got: String,
        #[label("expected {expected}")]
        span: Span,
    },
    InvalidLiteral {
        description: String,
        token: String,
        #[label("invalid {description} literal")]
        span: Span,
        inner_error: String,
    },
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Parse error: ")?;
        match self {
            Self::EmptyInput => write!(f, "input was empty"),
            Self::EndOfInput => write!(f, "unexpected end of input"),
            Self::FailedToParseEntireInput => {
                write!(f, "failed to parse entire input")
            }
            Self::InvalidToken(_) => write!(f, "invalid token"),
            Self::Todo(n) => write!(f, "add a nice message here {n}"),
            Self::Expected { expected, got, .. } => {
                write!(f, "expected '{expected}' but got '{got}'")
            }
            Self::InvalidLiteral {
                description,
                token,
                inner_error,
                ..
            } => {
                write!(f, "found an invalid {description} literal '{token}': {inner_error}")
            }
        }
    }
}

impl std::error::Error for ParseError {}

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
            // TODO: this should probably be an unwrap?
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
            Err(ParseError::Expected {
                expected: token.to_string(),
                got: next.to_string(),
                span,
            })
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
    ) -> ParseResult<Vec<T>> {
        self.accept_required(open)?;

        let mut items = Vec::new();

        // If there are no fields, return the empty vec.
        if self.accept_optional(close.clone())?.is_some() {
            return Ok(items);
        }

        // Parse the first field
        items.push(parser(self)?);

        // Now each field must be separated by a comma
        while self.accept_optional(sep.clone())?.is_some() {
            // If we have found the curly right, we have just
            // parsed the trailing comma.
            if self.peek_is(close.clone()) {
                break;
            }

            items.push(parser(self)?);
        }

        self.accept_required(close)?;

        Ok(items)
    }
}

/// # Parsing complex expressions
impl<'source> Parser<'source> {
    pub fn parse(input: &'source str) -> ParseResult<SyntaxTree> {
        Self::run_parser(Self::tree, input)
    }

    fn run_parser<T>(
        mut parser: impl FnMut(&mut Self) -> ParseResult<T>,
        input: &'source str,
    ) -> ParseResult<T> {
        let mut p = Self {
            lexer: Lexer::new(input).spanned().peekable(),
        };
        let out = parser(&mut p)?;
        if p.lexer.next().is_some() {
            return Err(ParseError::FailedToParseEntireInput);
        }
        Ok(out)
    }

    fn tree(&mut self) -> ParseResult<SyntaxTree> {
        let mut expressions = Vec::new();

        while self.peek().is_some() {
            expressions.push(self.root()?);
        }

        Ok(SyntaxTree { expressions })
    }

    /// Parse a root expression
    ///
    /// ```ebnf
    /// Root ::= Rib | Table | OutputStream | FilterMap | Type
    /// ```
    fn root(&mut self) -> ParseResult<RootExpr> {
        let expr = match self.peek().ok_or(ParseError::EndOfInput)? {
            Token::Rib => RootExpr::Rib(self.rib()?),
            Token::Table => RootExpr::Table(self.table()?),
            Token::OutputStream => {
                RootExpr::OutputStream(self.output_stream()?)
            }
            Token::FilterMap | Token::Filter => {
                RootExpr::FilterMap(Box::new(self.filter_map()?))
            }
            Token::Type => RootExpr::Ty(self.record_type_assignment()?),
            _ => {
                let (token, span) = self.next()?;
                return Err(ParseError::Expected {
                    expected:
                        "a rib, table, output-stream, filter or filter-map"
                            .into(),
                    got: token.to_string(),
                    span,
                });
            }
        };
        Ok(expr)
    }
}

/// # Parsing identifiers
impl<'source> Parser<'source> {
    fn identifier(&mut self) -> ParseResult<Identifier> {
        let (token, span) = self.next()?;
        match token {
            Token::Ident(s) => Ok(Identifier { ident: s.into() }),
            // 'contains' and `type` is already used as both a keyword and an identifier
            Token::Contains => Ok(Identifier {
                ident: "contains".into(),
            }),
            Token::Type => Ok(Identifier {
                ident: "type".into(),
            }),
            _ => Err(ParseError::Expected {
                expected: "an identifier".into(),
                got: token.to_string(),
                span,
            }),
        }
    }

    fn type_identifier(&mut self) -> ParseResult<TypeIdentifier> {
        let (token, span) = self.next()?;
        match token {
            Token::Ident(s) => Ok(TypeIdentifier { ident: s.into() }),
            // 'contains' and `type` already used as both a keyword and an identifier
            Token::Contains => Ok(TypeIdentifier {
                ident: "contains".into(),
            }),
            Token::Type => Ok(TypeIdentifier {
                ident: "type".into(),
            }),
            _ => Err(ParseError::Expected {
                expected: "an identifier".into(),
                got: token.to_string(),
                span,
            }),
        }
    }
}
