//! Parser for Roto scripts
//!
//! The parser is a fairly standard recursive descent parser.
//!
//! There is currently no way that the parser can recover from invalid syntax.
//! Therefore, we can only report one parse error.

use std::collections::VecDeque;

use crate::{
    ast::{
        ConstantDeclaration, Declaration, FunctionDeclaration, Identifier,
        ImportPath, SyntaxTree, Test,
    },
    parser::error::Hint,
};

#[cfg(feature = "repl")]
use crate::ast::Stmt;

use lexer::Lexer;
use token::{Keyword, Token};

use self::meta::{Meta, Span, Spans};

mod error;
mod expr;
mod filter_map;
pub mod lexer;
pub mod meta;
mod precedence;
mod signature;
pub mod token;

pub use error::{ParseError, ParseErrorKind};

#[cfg(all(test, not(miri)))]
mod test_expressions;
#[cfg(all(test, not(miri)))]
mod test_sections;

pub type ParseResult<T> = Result<T, Box<ParseError>>;

#[cfg(feature = "repl")]
pub enum ReplStmt {
    Declaration(Declaration),
    Let(Meta<Stmt>),
    Expr,
}

pub struct Parser<'source, 'spans> {
    file: usize,
    file_length: usize,
    lexer: Lexer<'source>,
    prev_end: usize,
    pub spans: &'spans mut Spans,
}

#[derive(Clone, Default)]
pub struct Extras {
    pub comments: VecDeque<Span>,
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
                self.prev_end = span.end;
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

    fn peek_many<const N: usize>(&mut self) -> Option<[&Token<'source>; N]> {
        self.lexer.peek_many()
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
    ) -> ParseResult<(SyntaxTree, Extras)> {
        Self::run_parser(Self::tree, file, spans, input)
    }

    /// Parse a single statement (used in REPL)
    ///
    /// Unlike [`Parser::parse`], this accepts one item rather than a whole
    /// file, and that item may be an expression or a `let`, neither of which
    /// can appear at the top level of a script.
    ///
    /// The span of the statement leaves out the optional semicolon after it.
    #[cfg(feature = "repl")]
    pub fn parse_repl_stmt(
        file: usize,
        spans: &'spans mut Spans,
        input: &'source str,
    ) -> ParseResult<Meta<ReplStmt>> {
        let (stmt, _) = Self::run_parser(Self::stmt, file, spans, input)?;
        Ok(stmt)
    }

    pub fn run_parser<T>(
        mut parser: impl FnMut(&mut Self) -> ParseResult<T>,
        file: usize,
        spans: &'spans mut Spans,
        input: &'source str,
    ) -> ParseResult<(T, Extras)> {
        let mut p = Self {
            file,
            file_length: input.len(),
            lexer: Lexer::new(file, input),
            prev_end: 0,
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
        Ok((out, p.lexer.extras))
    }

    fn tree(&mut self) -> ParseResult<SyntaxTree> {
        let mut declarations = Vec::new();

        let shebang = self.lexer.get_shebang();

        while self.peek().is_some() {
            declarations.push(self.root()?);
        }

        Ok(SyntaxTree {
            shebang,
            declarations,
        })
    }

    /// Parse a single statement
    ///
    /// ```ebnf
    /// Stmt ::= (Declaration | LetStmt | Expr) ';'?
    /// ```
    #[cfg(feature = "repl")]
    fn stmt(&mut self) -> ParseResult<Meta<ReplStmt>> {
        let stmt = self.spanned(|p| {
            Ok(if let Some(declaration) = p.try_root()? {
                ReplStmt::Declaration(declaration)
            } else if matches!(p.peek(), Some(Token::Keyword(Keyword::Let))) {
                ReplStmt::Let(p.let_stmt()?)
            } else {
                p.expr()?;
                ReplStmt::Expr
            })
        })?;

        self.next_is(Token::SemiColon);

        Ok(stmt)
    }

    /// Parse a root expression
    ///
    /// ```ebnf
    /// Root ::= FilterMap | Function | Type | Enum | Test
    ///        | Const ';'
    ///        | Import ';'
    /// ```
    fn root(&mut self) -> ParseResult<Declaration> {
        if let Some(declaration) = self.try_root()? {
            if matches!(
                declaration,
                Declaration::Const(_) | Declaration::Import(_)
            ) {
                self.take(Token::SemiColon)?;
            }
            return Ok(declaration);
        }

        let (token, span) = self.next()?;
        Err(ParseError::expected(
            "a function, filter, filtermap or import",
            token,
            span,
        )
        .into())
    }

    /// Try to parse a root expression, return `None` if it can't
    ///
    /// ```ebnf
    /// Declaration ::= FilterMap | Function | Type | Enum | Test
    ///               | Const | Import
    /// ```
    fn try_root(&mut self) -> ParseResult<Option<Declaration>> {
        let declaration = match self.peek() {
            Some(Token::Keyword(Keyword::FilterMap | Keyword::Filter)) => {
                Declaration::FilterMap(Box::new(self.filter_map()?))
            }
            Some(Token::Keyword(Keyword::Const)) => {
                Declaration::Const(self.constant()?)
            }
            Some(Token::Keyword(Keyword::Record)) => {
                Declaration::Record(self.record_type_assignment()?)
            }
            Some(Token::Keyword(Keyword::Enum)) => {
                Declaration::Enum(self.enum_declaration()?)
            }
            Some(Token::Keyword(Keyword::Fn)) => {
                Declaration::Function(self.function()?)
            }
            Some(Token::Keyword(Keyword::Test)) => {
                Declaration::Test(self.test()?)
            }
            Some(Token::Keyword(Keyword::Import)) => {
                Declaration::Import(self.import()?)
            }
            _ => return Ok(None),
        };
        Ok(Some(declaration))
    }

    /// Parse a constant declaration
    fn constant(&mut self) -> ParseResult<ConstantDeclaration> {
        self.take(Token::Keyword(Keyword::Const))?;
        let ident = self.identifier()?;
        self.take(Token::Colon)?;
        let ty = self.type_expr()?;
        self.take(Token::Eq)?;
        let expr = self.expr()?;

        Ok(ConstantDeclaration { ident, ty, expr })
    }

    /// Parse a term section
    ///
    /// ```ebnf
    /// Function ::= 'fn' Identifier Params ('->' TypeExpr)? Block
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

    fn import(&mut self) -> ParseResult<Meta<ImportPath>> {
        self.take(Token::Keyword(Keyword::Import))?;
        self.path_expr()
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

    /// Run `parser` and register the span of the tokens it took
    #[cfg(feature = "repl")]
    fn spanned<T>(
        &mut self,
        parser: impl FnOnce(&mut Self) -> ParseResult<T>,
    ) -> ParseResult<Meta<T>> {
        let start = match self.lexer.peek() {
            Some((_, span)) => span.start,
            None => self.file_length,
        };
        let node = parser(self)?;
        // A parser that took no tokens gets an empty span
        let end = self.prev_end.max(start);
        Ok(self.add_span(Span::new(self.file, start..end), node))
    }
}
