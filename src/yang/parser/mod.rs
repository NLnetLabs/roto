//! Parser for Yang schema files
//!
//! The parser is a fairly standard recursive descent parser.
//!
//! There is currently no way that the parser can recover from invalid syntax.
//! Therefore, we can only report one parse error.

use crate::ast::{
    Identifier, Literal, Stmt, SyntaxTree, YangModuleDeclaration,
};
use crate::parser::Declaration;
use crate::parser::{ParseError, ParseErrorKind};
use crate::yang::parser::expr::Cardinality;
pub use crate::yang::parser::token::Keyword;
use crate::yang::parser::token::{Lexer, Token};
use crate::yang::types::{YangArgType, YangNodeType};
use crate::{
    parser::ParseResult,
    yang::parser::ast::{Argument, Test, YangStmtSeq},
};
use std::iter::Peekable;

use crate::parser::meta::{Meta, MetaId, Span, Spans};

pub mod ast;
mod expr;
mod filter_map;
pub mod meta;
pub mod token;

// #[cfg(test)]
// mod test_expressions;
// #[cfg(test)]
// mod test_sections;

// type ParseResult<'a, T> = Result<T, YangParseError>;

// #[derive(Clone, Debug)]
// pub struct YangParseError {
//     pub location: Span,
//     pub kind: ParseErrorKind,
// }

// impl YangParseError {
//     fn expected(
//         expected: impl Display,
//         got: impl Display,
//         span: Span,
//     ) -> Self {
//         Self {
//             kind: ParseErrorKind::Expected {
//                 expected: expected.to_string(),
//                 got: got.to_string(),
//             },
//             location: span,
//         }
//     }

//     fn invalid_location(
//         got: impl Display,
//         parent: impl Display,
//         span: Span,
//     ) -> Self {
//         Self {
//             kind: ParseErrorKind::InvalidLocation {
//                 got: got.to_string(),
//                 parent: parent.to_string(),
//             },
//             location: span,
//         }
//     }

//     fn invalid_literal(
//         description: impl Display,
//         token: impl Display,
//         inner: impl Display,
//         span: Span,
//     ) -> Self {
//         Self {
//             kind: ParseErrorKind::InvalidLiteral {
//                 description: description.to_string(),
//                 token: token.to_string(),
//                 inner_error: inner.to_string(),
//             },
//             location: span,
//         }
//     }

//     fn custom(
//         description: impl Display,
//         label: impl Display,
//         span: Span,
//     ) -> Self {
//         Self {
//             kind: ParseErrorKind::Custom {
//                 description: description.to_string(),
//                 label: label.to_string(),
//             },
//             location: span,
//         }
//     }
// }

// #[derive(Clone, Debug)]
// pub enum ParseErrorKind {
//     EmptyInput,
//     EndOfInput,
//     FailedToParseEntireInput,
//     InvalidToken,
//     Expected {
//         expected: String,
//         got: String,
//     },
//     InvalidLiteral {
//         description: String,
//         token: String,
//         inner_error: String,
//     },
//     InvalidLocation {
//         got: String,
//         parent: String,
//     },
//     Custom {
//         description: String,
//         label: String,
//     },
// }

// impl ParseErrorKind {
//     pub fn label(&self) -> String {
//         match self {
//             Self::EmptyInput => "input is empty".into(),
//             Self::EndOfInput => "reached end of input".into(),
//             Self::FailedToParseEntireInput => "parser got stuck here".into(),
//             Self::InvalidToken => "invalid token".into(),
//             Self::Expected { expected, .. } => {
//                 format!("expected `{expected}`")
//             }
//             Self::InvalidLiteral { description, .. } => {
//                 format!("invalid {description}")
//             }
//             Self::InvalidLocation { got, .. } => {
//                 format!("the statement {got} cannot be placed here")
//             }
//             Self::Custom { label, .. } => label.clone(),
//         }
//     }
// }

// impl std::fmt::Display for ParseErrorKind {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             Self::EmptyInput => write!(f, "input was empty"),
//             Self::EndOfInput => write!(f, "unexpected end of input"),
//             Self::FailedToParseEntireInput => {
//                 write!(f, "failed to parse entire input")
//             }
//             Self::InvalidToken => write!(f, "invalid token"),
//             Self::Expected { expected, got, .. } => {
//                 write!(f, "expected {expected} but got '{got}'")
//             }
//             Self::InvalidLiteral {
//                 description,
//                 token,
//                 inner_error,
//                 ..
//             } => {
//                 write!(f, "found an invalid {description} literal '{token}': {inner_error}")
//             }
//             Self::InvalidLocation { got, parent } => {
//                 write!(
//                     f,
//                     "the statement '{got}' cannot be a sub-statement of \
//                      '{parent}'"
//                 )
//             }
//             Self::Custom { description, .. } => {
//                 write!(f, "{description}")
//             }
//         }
//     }
// }

// impl std::fmt::Display for YangParseError {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "{}", self.kind)
//     }
// }

// impl std::error::Error for YangParseError {}

/// A custom parser for the yang modeling language
pub struct YangParser<'source, 'spans> {
    file: usize,
    file_length: usize,
    lexer: Peekable<Lexer<'source>>,
    spans: &'spans mut Spans,
}

type Type = ParseResult<(YangStmt, Span)>;

/// # Helper methods
impl<'source> YangParser<'source, '_> {
    /// Move the lexer forward and return the token
    fn next(&mut self) -> ParseResult<(Token<'source>, Span)> {
        match self.lexer.next() {
            None => Err(Box::new(ParseError {
                kind: ParseErrorKind::EndOfInput,
                location: Span::new(
                    self.file,
                    self.file_length..self.file_length,
                ),
                note: None,
                hints: Vec::new(),
            })),
            Some((Err(()), span)) => Err(Box::new(ParseError {
                kind: ParseErrorKind::InvalidToken,
                location: Span::new(self.file, span),
                note: None,
                hints: Vec::new(),
            })),
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
            Err(Box::new(ParseError::expected(token, next, span)))
        }
    }

    /// Move the lexer forward if the token is a keyword.
    /// Keywords are either a builtin string or a string containing a colon,
    /// but not in the first or the last position, i.e. 'namespace:keyword'.
    ///
    /// Furthermore a keyword always represents a yang (sub-)statement. if
    /// it's a sub-statement it may invalid for a specific parent statement.
    /// That is also checked here. If the parent_kw is None, then it will be
    /// allowed if it's a valid statement.
    fn next_is_keyword(&mut self, parent_kw: Option<Meta<Keyword>>) -> Type {
        let (next, span) = self.next()?;

        match next {
            // It's a builtin keyword, but is it in an allowed location?
            // (depends on the parent)
            Token::UnquotedString(s) => {
                let Ok(kw) = Keyword::try_from(s).map_err(|_| {
                    ParseError::expected("a keyword", &next, span)
                }) else {
                    if s.contains(':') {
                        let split = s
                            .split(':')
                            .map(|s| self.add_span(span, Identifier::from(s)))
                            .collect::<Vec<_>>();
                        let stmt = YangStmt::ExtStmt((
                            Some(split[0].clone()),
                            Some(split[1].clone()),
                        ));

                        return Ok((stmt, span));
                    } else {
                        return Err(Box::new(ParseError::expected(
                            "a yang keyword",
                            next,
                            span,
                        )));
                    }
                };

                match parent_kw {
                    // No parent keyword, we're at the root of something: all
                    // keywords allowed.
                    None => {
                        Ok((YangStmt::Stmt(self.add_span(span, kw)), span))
                    }
                    Some(parent_kw) => match parent_kw
                        .allowed_sub_stmts()
                        .into_iter()
                        .find(|c_kw| c_kw.0 == kw)
                    {
                        // Yes, it is completely correct
                        Some((checked_kw, _cardi)) => Ok((
                            YangStmt::Stmt(self.add_span(span, checked_kw)),
                            span,
                        )),
                        // It is a builtin keyword but not allowed for this
                        // parent
                        None => Err(Box::new(ParseError::invalid_location(
                            next,
                            parent_kw.as_str(),
                            span,
                        ))),
                    },
                }
            }
            Token::Keyword(Keyword::Module) => {
                if let Some(parent) = parent_kw {
                    return Err(Box::new(ParseError::invalid_location(
                        "Module can only appear at top level. Maybe you \
                         meant to use 'submodule'?",
                        parent.as_str(),
                        span,
                    )));
                }
                match self.next() {
                    Ok((Token::Ident(ident), span)) => Ok((
                        YangStmt::Module(
                            self.add_span(span, Identifier::from(ident)),
                            false,
                        ),
                        span,
                    )),
                    Ok((t, span)) => Err(Box::new(ParseError::expected(
                        "a module or submodule",
                        t,
                        span,
                    ))),
                    Err(e) => Err(e),
                }
            }
            Token::Keyword(Keyword::SubModule) => {
                if let Some(parent) = parent_kw {
                    return Err(Box::new(ParseError::invalid_location(
                        "Submodule can only appear at top level. It's parent \
                         can be defined with the `belongs-to` statement.",
                        parent.as_str(),
                        span,
                    )));
                }
                let module_name = self.next()?;
                // let (belongs_to_name, btn_span) = self.next()?;
                // let Token::Ident(btn) = belongs_to_name else {
                //     return Err(Box::new(ParseError::expected(
                //         "belongs-to",
                //         belongs_to_name,
                //         span,
                //     )));
                // };
                match module_name {
                    (Token::Ident(ident), span) => {
                        Ok((
                            YangStmt::Module(
                                self.add_span(span, Identifier::from(ident)),
                                true, // Some(self.add_span(
                                      //     btn_span,
                                      //     Identifier::from(btn),
                                      // )),
                            ),
                            span,
                        ))
                    }
                    (t, span) => Err(Box::new(ParseError::expected(
                        "a module or submodule",
                        t,
                        span,
                    ))),
                }
            }
            Token::Keyword(kw) => {
                match parent_kw {
                    // No parent keyword, we're at the root of something: all
                    // keywords allowed.
                    None => {
                        Ok((YangStmt::Stmt(self.add_span(span, kw)), span))
                    }
                    Some(parent_kw) => match parent_kw
                        .allowed_sub_stmts()
                        .into_iter()
                        .find(|c_kw| c_kw.0 == kw)
                    {
                        // Yes, it is completely correct
                        Some((checked_kw, _cardi)) => Ok((
                            YangStmt::Stmt(self.add_span(span, checked_kw)),
                            span,
                        )),
                        // It is a builtin keyword but not allowed for this
                        // parent
                        None => Err(Box::new(ParseError::invalid_location(
                            next,
                            parent_kw.as_str(),
                            span,
                        ))),
                    },
                }
            }
            // not a builtin kewyord, but it can still be a keyword defined
            // in a yang extension, in the from of 'ext-name:keyword'. To us
            // the keyword is just a string. module and type checking later on
            // should validate that string.
            //
            // First, you can't start with a colon
            Token::Ident(s) if s.starts_with(':') => {
                Err(Box::new(ParseError::custom(
                    "empty namespace in keyword is not allowed",
                    "invalid keyword",
                    span,
                )))
            }
            // Second, you should not start with [double|single] quote
            Token::QuotedString(s)
                if s.starts_with("'") || s.starts_with('"') =>
            {
                // if there's no colon in here, this cannot be a keyword from
                // an extension, so return with an error.
                // incompletely quoted, no closing quote.
                Err(Box::new(ParseError::custom(
                    "a keyword should not be wrapped in quotes",
                    "try removing the quote(s)",
                    span,
                )))
            }
            Token::Ident(ident)
                if ident.chars().next().unwrap().is_alphanumeric()
                    && ident.contains(':') =>
            {
                let split = ident
                    .split(':')
                    .map(|s| self.add_span(span, Identifier::from(s)))
                    .collect::<Vec<_>>();
                Ok((
                    YangStmt::ExtStmt((
                        Some(split[0].clone()),
                        Some(split[1].clone()),
                    )),
                    span,
                ))
            }
            _ => Err(Box::new(ParseError::expected(
                "a yang statement",
                next,
                span,
            ))),
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
    /// So, the list is allowed to be empty and a trailing separator is
    /// allowed.
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
impl<'source, 'spans> YangParser<'source, 'spans> {
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
            lexer: Lexer::new(input).peekable(),
            spans,
        };
        let out = parser(&mut p)?;
        if let Some((_, s)) = p.lexer.next() {
            return Err(Box::new(ParseError {
                kind: ParseErrorKind::FailedToParseEntireInput,
                location: Span::new(file, s),
                note: None,
                hints: Vec::new(),
            }));
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

        let (module_tree, span) = self.yang_stmt_seq(None)?;
        let expr = match module_tree.is_module() {
            Some((module, is_sub)) if !is_sub => {
                let Some(prefix) = module.find_attr("prefix") else {
                    return Err(Box::new(ParseError::expected(
                        "a prefix statement",
                        "nothing",
                        span,
                    )));
                };
                let Some(namespace) = module.find_attr("namespace") else {
                    return Err(Box::new(ParseError::expected(
                        "a namespace statement",
                        "nothing",
                        span,
                    )));
                };
                Ok(Declaration::YangModule(YangModuleDeclaration {
                    ident: Meta {
                        id: module_tree.id,
                        node: module.stmt.as_ident(),
                    },
                    prefix: Some(Meta {
                        id: prefix.id,
                        node: prefix.as_ident().unwrap(),
                    }),
                    namespace: Some(Meta {
                        id: namespace.id,
                        node: namespace.as_str(),
                    }),
                    parent: None,
                    body: module.sub_stmts.clone(),
                }))
            }
            Some((module, _)) => {
                let Some(parent) = module.find_attr("belongs-to") else {
                    return Err(Box::new(ParseError::expected(
                        "belongs-to",
                        "nothing",
                        span,
                    )));
                };
                Ok(Declaration::YangModule(YangModuleDeclaration {
                    ident: Meta {
                        id: module_tree.id,
                        node: module.stmt.as_ident(),
                    },
                    parent: Some(Meta {
                        id: parent.id,
                        node: parent.as_ident().unwrap(),
                    }),
                    body: module.sub_stmts.clone(),
                    prefix: None,
                    namespace: None,
                }))
            }
            None => {
                let (token, span) = self.next()?;
                Err(Box::new(ParseError::expected(
                    "a yang (zub)module",
                    token,
                    span,
                )))
            }
        };

        // let expr = match self.peek().ok_or(end_of_input)? {
        //     Token::Keyword(Keyword::Module)
        //     | Token::Keyword(Keyword::SubModule) => {
        //         let (module, span) = self.yang_stmt_seq(None)?;
        //         let module = module.is_module().ok_or_else(|| {
        //             ParseError::expected(
        //                 "a yang (xub)module",
        //                 module.node.stmt.clone(),
        //                 span,
        //             )
        //         })?;

        //         Declaration::YangModule(
        //             // Stmt::YangStmtSeq(self.yang_stmt_seq(None)?.0.node),
        //             YangModuleDeclaration {
        //                 ident: module.0.clone(),
        //                 prefix: module.0
        //                 body: module.1.clone(),
        //             },
        //         )
        //     }
        //     // Token::Test => Declaration::Test(self.test()?),
        //     // Token::Slash | Token::DoubleSlash => {
        //     //     Declaration::XPath(self.xpath()?)
        //     // }
        //     _t => {
        //         let (token, span) = self.next()?;
        //         return Err(Box::new(ParseError::expected(
        //             "a yang (zub)module",
        //             token,
        //             span,
        //         )));
        //     }
        // };
        expr
    }

    fn yang_stmt_seq(
        &mut self,
        parent_kw: Option<Meta<Keyword>>,
    ) -> ParseResult<(Meta<YangStmtSeq>, Span)> {
        // A YANG module contains a sequence of statements. Each statement
        // starts with a keyword, followed by zero or one argument, followed
        // by either a semicolon (";") or a block of substatements enclosed
        // within braces ("{ }"):
        // statement = keyword [argument] (";" / "{" *statement "}")
        //
        // but further on in RFC7950:
        //
        // When an imported extension is used, the extension's keyword MUST
        // be qualified using the prefix with which the extension's module
        // was imported.  If an extension is used in the module where it is
        // defined, the extension's keyword MUST be qualified with the prefix
        // of this module.
        let (stmt, start_span) = self.next_is_keyword(parent_kw)?;

        // next up, the argument, it may not be there, but it cannot be last
        // token, so we still could error out here.
        let arg = self.argument()?;

        if self.peek_is(Token::CurlyLeft) {
            // we have a block
            let block = self.block(stmt.node())?;

            if let Some(st) = stmt.node() {
                // we got an argument, but does our keyword even take one?
                if let Some((arg, span)) = &arg
                    && st.arg_type() == YangArgType::None
                {
                    return Err(Box::new(ParseError::custom(
                        format!(
                            "statement `{stmt}` does not take an argument, \
                             but we got `{}`",
                            arg
                        ),
                        "this statement",
                        *span,
                    )));
                }

                // we got all the statements in the block, but are all the
                // mandatory one's there ('ExactlyOne')
                let mut missing_stmts = vec![];
                st.node
                    .allowed_sub_stmts()
                    .iter()
                    .filter_map(|mand_s| {
                        if mand_s.1 == Cardinality::ExactlyOne {
                            Some(mand_s.0)
                        } else {
                            None
                        }
                    })
                    .for_each(|mand_s| {
                        if !block.stmts.iter().any(|decl_s| {
                            if let Stmt::YangStmtSeq(node) = &decl_s.node {
                                node.stmt
                                    .node()
                                    .map(|s| s.node == mand_s)
                                    .unwrap_or_else(|| {
                                        // this is an extended statement,
                                        // which is never mandatory
                                        true
                                    })
                            } else {
                                false
                            }
                        }) {
                            missing_stmts.push(mand_s.as_str());
                        }
                    });

                if !missing_stmts.is_empty() {
                    return Err(Box::new(ParseError::custom(
                        format!(
                            "missing sub-statement(s): {:?} for statement \
                            `{}`",
                            missing_stmts,
                            st.node.as_str()
                        ),
                        "this argument",
                        start_span,
                    )));
                }
            };

            return Ok((
                self.add_span(
                    start_span,
                    YangStmtSeq {
                        stmt,
                        arg: arg.map(|a| a.0),
                        sub_stmts: Meta {
                            id: block.id,
                            node: crate::ast::Expr::Block(block),
                        },
                    },
                ),
                start_span,
            ));
        }

        // no block in this sequence, there must be either an argument,
        // or it is an attribute that is implicitly a bool set to true
        // (this comes from Cisco-style router configurations, e.g.
        // 'nacm:default-deny-all').
        let arg = if let Some((arg, _span)) = arg {
            // Meta {
            //     id: arg.id,
            //     node: Literal::String(arg.node.to_string()),
            // }
            arg
        } else {
            Meta {
                id: MetaId(start_span.start),
                node: Argument::Ident(Identifier::from("true")),
            }
        };

        let (next, span) = self.next()?;
        if let Token::SemiColon = next {
        } else {
            return Err(Box::new(ParseError::expected(
                "semicolon or block",
                next,
                span,
            )));
        };

        Ok((
            self.add_span(
                span,
                YangStmtSeq {
                    stmt,
                    arg: None,
                    sub_stmts: Meta {
                        id: arg.id,
                        node: crate::ast::Expr::Argument(arg),
                    },
                },
            ),
            span,
        ))
    }

    fn test(&mut self) -> ParseResult<Test> {
        self.take(Token::Test)?;
        let ident = self.yang_stmt_seq(None)?.0;
        let body = self.block(ident.node.stmt.node())?;
        Ok(Test {
            ident: ident.stmt.node().unwrap(),
            body,
        })
    }
}

/// # Parsing identifiers
impl YangParser<'_, '_> {
    /// Parse an identifier
    ///
    /// The `contains` and `type` keywords are treated as identifiers,
    /// because we already have tests that use these as names for methods.
    fn identifier(&mut self) -> ParseResult<Meta<Identifier>> {
        let (token, span) = self.next()?;
        let ident = if let Token::Ident(s) = token {
            s
        } else {
            return Err(Box::new(ParseError::expected(
                "an identifier",
                token,
                span,
            )));
        };
        let ident = Identifier::from(ident);
        Ok(self.add_span(span, ident))
    }

    fn concatenate_plussed_strings(
        &mut self,
        s: &str,
    ) -> ParseResult<Argument> {
        let mut res_string = s.to_string();
        while let Some(Token::UnquotedString("+")) = self.peek() {
            let _ = self.take(Token::UnquotedString("+"));
            let (next_string, span) = self.next()?;
            if let Token::QuotedString(s) = next_string {
                res_string = format!(
                    "{}{}",
                    res_string,
                    special_chars(&s[1..s.len() - 1])
                );
            } else {
                return Err(Box::new(ParseError::expected(
                    "a quoted string",
                    next_string,
                    span,
                )));
            }
            self.add_span(span, s);
        }
        Ok(Argument::QuotedString(Literal::String(res_string)))
    }

    /// The argument is the token that is always preceded by a statement, and
    /// it can be a QuotedString (easy), an unquoted string, or an Identifier.
    /// An identifier is always a valid unquoted string (the reverse is not
    /// true).
    fn argument(&mut self) -> ParseResult<Option<(Meta<Argument>, Span)>> {
        if self.peek_is(Token::CurlyLeft) || self.peek_is(Token::SemiColon) {
            return Ok(None);
        }

        let Ok((token, span)) = self.next() else {
            return Ok(None);
        };

        let arg = match token {
            Token::Ident(s) if s.contains(':') => {
                let split = s.split(':').collect::<Vec<_>>();

                if split.len() != 2 {
                    return Err(Box::new(ParseError::expected(
                        "an identifier with or without a prefix separated \
                        by ':'",
                        s,
                        span,
                    )));
                };
                Argument::PrefixIdent((
                    Identifier::from(split[0]),
                    Identifier::from(split[1]),
                ))
            }
            Token::Ident(s) => Argument::Ident(Identifier::from(s)),
            // double quoted string, this actually matters, since yang only
            // has special characters in double quoted strings.
            Token::QuotedString(s) if s.starts_with('"') => {
                // could still be an identifier
                let noq = s.strip_circumfix('"', '"').unwrap();
                if noq
                    .find(|c: char| {
                        !(c.is_alphanumeric()
                            || c == '_'
                            || c == '-'
                            || c == '.')
                    })
                    .is_none()
                    && noq.starts_with(|c: char| {
                        c.is_ascii_uppercase()
                            || c.is_ascii_lowercase()
                            || c == '_'
                    })
                {
                    Argument::Ident(Identifier::from(noq))
                } else {
                    if s.ends_with('"') {
                        let res_string = special_chars(&s[1..s.len() - 1]);
                        self.concatenate_plussed_strings(res_string.as_str())?
                    } else {
                        let (_, span) = self.next()?;
                        return Err(Box::new(ParseError::expected(
                            "a quoted string as argument",
                            s,
                            span,
                        )));
                    }
                }
            }
            Token::QuotedString(s) if s.starts_with("'") => {
                // could still be an identifier
                let noq = s.strip_circumfix("'", "'").unwrap();
                if noq
                    .find(|c: char| {
                        !(c.is_alphanumeric()
                            || c == '_'
                            || c == '-'
                            || c == '.')
                    })
                    .is_none()
                    && noq.starts_with(|c: char| {
                        c.is_ascii_uppercase()
                            || c.is_ascii_lowercase()
                            || c == '_'
                    })
                {
                    Argument::Ident(Identifier::from(noq))
                } else {
                    if s.ends_with("'") {
                        self.concatenate_plussed_strings(s)?
                    } else {
                        let (_, span) = self.next()?;
                        return Err(Box::new(ParseError::expected(
                            "a quoted string as argument",
                            s,
                            span,
                        )));
                    }
                }
            }
            Token::UnquotedString(s) => {
                let l = Literal::String(s.to_string());
                Argument::UnquotedString(l)
            }
            // It ain't great, but keywords in yang are not actual keywords
            //  apparently: they can appear as identifiers, so here goes.
            s => Argument::Ident(Identifier::from(s.to_string())),
        };

        Ok(Some((self.add_span(span, arg), span)))
    }
}

impl YangParser<'_, '_> {
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum YangStmt {
    // module name, is a submodule
    Module(Meta<Identifier>, bool),
    Stmt(Meta<Keyword>),
    ExtStmt((Option<Meta<Identifier>>, Option<Meta<Identifier>>)),
}

impl YangStmt {
    pub fn node(&self) -> Option<Meta<Keyword>> {
        match self {
            YangStmt::Stmt(meta) => Some(meta.clone()),
            YangStmt::ExtStmt(_) => None,
            YangStmt::Module(meta, belongs_to) => match belongs_to {
                false => Some(Meta {
                    id: meta.id,
                    node: Keyword::Module,
                }),
                true => Some(Meta {
                    id: meta.id,
                    node: Keyword::SubModule,
                }),
            },
        }
    }

    pub fn is_keyword(&self, kw: Keyword) -> bool {
        match self {
            YangStmt::Module(_meta, _) => false,
            YangStmt::Stmt(meta) => meta.node == kw,
            YangStmt::ExtStmt((_p, _meta)) => false,
        }
    }

    pub fn as_ident(&self) -> Identifier {
        match self {
            YangStmt::Module(meta, _) => Identifier::from(meta.node.as_str()),
            YangStmt::Stmt(meta) => Identifier::from(meta.node.as_str()),
            YangStmt::ExtStmt((p, meta)) => Identifier::from(
                format!(
                    "{}:{}",
                    p.clone().map(|p| p.as_str()).unwrap_or(""),
                    meta.clone().map(|m| m.as_str()).unwrap_or("")
                )
                .as_str(),
            ),
        }
    }
}

impl std::fmt::Display for YangStmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            YangStmt::Module(meta, btn) => match btn {
                false => write!(f, "{}", meta.as_str()),
                true => write!(f, "{} (submodule)", meta.as_str()),
            },
            YangStmt::Stmt(meta) => write!(f, "{}", meta.as_str()),
            YangStmt::ExtStmt(meta) => {
                let p =
                    meta.0.clone().map(|p| p.as_str()).unwrap_or("<unknown>");
                let i =
                    meta.1.clone().map(|i| i.as_str()).unwrap_or("<unknown>");
                write!(f, "{}:{}", p, i)
            }
        }
    }
}

// Helper functions

// RFC7950:
// """
// Within a double-quoted string (enclosed within " "), a backslash
// character introduces a representation of a special character, which
// depends on the character that immediately follows the backslash:
//
//    \n      newline
//    \t      a tab character
//    \"      a double quote
//    \\      a single backslash
//
// The backslash MUST NOT be followed by any other character.
// """
fn special_chars(s: &str) -> String {
    let mut s = s.to_string();
    let ub = &mut [0; 2];

    s = s.replace("\\t", '\t'.encode_utf8(ub));
    s = s.replace("\\\"", '\"'.encode_utf8(ub));
    s = s.replace("\\\\", '\\'.encode_utf8(ub));

    trailing_ws_before_newline(s)
}

// RFC7950 6.1.3:
// """
// If a double-quoted string contains a line break
// followed by space or tab characters that are used to
// indent the text according to the layout in the YANG
// file, this leading whitespace is stripped from the
// string, up to and including the column of the starting
// double quote character, or to the first non-whitespace
// character, whichever occurs first.  Any tab character
// in a succeeding line that must be examined for
// stripping is first converted into 8 space characters
// """
//
// We are violating this, if a string contains a line
// break, we will just strip all consecutive whitespace
// after it.
fn trailing_ws_before_newline(s: String) -> String {
    s.split('\n')
        .enumerate()
        .map(|(i, s)| if i > 0 { s.trim_ascii_start() } else { s })
        .collect::<String>()
}
