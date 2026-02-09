use super::{Parser, meta::Spans};
use crate::{
    ast::Signature,
    parser::{
        ParseResult,
        token::{Keyword, Token},
    },
    runtime::ConstantValue,
};

impl<'source, 'spans> Parser<'source, 'spans> {
    pub fn parse_signature(
        spans: &'spans mut Spans,
        literals: &'spans mut Vec<ConstantValue>,
        s: &'source str,
    ) -> ParseResult<Signature> {
        Self::run_parser(Self::signature, 0, spans, literals, s, None)
    }

    fn signature(&mut self) -> ParseResult<Signature> {
        self.take(Token::Keyword(Keyword::Fn))?;

        let type_params = self.type_parameters()?;

        let params = self
            .separated(
                Token::RoundLeft,
                Token::RoundRight,
                Token::Comma,
                Self::type_expr,
            )?
            .node;

        let ret = if self.next_is(Token::Arrow) {
            Some(self.type_expr()?)
        } else {
            None
        };

        Ok(Signature {
            type_params,
            params,
            ret,
        })
    }
}
