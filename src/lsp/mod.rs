use tower_lsp::jsonrpc::{self, Result};
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

use crate::parser::token::{Lexer, TokenClassification};

#[derive(Debug)]
struct Backend {
    client: Client,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(
        &self,
        _: InitializeParams,
    ) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
                            work_done_progress_options:
                                WorkDoneProgressOptions {
                                    work_done_progress: None,
                                },
                            legend: SemanticTokensLegend {
                                token_types: vec![
                                    SemanticTokenType::COMMENT,
                                    SemanticTokenType::KEYWORD,
                                    SemanticTokenType::STRING,
                                    SemanticTokenType::NUMBER,
                                    SemanticTokenType::VARIABLE,
                                    SemanticTokenType::OPERATOR,
                                ],
                                token_modifiers: vec![],
                            },
                            range: Some(false),
                            full: Some(SemanticTokensFullOptions::Bool(true)),
                        },
                    ),
                ),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "server initialized!")
            .await;
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let text_document = params.text_document;

        let s = std::fs::read_to_string(text_document.uri.path())
            .map_err(|_| jsonrpc::Error::invalid_request())?;

        let lexer = Lexer::new(&s);

        let mut tokens = vec![];
        let mut last_len = 0;
        let mut last_end = 0;
        let mut delta_line = 0;
        for (token, range) in lexer {
            if range.start > last_end {
                let length = (last_end - range.start) as u32;
                tokens.push(SemanticToken {
                    delta_line,
                    delta_start: last_len,
                    length,
                    token_type: 1,
                    token_modifiers_bitset: 0,
                });
                delta_line = s[last_end..range.start]
                    .chars()
                    .filter(|c| *c == '\n')
                    .count() as u32;
                last_len = length;
            }

            last_end = range.end;

            let token = token.unwrap();
            let token_type = match token.classify() {
                TokenClassification::Keyword => 1,
                TokenClassification::String => 2,
                TokenClassification::Number => 3,
                TokenClassification::Variable => 4,
                TokenClassification::Punctuation => 5,
            };
            let length = (range.end - range.start) as u32;
            tokens.push(SemanticToken {
                delta_line,
                delta_start: last_len,
                length,
                token_type,
                token_modifiers_bitset: 0,
            });
            delta_line = 0;
            last_len = length;
        }

        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data: tokens,
        })))
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
}

pub fn run() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend { client });

    tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()
        .unwrap()
        .block_on(async {
            Server::new(stdin, stdout, socket).serve(service).await;
        })
}
