use log::trace;
use nom::error::convert_error;
use roto::{
    blocks::Scope,
    compiler::{CompileError, Compiler},
};

pub struct TestCompiler<'a> {
    name: &'a str,
    compiler: Compiler,
    source_code: &'a str,
}

impl<'a> TestCompiler<'a> {
    pub fn create(name: &'a str, source_code: &'a str) -> Self {
        Self {
            name,
            compiler: Compiler::new(),
            source_code,
        }
    }

    pub fn test_parse(mut self, expect_success: bool) -> Self {
        trace!("test parse {}", self.name);

        let parse_res = self.compiler.parse_source_code(self.source_code);

        trace!("{} {:#?}", self.name, self.compiler.ast);
        if let Err(e) = parse_res.clone() {
            trace!("{}", convert_error(self.source_code, e));
        }

        match expect_success {
            false => assert!(parse_res.is_err()),
            true => assert!(parse_res.is_ok()),
        };

        self
    }

    pub fn test_eval(mut self, expect_success: bool) -> Self {
        trace!("test eval {}", self.name);
        let eval_res = self.compiler.eval_ast();

        if let Err(e) = &eval_res {
            trace!("Error: {}", e);
        }

        match expect_success {
            false => assert!(eval_res.is_err()),
            true => assert!(eval_res.is_ok()),
        }

        self
    }

    // this is *NOT* dead code, rust-analyzer!
    #[allow(dead_code)]
    pub fn test_compile(
        self,
        expect_success: bool,
    ) -> Result<(), Vec<(Scope, CompileError)>> {
        trace!("compile eval {}", self.name);
        let compile_res = self.compiler.compile().unwrap();

        let res = compile_res.packs();

        match expect_success {
            false => assert!(!res.1.is_empty()),
            true => assert!(!res.0.is_empty() && res.1.is_empty()),
        };

        Ok(())
    }
}
