#![cfg(any())]
use log::trace;
use roto::pipeline::{self, RotoReport};

pub struct TestCompiler<'a> {
    name: &'a str,
    source_code: &'a str,
}

impl<'a> TestCompiler<'a> {
    pub fn create(name: &'a str, source_code: &'a str) -> Self {
        Self { name, source_code }
    }

    #[track_caller]
    pub fn test_parse(self, expect_success: bool) -> Self {
        trace!("test parse {}", self.name);

        let files = pipeline::test_file(self.source_code);
        let parse_res = pipeline::parse(&files);

        match expect_success {
            false => assert!(parse_res.is_err()),
            true => assert!(parse_res.is_ok()),
        };

        self
    }

    #[track_caller]
    pub fn test_eval(self, expect_success: bool) -> Self {
        trace!("test eval {}", self.name);

        let files = pipeline::test_file(self.source_code);
        let trees = pipeline::parse(&files).unwrap();
        let typeres = pipeline::typecheck(&files, &trees);
        if let Err(e) = typeres {
            eprintln!("{e}");
            panic!("Got a type error before evaluation");
        }
        let eval_res = pipeline::evaluate(&files, &trees);

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
    ) -> Result<(), RotoReport> {
        trace!("compile eval {}", self.name);

        let files = pipeline::test_file(self.source_code);
        let trees = pipeline::parse(&files).unwrap();
        pipeline::typecheck(&files, &trees).unwrap();
        let symbols = pipeline::evaluate(&files, &trees).unwrap();
        let res = pipeline::compile(&files, &symbols, None);

        match expect_success {
            false => assert!(res.is_err()),
            true => assert!(res.is_ok()),
        };

        res.map(|_| ())
    }
}
