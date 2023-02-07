#[cfg(test)]
use nom::error::convert_error;
use roto::compile::{CompileError, Compiler, RotoPack};

pub struct TestCompiler<'a> {
    name: &'a str,
    compiler: Compiler,
    source_code: &'a str,
}

impl<'a> TestCompiler<'a> {
    pub(crate) fn create(name: &'a str, source_code: &'a str) -> Self {
        Self {
            name,
            compiler: Compiler::new(),
            source_code,
        }
    }

    pub(crate) fn test_parse(mut self, expect_success: bool) -> Self {
        println!("test parse {}", self.name);

        let parse_res = self.compiler.parse_source_code(self.source_code);

        println!("{} {:#?}", self.name, self.compiler.ast);
        if let Err(e) = parse_res.clone() {
            println!("{}", convert_error(self.source_code, e));
        }

        match expect_success {
            false => assert!(parse_res.is_err()),
            true => assert!(parse_res.is_ok()),
        };

        self
    }

    pub(crate) fn test_eval(mut self, expect_success: bool) -> Self {
        println!("test eval {}", self.name);
        let eval_res = self.compiler.eval_ast();

        if let Err(e) = &eval_res {
            println!("Error: {}", e);
        }
        
        match expect_success {
            false => assert!(eval_res.is_err()),
            true => assert!(eval_res.is_ok()),
        }

        self
    }

    pub(crate) fn test_compile(
        self,
        expect_success: bool,
    ) -> Vec<Result<RotoPack, CompileError>> {
        println!("compile eval {}", self.name);
        let compile_res = self.compiler.compile();

        for compiled_mod in &compile_res {
            match expect_success {
                false => assert!(compiled_mod.is_err()),
                true => assert!(compiled_mod.is_ok()),
            };
        }

        compile_res
    }
}

#[test]
fn test_module_garbage_1() {
    let _parsed_data = TestCompiler::create(
        "garbage_1",
        r###"
    module garbage {
        ;dljhg;sdlkjgsdf
        d
        dg
        dfgijdfg
        sdfgsiofjg'\ae]-rg[pe,g]okd,g[pdgof
        ]]
    }
    "###,
    )
    .test_parse(false);
}

#[test]
fn test_module_1() {
    let mut _compiler = TestCompiler::create(
        "module_1",
        r###"
        module in-module {
            define {
                rx_tx route: MyRoute;
            }

            term on-my-terms {
                match {
                    route.as-path.origin() == AS211321;
                }
            }
        }

        rib ribber contains MyRoute {
            prefix: Prefix,
            as-path: AsPath
        }
        "###,
    )
    .test_parse(true)
    .test_eval(true)
    .test_compile(true);
}

#[test]
fn test_module_2() {
    let _compiler = TestCompiler::create(
        "module_1",
        r###"
        module in-module {
            define {
                rx route: ExtRoute;
                tx ext_route: ExtRoute;
            }

            term on-my-terms {
                match {
                    route.as-path.origin() == AS211321;
                }
            }
        }

        rib ext-rib contains ExtRoute {
            prefix: Prefix,
            as-path: AsPath
        }
        "###,
    )
    .test_parse(true)
    .test_eval(true)
    .test_compile(true);
}