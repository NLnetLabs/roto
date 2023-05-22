use nom::error::convert_error;
use roto::{
    ast::ShortString,
    compile::{CompileError, Compiler},
};

pub struct TestCompiler<'a> {
    name: &'a str,
    compiler: Compiler<'a>,
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
    ) -> Result<(), Vec<(ShortString, CompileError)>> {
        println!("compile eval {}", self.name);
        let compile_res = self.compiler.compile();

        let res = if compile_res.is_success() {
            Ok(())
        } else {
            Err(compile_res.get_mis_compilations().to_vec())
        };

        match expect_success {
            false => assert!(res.is_err()),
            true => assert!(res.is_ok()),
        };

        res
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
fn test_module_00() {
    let mut _compiler = TestCompiler::create(
        "module_1",
        r###"
        module in-module {
            define {
                rx_tx route: Route;
            }

            term on-my-terms {
                match {
                    route.as-path.origin() == AS211321;
                }
            }
        }
        "###,
    )
    .test_parse(true)
    .test_eval(true)
    .test_compile(true);
}

#[test]
fn test_module_20() {
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
fn test_module_30() {
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

#[test]
fn test_module_31() {
    let _compiler = TestCompiler::create(
        "module_1",
        r###"
        module module_1 {
            define {
                // specify the types of that this filter receives
                // and sends.
                // rx_tx route: StreamRoute;
                rx pph_asn: MyRec;
                tx out: Asn;
            }

            term peer-asn-matches {
                match {
                    pph_asn.asn == AS65534;
                }
            }

            action set-asn {
                pph_asn.asn.set(AS200);
            }

            apply {
                use my-module;
                filter match peer-asn-matches matching { set-asn; return accept; };
                return reject;
            }
        }

        type MyRec {
            asn: Asn
        }
        "###,
    )
    .test_parse(true)
    .test_eval(true)
    .test_compile(true);
}

#[test]
fn test_module_32() {
    let _compiler = TestCompiler::create(
        "module_1",
        r###"
        module module_1 {
            define {
                // specify the types of that this filter receives
                // and sends.
                // rx_tx route: StreamRoute;
                rx pph_asn: MyRec;
                tx out: Asn;
            }

            term peer-asn-matches {
                match {
                    pph_asn.i_dont_exist == AS65534;
                }
            }

            action set-asn {
                pph_asn.asn.set(AS200);
            }

            apply {
                use my-module;
                filter match peer-asn-matches matching { set-asn; return accept; };
                return reject;
            }
        }

        type MyRec {
            asn: Asn
        }
        "###,
    )
    .test_parse(true)
    .test_eval(false);
}

#[test]
fn test_module_33() {
    let _compiler = TestCompiler::create(
        "module_1",
        r###"
        module module_1 {
            define {
                // specify the types of that this filter receives
                // and sends.
                // rx_tx route: StreamRoute;
                rx pph_asn: MyRec;
                tx out: Asn;
            }

            term peer-asn-matches {
                match {
                    pph_asn.asn == AS65534;
                }
            }

            action set-asn {
                pph_asn.asn.set(AS200);
            }

            apply {
                use my-module;
                filter match peer-asn-matches matching { set-asn; return accept; };
                return reject;
            }
        }

        type IamNotUsed {
            asn: Asn
        }
        "###,
    )
    .test_parse(true)
    .test_eval(false);
}

#[test]
fn test_module_34() {
    let _compiler = TestCompiler::create(
        "module_34",
        r###"
        module module_1 {
            define {
                // specify the types of that this filter receives
                // and sends.
                // rx_tx route: StreamRoute;
                rx pph_asn: PerPeerHeader;
                tx out: PerPeerHeader;
            }

            term peer-asn-matches {
                match {
                    pph_asn.asn == AS65534;
                }
            }

            action set-asn {
                pph_asn.asn.set(AS200);
                // send-to(mqtt, pph_asn);
            }

            apply {
                use my-module;
                filter match peer-asn-matches matching { set-asn; return accept; };
                return reject;
            }
        }

        type PerPeerHeader {
            asn: Asn
        }
        
        type BmpMsg {
            type: U8,
            header: { asn: Asn }
        }
        "###,
    )
    .test_parse(true)
    .test_eval(true);
}