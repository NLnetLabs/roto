use nom::error::convert_error;
use roto::{
    compile::{CompileError, Compiler}, blocks::Scope,
};

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
    ) -> Result<(), Vec<(Scope, CompileError)>> {
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
fn test_filter_map_garbage_1() {
    let _parsed_data = TestCompiler::create(
        "garbage_1",
        r###"
    filter-map garbage {
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
fn test_filter_map_00() {
    let mut _compiler = TestCompiler::create(
        "filter-map_1",
        r###"
        filter-map in-module-map {
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
fn test_filter_map_10() {
    let mut _compiler = TestCompiler::create(
        "filter-map-10",
        r###"
        filter-map my-module {
            define {
                rx_tx bgp_msg: BgpUpdateMessage;
            }
        
            term afi-safi-unicast {
                match {
                    bgp_msg.nlris.afi != IPV4;
                }
            }
        
            action send-message {
                mqtt.send({ 
                    message: String.format("🤭 I, the messager, saw {} in a BGP update.", AS3200)
                });
            }
        
            apply {
                filter match afi-safi-unicast matching {
                    return reject;
                };
            }
        }
        
        output-stream mqtt contains Message {
            message: String
        }
        "###
    )
    .test_parse(true)
    .test_eval(true)
    .test_compile(true);

}

#[test]
fn test_filter_map_11() {
    let mut _compiler = TestCompiler::create(
        "filter-map-10",
        r###"
        filter-map my-module {
            define {
                rx_tx bgp_msg: BgpUpdateMessage;
            }
        
            term afi-safi-unicast {
                match {
                    bgp_msg.nlris.afi != IPV4;
                }
            }
        
            action send-message {
                mqtt.send({ 
                    message: String.format("🤭 I, the messager, saw {} in a BGP update.", 3200)
                });
            }
        
            apply {
                filter match afi-safi-unicast matching {
                    return reject;
                };
            }
        }
        
        output-stream mqtt contains Message {
            message: String
        }
        "###
    )
    .test_parse(true)
    .test_eval(true)
    .test_compile(true);

}

#[test]
fn test_filter_map_20() {
    let mut _compiler = TestCompiler::create(
        "filter-map_1",
        r###"
        filter-map in-filter-map {
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
fn test_filter_map_30() {
    let _compiler = TestCompiler::create(
        "filter-map_1",
        r###"
        filter-map in-filter-map {
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
fn test_filter_map_31() {
    let _compiler = TestCompiler::create(
        "filter-map_1",
        r###"
        filter-map filter-map_1 {
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
                use my-filter-map;
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
fn test_filter_map_32() {
    let _compiler = TestCompiler::create(
        "filter-map_1",
        r###"
        filter-map filter-map_1 {
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
                use my-filter-map;
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
fn test_filter_map_33() {
    let _compiler = TestCompiler::create(
        "filter-map_1",
        r###"
        filter-map filter-map_1 {
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
                use my-filter-map;
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
fn test_filter_map_34() {
    let _compiler = TestCompiler::create(
        "filter-map_34",
        r###"
        filter-map filter-map_1 {
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
                use my-filter-map;
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