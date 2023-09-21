mod helpers;
mod common;

use helpers::TestCompiler;
use log::trace;

#[test]
fn test_filter_map_garbage_1() {
    common::init();
    
    let _parsed_data = TestCompiler::create(
        "garbage_1",
        r"
        filter-map garbage {
            ;dljhg;sdlkjgsdf
            d
            dg
            dfgijdfg
            sdfgsiofjg'\ae]-rg[pe,g]okd,g[pdgof
            ]]
        }",
    )
    .test_parse(false);
}

#[test]
fn test_filter_map_00() {
    common::init();

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
    common::init();

    let parser = TestCompiler::create(
        "filter-map-10",
        r#"
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
        "#
    )
    .test_parse(true);
    let evaluator = parser.test_eval(true);
    let compiler = evaluator.test_compile(true);
    trace!("{:#?}", compiler);
}

#[test]
fn test_filter_map_11() {
    common::init();

    let mut _compiler = TestCompiler::create(
        "filter-map-10",
        r#"
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
        "#
    )
    .test_parse(true)
    .test_eval(true)
    .test_compile(true);

}

#[test]
fn test_filter_map_20() {
    common::init();

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
    common::init();

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
    common::init();

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
    common::init();

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
    common::init();

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