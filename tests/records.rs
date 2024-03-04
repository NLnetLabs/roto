use log::trace;
use roto::ast::AcceptReject;
use roto::compiler::Compiler;

use roto::blocks::Scope::{self, FilterMap};
use roto::types::builtin::{NlriStatus, PeerId, PeerRibType, Provenance, RouteContext};
use roto::types::collections::Record;
use roto::types::typedef::TypeDef;
use roto::types::typevalue::TypeValue;
use roto::vm::{self, VmResult};

use rotonda_store::prelude::MergeUpdate;

use routecore::asn::Asn;

mod common;

#[derive(Debug, Clone)]
struct RibValue(Vec<TypeValue>);

impl MergeUpdate for RibValue {
    type UserDataIn = ();
    type UserDataOut = ();

    fn merge_update(
        &mut self,
        update_record: RibValue,
        _: Option<&Self::UserDataIn>,
    ) -> Result<(), Box<dyn std::error::Error>> {
        self.0 = update_record.0;
        Ok(())
    }

    fn clone_merge_update(
        &self,
        update_meta: &Self,
        _: Option<&Self::UserDataIn>,
    ) -> Result<(Self, Self::UserDataOut), Box<dyn std::error::Error>>
    where
        Self: std::marker::Sized,
    {
        let mut new_meta = update_meta.0.clone();
        new_meta.push(self.0[0].clone());
        Ok((RibValue(new_meta), ()))
    }
}

impl std::fmt::Display for RibValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

fn src_code(
    record_assign: &str,
    code_line: &str,
    end_accept_reject: &str,
) -> String {
    let pre = format!(
        r###"
        filter-map in-filter-map {{
            define {{
                rx_tx msg: BmpMsg;
                c = 99;
                a = {};
            }}

            term peer-asn-matches {{
                match {{
                    {}
                }}
            }}

            apply {{
                filter match peer-asn-matches matching {{ return {}; }};
                return accept;
            }}
        }}

        type BmpMsg {{
            type: U8,
            asn: Asn
        }}

        type A {{
            asn: Asn,
            i: U8
        }}

        type C {{
            asn: Asn,
            i: {{ f: U8, g: Asn }},
            d: U32
        }}

        type D {{
            asn: Asn,
            i: {{ f: U8, g: Asn, h: {{ k: U32, l: U8 }} }},
            d: U32
        }}
    "###,
        record_assign, code_line, end_accept_reject
    );

    pre
}

fn test_data(
    name: Scope,
    source_code: &str,
) -> Result<VmResult, Box<dyn std::error::Error>> {
    trace!("Evaluate filter-map {}...", name);

    let c = Compiler::new();
    let roto_packs = c.build_from_compiler(source_code)?;

    let roto_pack = roto_packs.retrieve_pack_as_refs(&name)?;
    let asn: TypeValue = Asn::from_u32(211321).into();

    println!("ASN {:?}", asn);

    let my_rec_type = TypeDef::new_record_type(vec![
        ("type", Box::new(TypeDef::U8)),
        ("asn", Box::new(TypeDef::Asn)),
    ])
    .unwrap();

    let my_payload = Record::create_instance_with_ordered_fields(
        &my_rec_type,
        vec![("type", TypeValue::from(1_u8)), ("asn", asn)],
    )
    .unwrap();

    let mem = &mut vm::LinearMemory::uninit();

    println!("Used Arguments");
    println!("{:#?}", &roto_pack.arguments);
    println!("Used Data Sources");
    println!("{:#?}", &roto_pack.data_sources);

    for mb in roto_pack.get_mir().iter() {
        println!("{}", mb);
    }

    let peer_ip = "192.0.2.0".parse().unwrap();

    let provenance = Provenance {
        timestamp: chrono::Utc::now(),
        router_id: 0,
        connection_id: 0,
        peer_id: PeerId { addr: peer_ip, asn: Asn::from(65534) },
        peer_bgp_id: [0; 4].into(),
        peer_distuingisher: [0; 8],
        peer_rib_type: PeerRibType::OutPost,
    };

    let context = RouteContext::new(None, NlriStatus::InConvergence, provenance);

    let mut vm = vm::VmBuilder::new()
        .with_context(context)
        .with_data_sources(roto_pack.data_sources)
        .with_mir_code(roto_pack.mir)
        .build()?;

    let res = vm.exec(my_payload, None::<Record>, None, mem).unwrap();

    println!("\nRESULT");
    println!("action: {}", res.accept_reject);
    println!("rx    : {:?}", res.rx);
    println!("tx    : {:?}", res.tx);

    Ok(res)
}

#[test]
fn test_records_compare_1() {
    common::init();
    let src_line = src_code(
        // c = 99
        "A { i: c, asn: AS100 }",
        "100 in [a.i,2,3,4,5]; // Peer Down",
        "reject",
    );
    let test_run = test_data(FilterMap("in-filter-map".into()), &src_line);

    let VmResult { accept_reject, .. } = test_run.unwrap();
    assert_eq!(accept_reject, AcceptReject::Accept);
}

#[test]
fn test_records_compare_2() {
    common::init();
    let src_line = src_code(
        r#"A { asn: "stringetje", i: c }"#,
        "100 in [a.i,2,3,4,5]; // Peer Down",
        "reject",
    );
    let test_run = test_data(FilterMap("in-filter-map".into()), &src_line)
        .unwrap_err()
        .to_string();

    assert_eq!(
        test_run,
        "Eval error: The field name 'asn' has the wrong type. Expected 'Asn', but got 'String'"
    );
}

#[test]
fn test_records_compare_3() {
    common::init();
    let src_line = src_code(
        "A { asn: 200, i: c }",
        "100 in [a.i, 2,3,4,5]; // Peer Down",
        "reject",
    );
    let test_run = test_data(FilterMap("in-filter-map".into()), &src_line);

    let VmResult { accept_reject, .. } = test_run.unwrap();
    assert_eq!(accept_reject, AcceptReject::Accept);
}

#[test]
fn test_records_compare_4() {
    common::init();
    let src_line = src_code(
        "A { garbage_field: AS100, i: c }",
        "100 in [a.i, 2,3,4,5]; // Peer Down",
        "reject",
    );
    let test_run = test_data(FilterMap("in-filter-map".into()), &src_line)
        .unwrap_err()
        .to_string();
    assert_eq!(test_run, "Eval error: The field name 'garbage_field' cannot be found in type 'A'");
}

#[test]
fn test_records_compare_5() {
    common::init();
    let src_line = src_code(
        "B { asn: AS100, i: c }",
        "100 in [a.i, 2,3,4,5]; // Peer Down",
        "reject",
    );
    let test_run = test_data(FilterMap("in-filter-map".into()), &src_line)
        .unwrap_err()
        .to_string();
    assert_eq!(
        test_run,
        "Eval error: No type named 'B' found in scope 'filter-map 'in-filter-map''"
    );
}

#[test]
fn test_records_compare_6() {
    common::init();
    let src_line = src_code(
        "A { asn: AS100 }",
        "100 in [a.i, 2,3,4,5]; // Peer Down",
        "reject",
    );
    let test_run = test_data(FilterMap("in-filter-map".into()), &src_line)
        .unwrap_err()
        .to_string();
    assert_eq!(
        test_run,
        "The filter-map filter-map 'in-filter-map' was defined for but contained the following error:\n\
        This record: {\n\tasn: AS100\n   } is of type Record {asn: Asn, i: U8, }, but we got a record with type Record {asn: Asn, }. It's not the same and cannot be converted."
    );
}

#[test]
fn test_records_compare_7() {
    common::init();
    let src_line = src_code(
        "A { asn: AS100, i: c, d: 400 }",
        "100 in [a.i, 2,3,4,5]; // Peer Down",
        "reject",
    );
    let test_run = test_data(FilterMap("in-filter-map".into()), &src_line)
        .unwrap_err()
        .to_string();
    assert_eq!(
        test_run,
        "Eval error: The field name 'd' cannot be found in type 'A'"
    );
}

#[test]
fn test_records_compare_8() {
    common::init();
    let src_line = src_code(
        "C { asn: AS100, i: { f: 200, g: AS24 }, d: 400 }",
        "100 in [a.i, 2,3,4,5]; // Peer Down",
        "reject",
    );
    let test_run = test_data(FilterMap("in-filter-map".into()), &src_line)
        .unwrap_err()
        .to_string();
    assert_eq!(
        test_run,
        "Eval error: Record {f: U8, g: Asn, } cannot be converted into IntegerLiteral"
    );
}

#[test]
fn test_records_compare_9() {
    common::init();
    let src_line = src_code(
        "C { asn: AS100, i: { f: 200, g: AS24, h: { k: U32, l: U8 } }, d: 400 }",
        "100 in [a.i,2,3,4,5]; // Peer Down",
        "reject",
    );
    let test_run = test_data(FilterMap("in-filter-map".into()), &src_line)
        .unwrap_err()
        .to_string();
    assert_eq!(
        test_run,
        "Eval error: The sub-field name 'h' cannot be found in field 'i' in type 'C'"
    );
}

#[test]
fn test_records_compare_10() {
    common::init();
    let src_line = src_code(
        "C { asn: AS100, i: { f: 200, g: AS24, h: { k: U32, l: U8 } }, d: 400 }",
        "100 in [1,2,3,4,5]; // Peer Down",
        "reject",
    );
    let test_run = test_data(FilterMap("in-filter-map".into()), &src_line)
        .unwrap_err()
        .to_string();
    assert_eq!(
        test_run,
        "Eval error: The sub-field name 'h' cannot be found in field 'i' in type 'C'"
    );
}

#[test]
fn test_records_compare_11() {
    common::init();
    let src_line = src_code(
        "D { asn: AS100, i: { f: 200, g: AS24, h: { k: 86400, l: 2 } }, d: 400 }",
        "100 in [a.i.f,2,3,4,5]; // Peer Down",
        "reject",
    );
    let test_run = test_data(FilterMap("in-filter-map".into()), &src_line);

    let VmResult { accept_reject, .. } = test_run.unwrap();
    assert_eq!(accept_reject, AcceptReject::Accept);
}

#[test]
fn test_records_compare_12() {
    common::init();
    let src_line = src_code(
        "D { asn: AS99, i: { f: 86400, g: AS24, h: { l:2, k: 100 } }, d: 400 }",
        "100 in [a.i.h.k,2,3,4,5]; // Peer Down",
        "reject",
    );
    let test_run = test_data(FilterMap("in-filter-map".into()), &src_line);

    let VmResult { accept_reject, .. } = test_run.unwrap();
    assert_eq!(accept_reject, AcceptReject::Reject);
}
