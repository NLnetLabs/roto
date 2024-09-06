#![cfg(any())]

use log::{info, trace};
use roto::ast::AcceptReject;

use roto::blocks::Scope::{self, FilterMap};
use roto::pipeline;
use roto::types::builtin::{
    NlriStatus, PeerId, PeerRibType, Provenance, RouteContext,
};
use roto::types::collections::Record;
use roto::types::typedef::TypeDef;
use roto::vm::{self, VmResult};

use inetnum::asn::Asn;

mod common;

fn test_data(
    name: Scope,
    source_code: &str,
) -> Result<VmResult, Box<dyn std::error::Error>> {
    info!("Evaluate filter-map {}...", name);

    // Compile the source code in this example
    let rotolo = pipeline::run_test(source_code, None)?;
    let roto_pack = rotolo.retrieve_pack_as_refs(&name)?;

    let payload_type =
        TypeDef::new_record_type(vec![("asn", Box::new(TypeDef::Asn))])?;

    let payload = Record::create_instance_with_ordered_fields(
        &payload_type,
        vec![("asn", Asn::from(65534_u32).into())],
    )?;

    for mb in roto_pack.get_mir().iter() {
        println!("{}", mb);
    }

    let peer_ip = "192.0.2.0".parse().unwrap();

    let provenance = Provenance {
        timestamp: chrono::Utc::now(),
        connection_id: "192.0.2.10:178".parse().unwrap(),
        peer_id: PeerId {
            addr: peer_ip,
            asn: Asn::from(65534),
        },
        peer_bgp_id: [0; 4].into(),
        peer_distuingisher: [0; 8],
        peer_rib_type: PeerRibType::OutPost,
    };

    let context = RouteContext::new(None, NlriStatus::Empty, provenance);

    let mut vm = vm::VmBuilder::new()
        .with_data_sources(roto_pack.data_sources)
        .with_context(&context)
        .with_mir_code(roto_pack.mir)
        .build()?;

    let mem = &mut vm::LinearMemory::uninit();
    let res = vm.exec(payload, None::<Record>, None, mem)?;

    trace!("\nRESULT");
    trace!("action: {}", res.accept_reject);
    trace!("rx    : {:?}", res.rx);
    trace!("tx    : {:?}", res.tx);

    Ok(res)
}

fn src_code(code_line: &str, asn: &str, end_accept_reject: &str) -> String {
    let pre = format!(
        r###"
    filter-map my-filter-map {{
        define {{
            // specify the types of that this filter receives
            // and sends.
            // rx_tx route: StreamRoute;
            rx pph_asn: MyRec;
            tx out: Asn;
        }}

        term peer-asn-matches {{
            match {{
                pph_asn.asn == {};
            }}
        }}

        action set-asn {{
            pph_asn.asn.set(AS200);
        }}

        action set-again-asn {{
            pph_asn.asn.set(AS300);
        }}

        apply {{
            {}
            return {};
        }}
    }}

    type MyRec {{
        asn: Asn
    }}
    "###,
        asn, code_line, end_accept_reject
    );

    pre
}

#[test]
fn test_filter_map_10() {
    common::init();
    let src_line = &src_code(
        "filter match peer-asn-matches matching { return accept; };",
        "AS65534",
        "reject",
    );
    let test_run = test_data(FilterMap("my-filter-map".into()), src_line);

    if let Err(e) = &test_run {
        println!("{}", e);
        unreachable!();
    }

    let VmResult { accept_reject, .. } = test_run.unwrap();
    assert_eq!(accept_reject, AcceptReject::Accept);
}

#[test]
fn test_filter_map_11() {
    common::init();
    let src_line = &src_code(
        "filter match peer-asn-matches matching { return accept; };",
        "AS0",
        "reject",
    );
    let test_run = test_data(FilterMap("my-filter-map".into()), src_line);

    assert!(test_run.is_ok());

    let VmResult { accept_reject, .. } = test_run.unwrap();
    assert_eq!(accept_reject, AcceptReject::Reject);
}

#[test]
fn test_filter_map_12() {
    common::init();
    let src_line = &src_code(
        "filter match peer-asn-matches matching { return reject; };",
        "AS0",
        "accept",
    );
    let test_run = test_data(FilterMap("my-filter-map".into()), src_line);

    assert!(test_run.is_ok());

    let VmResult { accept_reject, .. } = test_run.unwrap();
    assert_eq!(accept_reject, AcceptReject::Accept);
}

#[test]
fn test_filter_map_20() {
    common::init();
    let src_line = &src_code(
        "filter match peer-asn-matches matching { return reject; };",
        "AS65534",
        "reject",
    );
    let test_run = test_data(FilterMap("my-filter-map".into()), src_line);

    assert!(test_run.is_ok());

    let VmResult { accept_reject, .. } = test_run.unwrap();
    assert_eq!(accept_reject, AcceptReject::Reject);
}

#[test]
fn test_filter_map_21() {
    let src_line = &src_code(
        "filter match peer-asn-matches matching { return reject; };",
        "AS0",
        "reject",
    );
    let test_run = test_data(FilterMap("my-filter-map".into()), src_line);

    let VmResult { accept_reject, .. } = test_run.unwrap();
    assert_eq!(accept_reject, AcceptReject::Reject);
}

#[test]
fn test_filter_map_22() {
    let src_line = &src_code(
        "filter match peer-asn-matches matching { return reject; };",
        "AS0",
        "accept",
    );
    let test_run = test_data(FilterMap("my-filter-map".into()), src_line);

    assert!(test_run.is_ok());

    let VmResult { accept_reject, .. } = test_run.unwrap();
    assert_eq!(accept_reject, AcceptReject::Accept);
}

#[test]
fn test_filter_map_30() {
    let src_line = &src_code(
        "filter match peer-asn-matches matching { set-asn; };",
        "AS65534",
        "reject",
    );
    let test_run = test_data(FilterMap("my-filter-map".into()), src_line);

    assert!(test_run.is_ok());

    let VmResult { accept_reject, .. } = test_run.unwrap();
    assert_eq!(accept_reject, AcceptReject::Reject);
}

#[test]
fn test_filter_map_31() {
    let src_line = &src_code(
        "filter match peer-asn-matches matching { set-asn; };",
        "AS0",
        "reject",
    );
    let test_run = test_data(FilterMap("my-filter-map".into()), src_line);

    assert!(test_run.is_ok());

    let VmResult { accept_reject, .. } = test_run.unwrap();
    assert_eq!(accept_reject, AcceptReject::Reject);
}

#[test]
fn test_filter_map_32() {
    let src_line = &src_code(
        "filter match peer-asn-matches matching { set-asn; };",
        "AS0",
        "accept",
    );
    let test_run = test_data(FilterMap("my-filter-map".into()), src_line);

    assert!(test_run.is_ok());

    let VmResult { accept_reject, .. } = test_run.unwrap();
    assert_eq!(accept_reject, AcceptReject::Accept);
}

#[test]
fn test_filter_map_40() {
    let src_line = &src_code(
        "filter match peer-asn-matches matching { set-asn; return accept; };",
        "AS65534",
        "reject",
    );
    let test_run = test_data(FilterMap("my-filter-map".into()), src_line);

    assert!(test_run.is_ok());

    let VmResult { accept_reject, .. } = test_run.unwrap();
    assert_eq!(accept_reject, AcceptReject::Accept);
}

#[test]
fn test_filter_map_41() {
    common::init();

    let src_line = &src_code(
        "filter match peer-asn-matches matching { set-asn; return accept; };",
        "AS0",
        "reject",
    );
    let test_run = test_data(FilterMap("my-filter-map".into()), src_line);

    assert!(test_run.is_ok());

    let VmResult { accept_reject, .. } = test_run.unwrap();
    assert_eq!(accept_reject, AcceptReject::Reject);
}

#[test]
fn test_filter_map_42() {
    let src_line = &src_code(
        "filter match peer-asn-matches matching { set-asn; return accept; };",
        "AS0",
        "accept",
    );
    let test_run = test_data(FilterMap("my-filter-map".into()), src_line);

    assert!(test_run.is_ok());

    let VmResult { accept_reject, .. } = test_run.unwrap();
    assert_eq!(accept_reject, AcceptReject::Accept);
}

#[test]
fn test_filter_map_50() {
    let src_line = &src_code(
        "filter match peer-asn-matches matching { set-asn; return reject; };",
        "AS65534",
        "reject",
    );
    let test_run = test_data(FilterMap("my-filter-map".into()), src_line);

    assert!(test_run.is_ok());

    let VmResult { accept_reject, .. } = test_run.unwrap();
    assert_eq!(accept_reject, AcceptReject::Reject);
}

#[test]
fn test_filter_map_51() {
    let src_line = &src_code(
        "filter match peer-asn-matches matching { set-asn; return reject; };",
        "AS0",
        "reject",
    );
    let test_run = test_data(FilterMap("my-filter-map".into()), src_line);

    assert!(test_run.is_ok());

    let VmResult { accept_reject, .. } = test_run.unwrap();
    assert_eq!(accept_reject, AcceptReject::Reject);
}

#[test]
fn test_filter_map_52() {
    let src_line = &src_code(
        "filter match peer-asn-matches matching { set-asn; return reject; };",
        "AS0",
        "accept",
    );
    let test_run = test_data(FilterMap("my-filter-map".into()), src_line);

    assert!(test_run.is_ok());

    let VmResult { accept_reject, .. } = test_run.unwrap();
    assert_eq!(accept_reject, AcceptReject::Accept);
}

#[test]
fn test_filter_map_60() {
    let src_line = &src_code(
        "filter match peer-asn-matches matching { set-asn; set-again-asn; };",
        "AS65534",
        "reject",
    );
    let test_run = test_data(FilterMap("my-filter-map".into()), src_line);

    assert!(test_run.is_ok());

    let VmResult { accept_reject, .. } = test_run.unwrap();
    assert_eq!(accept_reject, AcceptReject::Reject);
}

#[test]
fn test_filter_map_61() {
    let src_line = &src_code(
        "filter match peer-asn-matches matching { set-asn; set-again-asn; };",
        "AS0",
        "reject",
    );
    let test_run = test_data(FilterMap("my-filter-map".into()), src_line);

    assert!(test_run.is_ok());

    let VmResult { accept_reject, .. } = test_run.unwrap();
    assert_eq!(accept_reject, AcceptReject::Reject);
}

#[test]
fn test_filter_map_62() {
    let src_line = &src_code(
        "filter match peer-asn-matches matching { set-asn; set-again-asn; };",
        "AS0",
        "accept",
    );
    let test_run = test_data(FilterMap("my-filter-map".into()), src_line);

    assert!(test_run.is_ok());

    let VmResult { accept_reject, .. } = test_run.unwrap();
    assert_eq!(accept_reject, AcceptReject::Accept);
}

#[test]
fn test_filter_map_70() {
    let src_line = &src_code(
        r#"filter match peer-asn-matches matching { 
            set-asn; 
            set-again-asn; 
            return accept; 
        };"#,
        "AS65534",
        "reject",
    );
    let test_run = test_data(FilterMap("my-filter-map".into()), src_line);

    assert!(test_run.is_ok());

    let VmResult { accept_reject, .. } = test_run.unwrap();
    assert_eq!(accept_reject, AcceptReject::Accept);
}

#[test]
fn test_filter_map_71() {
    let src_line = &src_code(
        r#"filter match peer-asn-matches matching { 
            set-asn; 
            set-again-asn; 
            return accept; 
        };"#,
        "AS0",
        "reject",
    );
    let test_run = test_data(FilterMap("my-filter-map".into()), src_line);

    assert!(test_run.is_ok());

    let VmResult { accept_reject, .. } = test_run.unwrap();
    assert_eq!(accept_reject, AcceptReject::Reject);
}

#[test]
fn test_filter_map_72() {
    common::init();

    let src_line = &src_code(
        r#"filter match peer-asn-matches matching { 
            set-asn; 
            set-again-asn; 
            return accept; 
        };"#,
        "AS0",
        "accept",
    );
    let test_run = test_data(FilterMap("my-filter-map".into()), src_line);

    trace!("{:?}", test_run);
    assert!(test_run.is_ok());

    let VmResult { accept_reject, .. } = test_run.unwrap();
    assert_eq!(accept_reject, AcceptReject::Accept);
}

#[test]
fn test_filter_map_80() {
    common::init();
    let src_line = &src_code(
        r#"filter match peer-asn-matches matching { 
        set-asn; 
        set-again-asn; 
        return reject; };"#,
        "AS65534",
        "reject",
    );
    let test_run = test_data(FilterMap("my-filter-map".into()), src_line);

    assert!(test_run.is_ok());

    let VmResult { accept_reject, .. } = test_run.unwrap();
    assert_eq!(accept_reject, AcceptReject::Reject);
}

#[test]
fn test_filter_map_81() {
    common::init();
    let src_line = &src_code(
        r#"filter match peer-asn-matches matching { 
        set-asn; 
        set-again-asn; 
        return reject; 
    };"#,
        "AS65534",
        "reject",
    );
    let test_run = test_data(FilterMap("my-filter-map".into()), src_line);

    trace!("test run {:?}", test_run);
    assert!(test_run.is_ok());

    let VmResult { accept_reject, .. } = test_run.unwrap();
    assert_eq!(accept_reject, AcceptReject::Reject);
}

#[test]
fn test_filter_map_82() {
    common::init();
    let src_line = &src_code(
        r#"filter match peer-asn-matches matching { 
        set-asn; 
        set-again-asn; 
        return reject; 
    };"#,
        "AS65534",
        "accept",
    );
    let test_run = test_data(FilterMap("my-filter-map".into()), src_line);

    assert!(test_run.is_ok());

    let VmResult { accept_reject, .. } = test_run.unwrap();
    assert_eq!(accept_reject, AcceptReject::Reject);
}
