use log::{trace, info};
use roto::ast::AcceptReject;
use roto::compile::Compiler;

use roto::types::builtin::Asn;
use roto::types::collections::Record;
use roto::types::typedef::TypeDef;
use roto::types::typevalue::TypeValue;
use roto::vm;

mod common;

fn test_data(
    name: &str,
    source_code: &str,
) -> Result<
    (AcceptReject, TypeValue, std::option::Option<TypeValue>),
    Box<dyn std::error::Error>,
> {
    info!("Evaluate module {}...", name);

    // Compile the source code in this example
    let rotolo = Compiler::build(source_code)?;
    let roto_pack = rotolo.retrieve_public_as_arcs(name)?;

    let payload_type =
        TypeDef::new_record_type(vec![("asn", Box::new(TypeDef::Asn))])?;

    let payload = Record::create_instance(
        &payload_type,
        vec![("asn", Asn::from(65534).into())],
    )?;
    let ds_ref = roto_pack.data_sources.iter().collect::<Vec<_>>();

    let mut vm = vm::VmBuilder::new()
        .with_data_sources(ds_ref)
        .with_mir_code(roto_pack.mir)
        .build();

    let mem = &mut vm::LinearMemory::uninit();
    let res = vm.exec(payload, None::<Record>, None, mem)?;

    trace!("\nRESULT");
    trace!("action: {}", res.0);
    trace!("rx    : {:?}", res.1);
    trace!("tx    : {:?}", res.2);

    Ok(res)
}

fn src_code(code_line: &str, asn: &str, end_accept_reject: &str) -> String {
    let pre = format!(
        r###"
    module my-module {{
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
fn test_module_10() {
    common::init();
    let src_line = &src_code(
        "filter match peer-asn-matches matching { return accept; };",
        "AS65534",
        "reject",
    );
    let test_run = test_data("my-module", src_line);

    assert!(test_run.is_ok());

    let (ar, _rx, _tx) = test_run.unwrap();
    assert_eq!(ar, AcceptReject::Accept);
}

#[test]
fn test_module_11() {
    common::init();
    let src_line = &src_code(
        "filter match peer-asn-matches matching { return accept; };",
        "AS0",
        "reject",
    );
    let test_run = test_data("my-module", src_line);

    assert!(test_run.is_ok());

    let (ar, _rx, _tx) = test_run.unwrap();
    assert_eq!(ar, AcceptReject::Reject);
}

#[test]
fn test_module_12() {
    common::init();
    let src_line = &src_code(
        "filter match peer-asn-matches matching { return reject; };",
        "AS0",
        "accept",
    );
    let test_run = test_data("my-module", src_line);

    assert!(test_run.is_ok());

    let (ar, _rx, _tx) = test_run.unwrap();
    assert_eq!(ar, AcceptReject::Accept);
}

#[test]
fn test_module_20() {
    common::init();
    let src_line = &src_code(
        "filter match peer-asn-matches matching { return reject; };",
        "AS65534",
        "reject",
    );
    let test_run = test_data("my-module", src_line);

    assert!(test_run.is_ok());

    let (ar, _rx, _tx) = test_run.unwrap();
    assert_eq!(ar, AcceptReject::Reject);
}

#[test]
fn test_module_21() {
    let src_line = &src_code(
        "filter match peer-asn-matches matching { return reject; };",
        "AS0",
        "reject",
    );
    let test_run = test_data("my-module", src_line);

    assert!(test_run.is_ok());

    let (ar, _rx, _tx) = test_run.unwrap();
    assert_eq!(ar, AcceptReject::Reject);
}

#[test]
fn test_module_22() {
    let src_line = &src_code(
        "filter match peer-asn-matches matching { return reject; };",
        "AS0",
        "accept",
    );
    let test_run = test_data("my-module", src_line);

    assert!(test_run.is_ok());

    let (ar, _rx, _tx) = test_run.unwrap();
    assert_eq!(ar, AcceptReject::Accept);
}

#[test]
fn test_module_30() {
    let src_line = &src_code(
        "filter match peer-asn-matches matching { set-asn; };",
        "AS65534",
        "reject",
    );
    let test_run = test_data("my-module", src_line);

    assert!(test_run.is_ok());

    let (ar, _rx, _tx) = test_run.unwrap();
    assert_eq!(ar, AcceptReject::Reject);
}

#[test]
fn test_module_31() {
    let src_line = &src_code(
        "filter match peer-asn-matches matching { set-asn; };",
        "AS0",
        "reject",
    );
    let test_run = test_data("my-module", src_line);

    assert!(test_run.is_ok());

    let (ar, _rx, _tx) = test_run.unwrap();
    assert_eq!(ar, AcceptReject::Reject);
}

#[test]
fn test_module_32() {
    let src_line = &src_code(
        "filter match peer-asn-matches matching { set-asn; };",
        "AS0",
        "accept",
    );
    let test_run = test_data("my-module", src_line);

    assert!(test_run.is_ok());

    let (ar, _rx, _tx) = test_run.unwrap();
    assert_eq!(ar, AcceptReject::Accept);
}

#[test]
fn test_module_40() {
    let src_line = &src_code(
        "filter match peer-asn-matches matching { set-asn; return accept; };",
        "AS65534",
        "reject",
    );
    let test_run = test_data("my-module", src_line);

    assert!(test_run.is_ok());

    let (ar, _rx, _tx) = test_run.unwrap();
    assert_eq!(ar, AcceptReject::Accept);
}

#[test]
fn test_module_41() {
    let src_line = &src_code(
        "filter match peer-asn-matches matching { set-asn; return accept; };",
        "AS0",
        "reject",
    );
    let test_run = test_data("my-module", src_line);

    assert!(test_run.is_ok());

    let (ar, _rx, _tx) = test_run.unwrap();
    assert_eq!(ar, AcceptReject::Reject);
}

#[test]
fn test_module_42() {
    let src_line = &src_code(
        "filter match peer-asn-matches matching { set-asn; return accept; };",
        "AS0",
        "accept",
    );
    let test_run = test_data("my-module", src_line);

    assert!(test_run.is_ok());

    let (ar, _rx, _tx) = test_run.unwrap();
    assert_eq!(ar, AcceptReject::Accept);
}

#[test]
fn test_module_50() {
    let src_line = &src_code(
        "filter match peer-asn-matches matching { set-asn; return reject; };",
        "AS65534",
        "reject",
    );
    let test_run = test_data("my-module", src_line);

    assert!(test_run.is_ok());

    let (ar, _rx, _tx) = test_run.unwrap();
    assert_eq!(ar, AcceptReject::Reject);
}

#[test]
fn test_module_51() {
    let src_line = &src_code(
        "filter match peer-asn-matches matching { set-asn; return reject; };",
        "AS0",
        "reject",
    );
    let test_run = test_data("my-module", src_line);

    assert!(test_run.is_ok());

    let (ar, _rx, _tx) = test_run.unwrap();
    assert_eq!(ar, AcceptReject::Reject);
}


#[test]
fn test_module_52() {
    let src_line = &src_code(
        "filter match peer-asn-matches matching { set-asn; return reject; };",
        "AS0",
        "accept",
    );
    let test_run = test_data("my-module", src_line);

    assert!(test_run.is_ok());

    let (ar, _rx, _tx) = test_run.unwrap();
    assert_eq!(ar, AcceptReject::Accept);
}

#[test]
fn test_module_60() {
    let src_line = &src_code(
        "filter match peer-asn-matches matching { set-asn; set-again-asn; };",
        "AS65534",
        "reject",
    );
    let test_run = test_data("my-module", src_line);

    assert!(test_run.is_ok());

    let (ar, _rx, _tx) = test_run.unwrap();
    assert_eq!(ar, AcceptReject::Reject);
}

#[test]
fn test_module_61() {
    let src_line = &src_code(
        "filter match peer-asn-matches matching { set-asn; set-again-asn; };",
        "AS0",
        "reject",
    );
    let test_run = test_data("my-module", src_line);

    assert!(test_run.is_ok());

    let (ar, _rx, _tx) = test_run.unwrap();
    assert_eq!(ar, AcceptReject::Reject);
}

#[test]
fn test_module_62() {
    let src_line = &src_code(
        "filter match peer-asn-matches matching { set-asn; set-again-asn; };",
        "AS0",
        "accept",
    );
    let test_run = test_data("my-module", src_line);

    assert!(test_run.is_ok());

    let (ar, _rx, _tx) = test_run.unwrap();
    assert_eq!(ar, AcceptReject::Accept);
}

#[test]
fn test_module_70() {
    let src_line = &src_code(
        r#"filter match peer-asn-matches matching { 
            set-asn; 
            set-again-asn; 
            return accept; 
        };"#,
        "AS65534",
        "reject",
    );
    let test_run = test_data("my-module", src_line);

    assert!(test_run.is_ok());

    let (ar, _rx, _tx) = test_run.unwrap();
    assert_eq!(ar, AcceptReject::Accept);
}

#[test]
fn test_module_71() {
    let src_line = &src_code(
        r#"filter match peer-asn-matches matching { 
            set-asn; 
            set-again-asn; 
            return accept; 
        };"#,
        "AS0",
        "reject",
    );
    let test_run = test_data("my-module", src_line);

    assert!(test_run.is_ok());

    let (ar, _rx, _tx) = test_run.unwrap();
    assert_eq!(ar, AcceptReject::Reject);
}

#[test]
fn test_module_72() {
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
    let test_run = test_data("my-module", src_line);

    trace!("{:?}", test_run);
    assert!(test_run.is_ok());

    let (ar, _rx, _tx) = test_run.unwrap();
    assert_eq!(ar, AcceptReject::Accept);
}

#[test]
fn test_module_80() {
    common::init();
    let src_line = &src_code(
        r#"filter match peer-asn-matches matching { 
        set-asn; 
        set-again-asn; 
        return reject; };"#,
        "AS65534",
        "reject",
    );
    let test_run = test_data("my-module", src_line);

    assert!(test_run.is_ok());

    let (ar, _rx, _tx) = test_run.unwrap();
    assert_eq!(ar, AcceptReject::Reject);
}

#[test]
fn test_module_81() {
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
    let test_run = test_data("my-module", src_line);

    println!("test run {:?}", test_run);
    assert!(test_run.is_ok());

    let (ar, _rx, _tx) = test_run.unwrap();
    assert_eq!(ar, AcceptReject::Reject);
}

#[test]
fn test_module_82() {
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
    let test_run = test_data("my-module", src_line);

    assert!(test_run.is_ok());

    let (ar, _rx, _tx) = test_run.unwrap();
    assert_eq!(ar, AcceptReject::Reject);
}
