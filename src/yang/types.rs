use std::collections::HashMap;

// use indexmap::IndexMap;
use serde::{Deserialize, Serialize};

use crate::typechecker::types::Primitive;

// use crate::{
// dyn_tree::{file_tree::FileTree, module::Parsed},
// types::YangNodeType,
// yang, yang_internal,
// };

#[derive(Debug, Serialize, Deserialize)]
pub(crate) struct YangStatement {
    id: YangNodeId,
    name: String,
}

#[derive(Serialize, Deserialize)]
pub(crate) struct YangValue(Option<String>);

#[derive(Copy, Clone, Serialize, Deserialize, Hash, PartialEq, Eq, Debug)]
pub(crate) struct YangNodeId(pub(crate) u16);

#[derive(Debug)]
pub(crate) struct YangNode {
    // name: String,
    ty: YangNodeType,
    sub_statements: Vec<YangStatement>,
    child_nodes: HashMap<YangNodeId, YangNode>,
}

#[derive(Debug, Serialize, Deserialize, Hash, PartialEq, Eq)]
pub struct YangNameSpace(Vec<String>);

#[derive(Debug, PartialEq)]
pub enum YangNodeType {
    Tree,
    Value,
    Attr(YangArgType),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum YangArgType {
    Ident,
    String,
    // XSD-TYPES regex
    // https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#regex
    RegexString,
    // A prefix:ident
    Path,
    XPath,
    // Actually a String that can be quoted or unquoted "true", "false"
    Bool,
    Int,
    PosInt,
    // A positive integer or the string "unbounded"
    UnboundedPosInt,
    // a string that specifies a space-separated list of one or more leaf
    // identifiers of this list
    SpaceSeparatedIdents,
    Decimal64,
    // RFC7950 9.4.4
    LengthExpr,
    // RFC7950 9.2.4
    RangeExpr,
    None,
}

impl std::fmt::Display for YangArgType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            YangArgType::Ident => write!(f, "identifier"),
            YangArgType::String => write!(f, "string"),
            YangArgType::RegexString => {
                write!(f, "regular expression string")
            }
            YangArgType::Path => write!(f, "prefix and identifier"),
            YangArgType::XPath => write!(f, "xpath"),
            YangArgType::Bool => write!(f, "boolean"),
            YangArgType::Int => write!(f, "integer"),
            YangArgType::PosInt => write!(f, "positive integer"),
            YangArgType::UnboundedPosInt => {
                write!(f, "unbounded positive integer")
            }
            YangArgType::SpaceSeparatedIdents => {
                write!(f, "space separated identifiers")
            }
            YangArgType::Decimal64 => write!(f, "64-bits decimal"),
            YangArgType::LengthExpr => write!(f, "length expression"),
            YangArgType::RangeExpr => write!(f, "range expression"),
            YangArgType::None => write!(f, "none"),
        }
    }
}

// #[derive(Debug, Serialize, Deserialize)]
// pub struct YangTypeCollection(
//     HashMap<YangNameSpace, IndexMap<String, YangNode>>,
// );

// impl YangTypeCollection {
//     fn new() -> Self {
//         let mut root = Self(HashMap::new());
//         root.builtin_types();

//         root
//     }

//     // +---------------------+-------------------------------------+
//     // | Name                | Description                         |
//     // +---------------------+-------------------------------------+
//     // | binary              | Any binary data                     |
//     // | bits                | A set of bits or flags              |
//     // | boolean             | "true" or "false"                   |
//     // | decimal64           | 64-bit signed decimal number        |
//     // | empty               | A leaf that does not have any value |
//     // | enumeration         | One of an enumerated set of strings |
//     // | identityref         | A reference to an abstract identity |
//     // | instance-identifier | A reference to a data tree node     |
//     // | int8                | 8-bit signed integer                |
//     // | int16               | 16-bit signed integer               |
//     // | int32               | 32-bit signed integer               |
//     // | int64               | 64-bit signed integer               |
//     // | leafref             | A reference to a leaf instance      |
//     // | string              | A character string                  |
//     // | uint8               | 8-bit unsigned integer              |
//     // | uint16              | 16-bit unsigned integer             |
//     // | uint32              | 32-bit unsigned integer             |
//     // | uint64              | 64-bit unsigned integer             |
//     // | union               | Choice of member types              |
//     // +---------------------+-------------------------------------+

//     fn builtin_types(&mut self) {
//         let mut root_map = IndexMap::new();

//         root_map.insert(
//             "binary".to_string(),
//             YangNode {
//                 ty: YangNodeType::Builtin,
//                 sub_statements: vec![],
//                 child_nodes: HashMap::new(),
//             },
//         );

//         root_map.insert(
//             "bits".to_string(),
//             YangNode {
//                 ty: YangNodeType::Builtin,
//                 sub_statements: vec![],
//                 child_nodes: HashMap::new(),
//             },
//         );

//         root_map.insert(
//             "boolean".to_string(),
//             YangNode {
//                 ty: YangNodeType::Builtin,
//                 sub_statements: vec![],
//                 child_nodes: HashMap::new(),
//             },
//         );
//         root_map.insert(
//             "decimal64".to_string(),
//             YangNode {
//                 ty: YangNodeType::Builtin,
//                 sub_statements: vec![],
//                 child_nodes: HashMap::new(),
//             },
//         );
//         root_map.insert(
//             "empty".to_string(),
//             YangNode {
//                 ty: YangNodeType::Builtin,
//                 sub_statements: vec![],
//                 child_nodes: HashMap::new(),
//             },
//         );
//         root_map.insert(
//             "enumeration".to_string(),
//             YangNode {
//                 ty: YangNodeType::Builtin,
//                 sub_statements: vec![],
//                 child_nodes: HashMap::new(),
//             },
//         );
//         root_map.insert(
//             "identityref".to_string(),
//             YangNode {
//                 ty: YangNodeType::Builtin,
//                 sub_statements: vec![],
//                 child_nodes: HashMap::new(),
//             },
//         );
//         root_map.insert(
//             "int8".to_string(),
//             YangNode {
//                 ty: YangNodeType::Builtin,
//                 sub_statements: vec![],
//                 child_nodes: HashMap::new(),
//             },
//         );
//         root_map.insert(
//             "int16".to_string(),
//             YangNode {
//                 ty: YangNodeType::Builtin,
//                 sub_statements: vec![],
//                 child_nodes: HashMap::new(),
//             },
//         );
//         root_map.insert(
//             "int32".to_string(),
//             YangNode {
//                 ty: YangNodeType::Builtin,
//                 sub_statements: vec![],
//                 child_nodes: HashMap::new(),
//             },
//         );
//         root_map.insert(
//             "int64".to_string(),
//             YangNode {
//                 ty: YangNodeType::Builtin,
//                 sub_statements: vec![],
//                 child_nodes: HashMap::new(),
//             },
//         );
//         root_map.insert(
//             "leafref".to_string(),
//             YangNode {
//                 ty: YangNodeType::Builtin,
//                 sub_statements: vec![],
//                 child_nodes: HashMap::new(),
//             },
//         );
//         root_map.insert(
//             "string".to_string(),
//             YangNode {
//                 ty: YangNodeType::Builtin,
//                 sub_statements: vec![],
//                 child_nodes: HashMap::new(),
//             },
//         );
//         root_map.insert(
//             "uint8".to_string(),
//             YangNode {
//                 ty: YangNodeType::Builtin,
//                 sub_statements: vec![],
//                 child_nodes: HashMap::new(),
//             },
//         );
//         root_map.insert(
//             "uint32".to_string(),
//             YangNode {
//                 ty: YangNodeType::Builtin,
//                 sub_statements: vec![],
//                 child_nodes: HashMap::new(),
//             },
//         );
//         root_map.insert(
//             "uint64".to_string(),
//             YangNode {
//                 ty: YangNodeType::Builtin,
//                 sub_statements: vec![],
//                 child_nodes: HashMap::new(),
//             },
//         );
//         root_map.insert(
//             "union".to_string(),
//             YangNode {
//                 ty: YangNodeType::Builtin,
//                 sub_statements: vec![],
//                 child_nodes: HashMap::new(),
//             },
//         );

//         self.0.insert(YangNameSpace(vec![]), root_map);
//     }

//     fn rotonda_module(&mut self) {
//         let mut root_map = IndexMap::new();
//         root_map.insert(
//             "logical-network-elements".to_string(),
//             YangNode {
//                 ty: YangNodeType::List("logical-network-element".to_string()),
//                 sub_statements: vec![],
//                 child_nodes: HashMap::new(),
//             },
//         );

//         self.0.insert(
//             YangNameSpace(vec![
//                 "urn".to_string(),
//                 "nlnetlabs.nl".to_string(),
//                 "rotonda".to_string(),
//             ]),
//             root_map,
//         );
//     }
// }

// macro_rules! src {
//     ($code:expr) => {
//         $crate::dyn_tree::file_tree::FileTree::test_file(
//             file!(),
//             $code,
//             line!() as usize - 1,
//         )
//     };
// }
// #[test]
// fn test_builtin() {
//     let mut root = YangTypeCollection::new();
//     root.rotonda_module();

//     println!("{:#?}", root);
// }

// #[test]
// fn test_yang_macro() {
//     let tree = src!(
//         r#"
//         typedef rr-cluster-id-type {
//             type union {
//               type uint32;
//               type inet:ipv4-address;
//             }
//             description
//               "Union type for route reflector cluster ids:
//                option 1: 4-byte number
//                option 2: IP address";
//         }"#
//     );

//     let parsed = tree.parse().unwrap();

//     println!("{:?}", parsed.spans);
//     println!("{:#?}", parsed.module_tree.modules[0].ast);
// }

// #[test]
// fn test_yang_2() {
//     let tree = src!(
//         r#"
//         grouping state {
//             description
//               "Grouping containing common counters relating to prefixes and
//                paths";

//             container statistics {
//               config false;
//               description
//                 "Global level statistics.";

//               leaf total-paths {
//                 type yang:gauge32;
//                 description
//                   "Total number of BGP paths (BGP routes) within the
//                    context";
//               }

//               leaf total-prefixes {
//                 type yang:gauge32;
//                 description
//                   "Total number of BGP prefixes (destinations) received
//                    within the context";
//               }
//             }
//         }"#
//     );

//     let parsed = tree.parse().unwrap();

//     println!("{:?}", parsed.spans);
//     println!("{:#?}", parsed.module_tree.modules[0].ast);
// }

// #[test]
// fn test_yang_3() {
//     // from a RFC draft. It is actually wrong, description is not allowed as a
//     // substatement for type
//     let tree = src!(
//         r#"
//         typedef ipv4-multicast-group-address {
//             type inet:ipv4-address {
//               pattern '(2((2[4-9])|(3[0-9]))\.).*';

//               description
//                 "This type represents an IPv4 multicast group address,
//                  which is in the range from 224.0.0.0 to 239.255.255.255.";
//                  reference "RFC1112: Host Extensions for IP Multicasting.";
//             }

//             typedef ipv6-multicast-group-address {
//                 mandatory true;
//                 type inet:ipv6-address {
//                 pattern
//                     '(([fF]{2}[0-9a-fA-F]{2}):).*';
//             }
//             description
//                 "This type represents an IPv6 multicast group address,
//                  which is in the range of FF00::/8.";
//             reference
//                 "RFC4291: IP Version 6 Addressing Architecture. Sec 2.7.
//                  RFC7346: IPv6 Multicast Address Scopes.";
//             }
//         }"#
//     );

//     let parsed = tree.parse();

//     assert!(parsed.is_err());
// }

// #[test]
// fn test_yang_4() {
//     // This is the corrected version from the RFC8294
//     let tree = src!(
//         r#"
//           typedef ipv4-multicast-group-address {
//             type inet:ipv4-address {
//               pattern '(2((2[4-9])|(3[0-9]))\.).*';
//             }
//             description
//               "This type represents an IPv4 multicast group address,
//                which is in the range from 224.0.0.0 to 239.255.255.255.";
//             reference "RFC1112: Host Extensions for IP Multicasting.";
//           }
//         "#
//     );

//     let parsed = tree.parse().unwrap();

//     println!("{:?}", parsed.spans);
//     println!("{:#?}", parsed.module_tree.modules[0].ast);
// }
