use crate::parser::Parser;

use super::{typed, TypeChecker, TypeResult};

fn typecheck(s: &str) -> TypeResult<typed::SyntaxTree> {
    let tree = Parser::parse(s).unwrap();
    let res = TypeChecker::new().check(tree);
    if let Err(e) = &res {
        eprintln!("{e}");
    }
    res
}

#[test]
fn one_record() {
    let src = "type Foo { a: u32 }";
    assert!(typecheck(src).is_ok());
}

#[test]
fn declared_multiple_times() {
    let src = "
        type Foo { a: u32 }
        type Foo { a: u32 }
    ";
    assert!(typecheck(src).is_err());
}

#[test]
fn undeclared_type_in_record() {
    let src = "type Bar { f: Foo }";
    assert!(typecheck(src).is_err());
}

#[test]
fn nested_record() {
    let src = "
        type Foo { a: { b: u32, c: u32 } }
    ";
    assert!(typecheck(src).is_ok());
}

#[test]
fn two_records() {
    let src = "
        type Foo { a: u32 }
        type Bar { f: Foo }
    ";
    assert!(typecheck(src).is_ok());

    let src = "
        type Bar { f: Foo }
        type Foo { a: u32 }
    ";
    assert!(typecheck(src).is_ok());
}

#[test]
fn record_cycle() {
    let src = "
        type Foo { f: Foo }
    ";
    assert!(typecheck(src).is_err());

    let src = "
        type Bar { f: Foo }
        type Foo { b: Bar }
    ";
    assert!(typecheck(src).is_err());

    let src = "
        type A { x: B }
        type B { x: C }
        type C { x: D }
        type D { x: A }
    ";
    assert!(typecheck(src).is_err());
    
    let src = "
        type A { x: B }
        type B { x: C }
        type C { x: { y: D } }
        type D { x: B }
    ";
    assert!(typecheck(src).is_err());
}

#[test]
fn table_contains_record() {
    let src = "
        table t contains A { b: B }
        type B { x: u32 }
    ";
    assert!(typecheck(src).is_ok());
}

#[test]
fn output_stream_contains_record() {
    let src = "
        output-stream o contains A { b: B }
        type B { x: u32 }
    ";
    assert!(typecheck(src).is_ok());
}

#[test]
fn rib_contains_record() {
    let src = "
        rib r contains A { b: B }
        type B { x: u32 }
    ";
    assert!(typecheck(src).is_ok());
}
