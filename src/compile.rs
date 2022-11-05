use std::sync::Arc;

use crate::{ast::{AcceptReject, ShortString}, symbols::SymbolTable, types::typevalue::TypeValue};

struct VecPayload(Vec<(ShortString, TypeValue)>);

pub trait Payload {
    fn set(&mut self, field: ShortString, value: TypeValue);
    fn get(&self, field: ShortString) -> Option<&Vec<u8>>;
}

impl VecPayload {
    fn set(&mut self, field: ShortString, data: TypeValue) {
        let field = &mut self.0.iter().find(|k| k.0 == field);
        let key = field.unwrap().0.clone();
        let _old = std::mem::replace(field, Some(&(key, data)));
    }

    fn get(&self, field: ShortString) -> Option<&TypeValue>{
        self.0.iter().find(|k| k.0 == field).map(|kv| &kv.1)
    }
}

trait ExtSource {
    fn get(&self, key: &str) -> Option<TypeValue>;
}

#[derive(Clone)]
pub struct ExtSources(Vec<Arc<dyn ExtSource>>);

pub fn compile_filter<'a, I: Payload, O: Payload>(
    symbols: &mut SymbolTable,
    available_sources: ExtSources,
) -> impl Fn(&mut I, &mut O) -> Result<AcceptReject, Box<dyn std::error::Error>> + 'a
{
    let _used_source = available_sources.0[0].clone();
    
    move |input: &mut I, output: &mut O| -> Result<AcceptReject, Box<dyn std::error::Error>> {
        input.set("bla".into(), TypeValue::from_literal("blub").unwrap());
        output.set("bla".into(), TypeValue::from_literal("blub").unwrap());
        Ok(AcceptReject::Accept)
    }
}