//! Evaluate HIR programs

use std::collections::HashMap;

use log::trace;

use crate::lower::{
    ir::{Instruction, IntCmp},
    value::{Value, Verdict},
};

use super::{
    ir::{Function, Operand, Var},
    value::SafeValue,
};

struct StackFrame {
    return_address: usize,
    return_place: Var,
}

/// Evaluate HIR
///
/// This is mostly used for testing purposes, since this evaluation is
/// fairly slow. This is because all variables at this point are identified
/// by strings and therefore stored as a hashmap.
pub fn eval(
    p: &[Function],
    filter_map: &str,
    rx: Vec<SafeValue>,
) -> SafeValue {
    let f = p
        .iter()
        .find(|f| f.name == "main")
        .expect("Need a main function!");
    let parameters = f.parameters.clone();

    // Make the program easier to work with by collecting all instructions
    // and constructing a map from labels to indices.
    let mut block_map = HashMap::new();
    let mut instructions = Vec::new();

    for block in p.iter().flat_map(|f| &f.blocks) {
        block_map.insert(block.label.clone(), instructions.len());
        instructions.extend(block.instructions.clone());
    }

    // This is our working memory for the interpreter
    let mut mem = HashMap::new();

    // Insert the rx value
    assert_eq!(parameters.len(), rx.len());
    for ((x, _), v) in parameters.iter().zip(rx) {
        mem.insert(Var { var: x.into() }, v);
    }

    let mut stack = Vec::new();
    let mut program_counter = block_map[filter_map];

    loop {
        let instruction = &instructions[program_counter];
        trace!("Inst: {instruction}");
        match instruction {
            Instruction::Jump(b) => {
                program_counter = block_map[b];
                continue;
            }
            Instruction::Switch {
                examinee,
                branches,
                default,
            } => {
                let val = eval_operand(&mem, examinee);
                let x = val.switch_on() as usize;

                let label = branches
                    .iter()
                    .find_map(|(i, branch)| (*i == x).then_some(branch))
                    .unwrap_or(default);
                program_counter = block_map[label];
                continue;
            }
            Instruction::Assign { to, val, .. } => {
                let val = eval_operand(&mem, val);
                mem.insert(to.clone(), val.clone());
            }
            Instruction::Call {
                to,
                ty: _,
                func,
                args,
            } => {
                stack.push(StackFrame {
                    return_address: program_counter,
                    return_place: to.clone(),
                });
                for (name, arg) in args {
                    let val = eval_operand(&mem, arg);
                    mem.insert(
                        Var {
                            var: format!("{func}::{name}"),
                        },
                        val.clone(),
                    );
                }
                program_counter = block_map[func];
                continue;
            }
            Instruction::CallExternal {
                to,
                ty: _,
                func,
                args,
            } => {
                let args: Vec<_> = args
                    .iter()
                    .map(|a| eval_operand(&mem, a).clone())
                    .collect();
                let val = func.call(args);
                mem.insert(to.clone(), val);
            }
            Instruction::Return(ret) => {
                let val = eval_operand(&mem, ret);
                if let Some(StackFrame {
                    return_address,
                    return_place,
                }) = stack.pop()
                {
                    mem.insert(return_place, val.clone());
                    program_counter = return_address + 1;
                    continue;
                } else {
                    return val.clone();
                }
            }
            Instruction::Exit(b, ret) => {
                let val = eval_operand(&mem, ret);
                let verdict = if *b {
                    Verdict::Accept(val.clone())
                } else {
                    Verdict::Reject(val.clone())
                };
                return SafeValue::Verdict(Box::new(verdict));
            }
            Instruction::Cmp {
                to,
                cmp,
                left,
                right,
            } => {
                let left = eval_operand(&mem, left);
                let right = eval_operand(&mem, right);
                let res = match cmp {
                    IntCmp::Eq => left == right,
                    IntCmp::Ne => left != right,
                    IntCmp::ULt => left.as_u32() < right.as_u32(),
                    IntCmp::ULe => left.as_u32() <= right.as_u32(),
                    IntCmp::UGt => left.as_u32() > right.as_u32(),
                    IntCmp::UGe => left.as_u32() >= right.as_u32(),
                    IntCmp::SLt => left.as_i32() < right.as_i32(),
                    IntCmp::SLe => left.as_i32() <= right.as_i32(),
                    IntCmp::SGt => left.as_i32() > right.as_i32(),
                    IntCmp::SGe => left.as_i32() >= right.as_i32(),
                };
                mem.insert(to.clone(), SafeValue::Bool(res));
            }
            Instruction::Eq { to, left, right } => {
                let left = eval_operand(&mem, left);
                let right = eval_operand(&mem, right);
                mem.insert(to.clone(), SafeValue::Bool(left.eq(right)));
            }
            Instruction::Not { to, val } => {
                let val = eval_operand(&mem, val).as_bool();
                mem.insert(to.clone(), SafeValue::Bool(val));
            }
            Instruction::And { to, left, right } => {
                let left = eval_operand(&mem, left).as_bool();
                let right = eval_operand(&mem, right).as_bool();
                mem.insert(to.clone(), SafeValue::Bool(left && right));
            }
            Instruction::Or { to, left, right } => {
                let left = eval_operand(&mem, left).as_bool();
                let right = eval_operand(&mem, right).as_bool();
                mem.insert(to.clone(), SafeValue::Bool(left || right));
            }
            Instruction::AccessRecord {
                to, record, field, ..
            } => {
                let record = eval_operand(&mem, record);
                let SafeValue::Record(record) = record else {
                    panic!("Should have been caught in typechecking")
                };
                let (_, val) =
                    record.iter().find(|(s, _)| s == field).unwrap();
                mem.insert(to.clone(), val.clone());
            }
            Instruction::CreateRecord { to, fields, .. } => {
                let fields = fields
                    .iter()
                    .map(|(s, op)| (s.into(), eval_operand(&mem, op).clone()))
                    .collect();
                mem.insert(to.clone(), SafeValue::Record(fields));
            }
            Instruction::CreateEnum {
                to, variant, data, ..
            } => {
                let val = data
                    .as_ref()
                    .map(|d| Box::new(eval_operand(&mem, d).clone()));
                mem.insert(to.clone(), SafeValue::Enum(*variant, val));
            }
            Instruction::AccessEnum { to, from, .. } => {
                let val = eval_operand(&mem, from);
                let SafeValue::Enum(_, Some(data)) = val else {
                    panic!("Should have been caught in typechecking")
                };
                mem.insert(to.clone(), data.as_ref().clone());
            }
            Instruction::EnumDiscriminant { to, from } => {
                let val = eval_operand(&mem, from);
                let SafeValue::Enum(discriminant, _) = val else {
                    panic!("Should have been caught in typechecking")
                };
                mem.insert(to.clone(), SafeValue::from(*discriminant));
            }
        }

        program_counter += 1;
    }
}

fn eval_operand<'a>(
    mem: &'a HashMap<Var, SafeValue>,
    op: &'a Operand,
) -> &'a SafeValue {
    match op {
        Operand::Place(p) => mem
            .get(p)
            .unwrap_or_else(|| panic!("No value was found for place {p}")),
        Operand::Value(v) => v,
    }
}
