//! Evaluate HIR programs

use std::collections::HashMap;

use log::trace;

use crate::{
    ast::BinOp,
    lower::{ir::Instruction, value::{Value, Verdict}},
};

use super::{
    ir::{Operand, Function, Var},
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
    p: &[Function<Var, SafeValue>],
    filter_map: &str,
    rx: SafeValue,
) -> SafeValue {
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
    mem.insert(
        Var {
            var: "$arg_0".into(),
        },
        rx,
    );

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
            Instruction::Assign { to, val } => {
                let val = eval_operand(&mem, val);
                mem.insert(to.clone(), val.clone());
            }
            Instruction::Call(to, b, args) => {
                stack.push(StackFrame {
                    return_address: program_counter,
                    return_place: to.clone(),
                });
                for (name, arg) in args {
                    let val = eval_operand(&mem, arg);
                    mem.insert(
                        Var {
                            var: format!("{b}::{name}"),
                        },
                        val.clone(),
                    );
                }
                program_counter = block_map[b];
                continue;
            }
            Instruction::CallExternal(to, func, args) => {
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
            Instruction::BinOp {
                to,
                op,
                left,
                right,
            } => {
                let left = eval_operand(&mem, left);
                let right = eval_operand(&mem, right);
                let res = match op {
                    BinOp::And => left.as_bool() && right.as_bool(),
                    BinOp::Or => left.as_bool() || right.as_bool(),
                    BinOp::Eq => left == right,
                    BinOp::Ne => left != right,
                    BinOp::Lt => left.as_u32() < right.as_u32(),
                    BinOp::Le => left.as_u32() <= right.as_u32(),
                    BinOp::Gt => left.as_u32() > right.as_u32(),
                    BinOp::Ge => left.as_u32() >= right.as_u32(),
                    BinOp::In => todo!(),
                    BinOp::NotIn => todo!(),
                };
                mem.insert(to.clone(), SafeValue::Bool(res));
            }
            Instruction::AccessRecord { to, record, field } => {
                let record = eval_operand(&mem, record);
                let SafeValue::Record(record) = record else {
                    panic!("Should have been caught in typechecking")
                };
                let (_, val) =
                    record.iter().find(|(s, _)| s == field).unwrap();
                mem.insert(to.clone(), val.clone());
            }
            Instruction::CreateRecord { to, fields } => {
                let fields = fields
                    .iter()
                    .map(|(s, op)| (s.into(), eval_operand(&mem, op).clone()))
                    .collect();
                mem.insert(to.clone(), SafeValue::Record(fields));
            }
            Instruction::CreateEnum { to, variant, data } => {
                let val = eval_operand(&mem, data);
                mem.insert(
                    to.clone(),
                    SafeValue::Enum(*variant, Box::new(val.clone())),
                );
            }
            Instruction::AccessEnum { to, from } => {
                let val = eval_operand(&mem, from);
                let SafeValue::Enum(_, data) = val else {
                    panic!("Should have been caught in typechecking")
                };
                mem.insert(to.clone(), data.as_ref().clone());
            }
        }

        program_counter += 1;
    }
}

fn eval_operand<'a>(
    mem: &'a HashMap<Var, SafeValue>,
    op: &'a Operand<Var, SafeValue>,
) -> &'a SafeValue {
    match op {
        Operand::Place(p) => mem
            .get(p)
            .unwrap_or_else(|| panic!("No value was found for place {p}")),
        Operand::Value(v) => v,
    }
}
