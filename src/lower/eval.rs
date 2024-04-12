use std::collections::HashMap;

use log::trace;

use crate::{
    ast::BinOp,
    lower::ir::{Instruction, Value},
};

use super::ir::{Operand, Program, SafeValue, Var};

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
    p: &Program<Var, SafeValue>,
    filter_map: &str,
    rx: SafeValue,
) -> SafeValue {
    // Make the program easier to work with by collecting all instructions
    // and constructing a map from labels to indices.
    let mut block_map = HashMap::new();
    let mut instructions = Vec::new();

    for block in &p.blocks {
        block_map.insert(block.label.clone(), instructions.len());
        instructions.extend(block.instructions.clone());
    }

    // This is our working memory for the interpreter
    let mut mem = HashMap::new();

    // Insert the rx value
    mem.insert(
        Var {
            var: format!("{filter_map}-rx"),
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
                let x = match val {
                    SafeValue::Bool(b) => *b as usize,
                    SafeValue::U8(x) => *x as usize,
                    SafeValue::U16(x) => *x as usize,
                    SafeValue::U32(x) => *x as usize,
                    SafeValue::Unit | SafeValue::Record(_) => panic!(),
                };

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
            Instruction::Access { to, record, field } => {
                let record = &mem[record];
                let SafeValue::Record(record) = record else {
                    panic!("Should have been caught in typechecking")
                };
                let (_, val) =
                    record.iter().find(|(s, _)| s == field).unwrap();
                mem.insert(to.clone(), val.clone());
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
        Operand::Place(p) => &mem[dbg!(p)],
        Operand::Value(v) => v,
    }
}
