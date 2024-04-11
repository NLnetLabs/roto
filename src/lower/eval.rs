use std::collections::HashMap;

use log::trace;

use crate::{
    ast::BinOp,
    lower::ir::{Instruction, Value},
};

use super::ir::{Operand, Program, SafeValue, Var};

pub fn eval(
    p: &Program<Var, SafeValue>,
    filter_map: &str,
    rx: SafeValue,
) -> SafeValue {
    let mut block_map = HashMap::new();

    for (i, block) in p.blocks.iter().enumerate() {
        block_map.insert(block.label.clone(), i);
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
    let mut current_block = filter_map;
    let mut start_instruction = 0;

    'outer: loop {
        trace!("Jump: '{current_block}'");
        let idx = block_map[current_block];
        for (i, instruction) in p.blocks[idx].instructions[start_instruction..].iter().enumerate()
        {
            trace!("Inst: {instruction}");
            match instruction {
                Instruction::Jump(b) => {
                    current_block = b;
                    start_instruction = 0;
                    continue 'outer;
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
                        SafeValue::Unit => panic!(),
                    };

                    for (i, branch) in branches {
                        if *i == x {
                            current_block = branch;
                            start_instruction = 0;
                            continue 'outer;
                        }
                    }

                    current_block = default;
                    start_instruction = 0;
                    continue 'outer;
                }
                Instruction::Assign { to, val } => {
                    let val = eval_operand(&mem, val);
                    mem.insert(to.clone(), *val);
                }
                Instruction::Call(b) => {
                    stack.push((current_block, i + start_instruction));
                    current_block = b;
                    start_instruction = 0;
                    continue 'outer;
                }
                // TODO: This is obviously not correct
                Instruction::Return => {
                    if let Some((b, i)) = stack.pop() {
                        current_block = b;
                        start_instruction = i + 1;
                        continue 'outer;
                    } else {
                        break 'outer;
                    }
                }
                Instruction::Exit => break 'outer,
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
            }
        }

        panic!("Should have terminated each block");
    }

    mem[&Var {
        var: format!("{filter_map}-return"),
    }]
}

fn eval_operand<'a>(
    mem: &'a HashMap<Var, SafeValue>,
    op: &'a Operand<Var, SafeValue>,
) -> &'a SafeValue {
    match op {
        Operand::Place(p) => &mem[p],
        Operand::Value(v) => v,
    }
}
