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

    let mut current_block = filter_map;
    'outer: loop {
        trace!("Jump: '{current_block}'");
        let idx = block_map[current_block];
        for instruction in &p.blocks[idx].instructions {
            trace!("Inst: {instruction}");
            match instruction {
                Instruction::Jump(b) => {
                    current_block = b;
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
                            continue 'outer;
                        }
                    }

                    current_block = default;
                    continue 'outer;
                }
                Instruction::Assign { to, val } => {
                    let val = eval_operand(&mem, val);
                    mem.insert(to.clone(), val.clone());
                }
                // TODO: This is obviously not correct
                Instruction::Return => break 'outer,
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
                        BinOp::Lt => todo!(),
                        BinOp::Le => todo!(),
                        BinOp::Gt => todo!(),
                        BinOp::Ge => todo!(),
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
