//! Dead code elimination on the MIR

use crate::{ice, label::LabelRef};

use super::{Block, Function, Instruction, Mir};

#[derive(Default)]
struct State(Vec<LabelRef>);

impl State {
    fn new() -> Self {
        Self::default()
    }

    fn add(&mut self, lbl: LabelRef) {
        match self.0.iter().find(|l| **l == lbl) {
            Some(_) => {} // do nothing
            None => self.0.push(lbl),
        }
    }

    fn contains(&self, lbl: &LabelRef) -> bool {
        self.0.contains(lbl)
    }

    fn get(&self, i: usize) -> LabelRef {
        self.0[i]
    }

    fn len(&self) -> usize {
        self.0.len()
    }
}

impl Mir {
    pub fn eliminate_dead_code(&mut self) {
        for function in &mut self.functions {
            process_function(function)
        }
    }
}

fn process_function(function: &mut Function) {
    let mut state = State::new();

    state.add(function.blocks[0].label);

    // We can't turn this into a for loop because the length of the state
    // increases over time.
    let mut i = 0;
    while i < state.len() {
        let lbl = state.get(i);
        let block =
            function.blocks.iter_mut().find(|b| b.label == lbl).unwrap();
        process_block(&mut state, block);
        i += 1;
    }

    function.blocks.retain(|b| state.contains(&b.label));
}

/// Truncate each block to its reachable instructions and add reachable blocks
/// to the state.
fn process_block(state: &mut State, block: &mut Block) {
    let mut final_instruction = None;
    for (i, inst) in block.instructions.iter().enumerate() {
        match inst {
            Instruction::Jump(lbl) => {
                // We jump from a reachable block to another block
                // so that block is now reachable.
                state.add(*lbl);
            }
            Instruction::Switch {
                examinee: _,
                branches,
                default,
            } => {
                // We consider each of the branches reachable
                for (_, lbl) in branches {
                    state.add(*lbl);
                }
                if let Some(default) = default {
                    state.add(*default);
                }
            }
            Instruction::Return { var: _ } => {}
            _ => continue,
        }

        final_instruction = Some(i);
        break;
    }

    if let Some(i) = final_instruction {
        block.instructions.truncate(i + 1);
    } else {
        ice!("Malformed MIR: block ends without a terminating instruction")
    }
}
