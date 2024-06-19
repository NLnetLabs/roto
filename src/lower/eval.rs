//! Evaluate HIR programs
//!
//! This is mostly used for testing purposes, since this evaluation is
//! fairly slow. This is because all variables at this point are identified
//! by strings and therefore stored as a hashmap.

use super::ir::{Function, Operand, Var};
use crate::lower::{
    ir::{Instruction, IntCmp},
    value::IrValue,
};
use log::trace;
use std::collections::HashMap;

/// Memory for the IR evaluation
///
/// This matches the
///
/// The IR evaluation is meant to be close to the native execution, but with
/// additional safety guarantees, not just to check programs, but mostly to
/// check correctness of the compiler. To achieve this, we store more
/// metadata for each allocation.
///
/// A pointer consists of:
///
///  - a stack frame index,
///  - a stack frame id,
///  - an allocation index, and
///  - an offset
///
/// When we read or write from a pointer, we do the following steps:
///
///  1. Get the stack frame at the given index.
///  2. Check whether the id matches.
///  3. Read the allocation at the allocation index.
///  4. Check that the access is in bounds (i.e. `offset + size < len`).
///  5. Check that the access is aligned (i.e. `offset % size == 0`)
///
/// This guarantees most of the properties that we would like to check:
///
///  - No use after free.
///  - No unaligned accesses.
///  - No out of bounds accesses.
///
/// Of course these are only checked at runtime, not statically enforced.
/// But it provides a good mechanism for testing the compiler.
#[derive(Debug)]
pub struct Memory {
    id_counter: usize,
    stack: Vec<StackFrame>,
    pointers: Vec<Pointer>,
}

#[derive(Clone, Debug)]
pub struct Pointer {
    stack_index: usize,
    stack_id: usize,
    allocation_index: usize,
    allocation_offset: usize,
}

#[derive(Debug)]
struct StackFrame {
    id: usize,
    return_address: usize,
    return_place: Option<Var>,
    allocations: Vec<Allocation>,
}

#[derive(Debug)]
struct Allocation {
    inner: Vec<u8>,
}

impl Pointer {
    fn offset_by(&self, offset: usize) -> Self {
        Self {
            allocation_offset: self.allocation_offset + offset,
            ..self.clone()
        }
    }
}

impl Default for Memory {
    fn default() -> Self {
        Self {
            id_counter: 1,
            pointers: Vec::new(),
            stack: vec![StackFrame {
                id: 0,
                return_address: 0,
                return_place: None,
                allocations: Vec::new(),
            }],
        }
    }
}

impl Memory {
    pub fn new() -> Self {
        Self::default()
    }

    fn copy(&mut self, to: usize, from: usize, size: usize) {
        let data: Vec<_> = self.read(from, size).into();
        self.write(to, &data);
    }

    pub fn write(&mut self, p: usize, val: &[u8]) {
        let p = &self.pointers[p];
        let frame = &mut self.stack[p.stack_index];
        assert_eq!(frame.id, p.stack_id);
        frame.write(p, val)
    }

    pub fn read(&self, p: usize, size: usize) -> &[u8] {
        let p = &self.pointers[p];
        let frame = &self.stack[p.stack_index];
        assert_eq!(frame.id, p.stack_id);
        frame.read(p, size)
    }

    fn push_frame(
        &mut self,
        return_address: usize,
        return_place: Option<Var>,
    ) {
        let id = self.id_counter;
        self.id_counter += 1;
        self.stack.push(StackFrame {
            id,
            return_address,
            return_place,
            allocations: Vec::new(),
        });
    }

    fn pop_frame(&mut self) -> Option<StackFrame> {
        // Keep the root, because it's values provided by the runtime
        if self.stack.len() == 1 {
            return None;
        }
        self.stack.pop()
    }

    fn offset_by(&mut self, p: usize, offset: usize) -> usize {
        let p = &self.pointers[p];
        self.pointers.push(p.offset_by(offset));
        self.pointers.len() - 1
    }

    pub fn allocate(&mut self, bytes: usize) -> usize {
        let stack_index = self.stack.len() - 1;
        let frame = &mut self.stack[stack_index];
        let stack_id = frame.id;
        let allocation_index = frame.allocations.len();
        frame.allocations.push(Allocation {
            inner: vec![0; bytes],
        });
        self.pointers.push(Pointer {
            stack_index,
            stack_id,
            allocation_index,
            allocation_offset: 0,
        });
        self.pointers.len() - 1
    }
}

impl StackFrame {
    fn write(&mut self, p: &Pointer, val: &[u8]) {
        let alloc = &mut self.allocations[p.allocation_index];
        alloc.write(p.allocation_offset, val);
    }

    fn read(&self, p: &Pointer, size: usize) -> &[u8] {
        let alloc = &self.allocations[p.allocation_index];
        alloc.read(p.allocation_offset, size)
    }
}

impl Allocation {
    fn write(&mut self, offset: usize, val: &[u8]) {
        assert!(
            offset + val.len() <= self.inner.len(),
            "memory access out of bounds"
        );
        assert!(offset % val.len() == 0, "memory access is unaligned");

        self.inner[offset..offset + val.len()].copy_from_slice(val);
    }

    fn read(&self, offset: usize, size: usize) -> &[u8] {
        assert!(
            offset + size <= self.inner.len(),
            "memory access out of bounds"
        );
        assert!(offset % size == 0, "memory access is unaligned");

        &self.inner[offset..offset + size]
    }
}

/// Evaluate HIR
///
/// This is mostly used for testing purposes, since this evaluation is
/// fairly slow. This is because all variables at this point are identified
/// by strings and therefore stored as a hashmap.
pub fn eval(
    p: &[Function],
    filter_map: &str,
    mem: &mut Memory,
    rx: Vec<IrValue>,
) -> Option<IrValue> {
    let f = p
        .iter()
        .find(|f| f.name == "main")
        .expect("Need a main function!");

    eprintln!("{}", &f);
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
    let mut vars = HashMap::<Var, IrValue>::new();

    // Insert the rx value
    assert_eq!(parameters.len(), rx.len(), "incorrect number of arguments");
    for ((x, _), v) in parameters.iter().zip(rx) {
        vars.insert(Var { var: x.into() }, v);
    }

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
                let val = eval_operand(&vars, examinee);
                let x = val.switch_on() as usize;

                let label = branches
                    .iter()
                    .find_map(|(i, branch)| (*i == x).then_some(branch))
                    .unwrap_or(default);
                program_counter = block_map[label];
                continue;
            }
            Instruction::Assign { to, val, .. } => {
                let val = eval_operand(&vars, val);
                vars.insert(to.clone(), val.clone());
            }
            Instruction::Call {
                to,
                ty: _,
                func,
                args,
            } => {
                mem.push_frame(program_counter, to.clone());
                for (name, arg) in args {
                    let val = eval_operand(&vars, arg);
                    vars.insert(
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
                    .map(|a| eval_operand(&vars, a).clone())
                    .collect();
                let val = func.call(args);
                vars.insert(to.clone(), val);
            }
            Instruction::Return(ret) => {
                let val =
                    ret.as_ref().map(|r| eval_operand(&vars, r).clone());
                if let Some(StackFrame {
                    id: _,
                    allocations: _,
                    return_address,
                    return_place,
                }) = mem.pop_frame()
                {
                    if let Some(val) = val {
                        vars.insert(return_place.unwrap(), val.clone());
                    }
                    program_counter = return_address + 1;
                    continue;
                } else {
                    return val;
                }
            }
            Instruction::Cmp {
                to,
                cmp,
                left,
                right,
            } => {
                let left = eval_operand(&vars, left);
                let right = eval_operand(&vars, right);
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
                vars.insert(to.clone(), IrValue::Bool(res));
            }
            Instruction::Eq { to, left, right } => {
                let left = eval_operand(&vars, left);
                let right = eval_operand(&vars, right);
                vars.insert(to.clone(), IrValue::Bool(left.eq(right)));
            }
            Instruction::Not { to, val } => {
                let val = eval_operand(&vars, val).as_bool();
                vars.insert(to.clone(), IrValue::Bool(val));
            }
            Instruction::And { to, left, right } => {
                let left = eval_operand(&vars, left).as_bool();
                let right = eval_operand(&vars, right).as_bool();
                vars.insert(to.clone(), IrValue::Bool(left && right));
            }
            Instruction::Or { to, left, right } => {
                let left = eval_operand(&vars, left).as_bool();
                let right = eval_operand(&vars, right).as_bool();
                vars.insert(to.clone(), IrValue::Bool(left || right));
            }
            Instruction::Offset { to, from, offset } => {
                let &IrValue::Pointer(from) = eval_operand(&vars, from)
                else {
                    panic!()
                };
                let new = mem.offset_by(from, *offset as usize);
                vars.insert(to.clone(), IrValue::Pointer(new));
            }
            Instruction::Alloc { to, size } => {
                let pointer = mem.allocate(*size as usize);
                vars.insert(to.clone(), IrValue::Pointer(pointer));
            }
            Instruction::Write { to, val } => {
                let &IrValue::Pointer(to) = eval_operand(&vars, to) else {
                    panic!()
                };
                let val = eval_operand(&vars, val);
                mem.write(to, &val.as_vec())
            }
            Instruction::Read { to, from, ty } => {
                let &IrValue::Pointer(from) = eval_operand(&vars, from)
                else {
                    panic!()
                };
                let size = ty.bytes();
                let res = mem.read(from, size);
                let val = IrValue::from_slice(ty, res);
                vars.insert(to.clone(), val);
            }
            Instruction::Copy { to, from, size } => {
                let &IrValue::Pointer(to) = eval_operand(&vars, to) else {
                    panic!()
                };
                let &IrValue::Pointer(from) = eval_operand(&vars, from)
                else {
                    panic!()
                };
                mem.copy(to, from, *size as usize);
            }
            Instruction::MemCmp {
                to,
                size,
                left,
                right,
            } => {
                let &IrValue::Pointer(left) = eval_operand(&vars, left)
                else {
                    panic!()
                };
                let &IrValue::Pointer(right) = eval_operand(&vars, right)
                else {
                    panic!()
                };
                let left = mem.read(left, *size as usize);
                let right = mem.read(right, *size as usize);
                let res = left == right;
                vars.insert(to.clone(), IrValue::Bool(res));
            }
        }

        program_counter += 1;
    }
}

fn eval_operand<'a>(
    mem: &'a HashMap<Var, IrValue>,
    op: &'a Operand,
) -> &'a IrValue {
    match op {
        Operand::Place(p) => {
            let Some(v) = mem.get(p) else {
                panic!("No value was found for place {p}")
            };
            v
        }
        Operand::Value(v) => v,
    }
}
