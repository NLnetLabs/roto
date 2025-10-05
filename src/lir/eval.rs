//! Evaluate IR programs
//!
//! This is mostly used for testing purposes, since this evaluation is
//! fairly slow. This is because all variables at this point are identified
//! by strings and therefore stored as a hashmap.

use log::trace;

use crate::{
    ast::Identifier,
    lir::{
        value::IrValue, FloatCmp, Function, Instruction, IntCmp, Operand,
        ValueOrSlot, Var, VarKind,
    },
    runtime::{ConstantValue, RuntimeFunctionRef},
    typechecker::{scope::ResolvedName, types::Primitive},
    Runtime,
};
use std::{collections::HashMap, sync::Arc};

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
    /// Counter for the id's of stack frames
    id_counter: usize,

    /// The current stack
    stack: Vec<StackFrame>,

    /// All pointers that have been given out
    pointers: Vec<Pointer>,
}

#[derive(Clone, Debug)]
enum Pointer {
    Global(GlobalPointer),
    Local(LocalPointer),
}

#[derive(Clone, Debug)]
pub struct GlobalPointer {
    ptr: *mut (),
}

#[derive(Clone, Debug)]
pub struct LocalPointer {
    /// Where in the stack the pointee lives
    stack_index: usize,

    /// ID of the stack frame where the pointee lives
    stack_id: usize,

    /// Allocation within the stack frame this pointer was created with
    allocation_index: usize,

    /// Offset within the allocation this pointer refers to
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
    inner: Box<[u8]>,
}

impl LocalPointer {
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
        let data: Vec<_> = self.read_slice(from, size).into();
        self.write(to, &data);
    }

    pub fn write(&mut self, p: usize, val: &[u8]) {
        let p = &self.pointers[p];
        match p {
            Pointer::Local(p) => {
                let frame = &mut self.stack[p.stack_index];
                assert_eq!(frame.id, p.stack_id);
                frame.write(p, val)
            }
            Pointer::Global(_p) => {
                panic!("Don't write to globals!");
            }
        }
    }

    pub fn read_array<const N: usize>(&self, p: usize) -> [u8; N] {
        let slice = self.read_slice(p, N);
        slice.try_into().unwrap()
    }

    pub fn read_slice(&self, p: usize, size: usize) -> &[u8] {
        let p = &self.pointers[p];
        match p {
            Pointer::Local(p) => {
                let frame = &self.stack[p.stack_index];
                assert_eq!(frame.id, p.stack_id);
                frame.read(p, size)
            }
            Pointer::Global(p) => unsafe {
                std::slice::from_raw_parts(p.ptr as *mut u8, size)
            },
        }
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
        match p {
            Pointer::Local(p) => {
                self.pointers.push(Pointer::Local(p.offset_by(offset)));
                self.pointers.len() - 1
            }
            Pointer::Global(_) => {
                panic!("Don't offset global pointer");
            }
        }
    }

    pub fn allocate(&mut self, bytes: usize) -> usize {
        let stack_index = self.stack.len() - 1;
        let frame = &mut self.stack[stack_index];
        let stack_id = frame.id;
        let allocation_index = frame.allocations.len();
        frame.allocations.push(Allocation {
            inner: vec![0; bytes].into_boxed_slice(),
        });
        self.pointers.push(Pointer::Local(LocalPointer {
            stack_index,
            stack_id,
            allocation_index,
            allocation_offset: 0,
        }));
        self.pointers.len() - 1
    }

    pub fn get(&self, p: usize) -> *mut () {
        let p = &self.pointers[p];
        match p {
            Pointer::Local(p) => {
                let frame = &self.stack[p.stack_index];
                frame.get(p)
            }
            Pointer::Global(p) => p.ptr,
        }
    }
}

impl StackFrame {
    fn write(&mut self, p: &LocalPointer, val: &[u8]) {
        let alloc = &mut self.allocations[p.allocation_index];
        alloc.write(p.allocation_offset, val);
    }

    fn read(&self, p: &LocalPointer, size: usize) -> &[u8] {
        let alloc = &self.allocations[p.allocation_index];
        alloc.read(p.allocation_offset, size)
    }

    fn get(&self, p: &LocalPointer) -> *mut () {
        let alloc = &self.allocations[p.allocation_index];
        alloc.get(p.allocation_offset)
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

    fn get(&self, offset: usize) -> *mut () {
        &self.inner[offset] as *const _ as *mut _
    }
}

/// Evaluate IR
///
/// This is mostly used for testing purposes, since this evaluation is
/// fairly slow. This is because all variables at this point are identified
/// by strings and therefore stored as a hashmap.
pub fn eval(
    rt: &Runtime,
    p: &[Function],
    filter_map: &str,
    mem: &mut Memory,
    ctx: IrValue,
    args: Vec<IrValue>,
) -> Option<IrValue> {
    let filter_map_ident = Identifier::from(format!("pkg.{filter_map}"));
    let f = p
        .iter()
        .find(|f| f.name == filter_map_ident)
        .expect("Need a main function!");

    let parameters = f.ir_signature.parameters.clone();

    // Make the program easier to work with by collecting all instructions
    // and constructing a map from labels to indices.
    let mut block_map = HashMap::new();
    let mut instructions = Vec::new();

    for block in p.iter().flat_map(|f| &f.blocks) {
        block_map.insert(block.label, instructions.len());
        instructions.extend(block.instructions.clone());
    }

    let constants: HashMap<ResolvedName, ConstantValue> = rt
        .constants()
        .values()
        .map(|g| (g.name, g.value.clone()))
        .collect();

    // This is our working memory for the interpreter
    let mut vars = HashMap::<Var, IrValue>::new();

    if f.ir_signature.return_ptr {
        assert_eq!(
            parameters.len(),
            args.len() - 1,
            "incorrect number of arguments"
        );
    } else {
        assert_eq!(
            parameters.len(),
            args.len(),
            "incorrect number of arguments"
        );
    }

    vars.insert(
        Var {
            scope: f.scope,
            kind: VarKind::Context,
        },
        ctx,
    );
    let mut values = args.into_iter();
    if f.ir_signature.return_ptr {
        vars.insert(
            Var {
                scope: f.scope,
                kind: VarKind::Return,
            },
            values.next().unwrap(),
        );
    }

    for ((x, _), v) in parameters.iter().zip(values) {
        vars.insert(
            Var {
                scope: f.scope,
                kind: VarKind::Explicit(*x),
            },
            v,
        );
    }

    for (var, val_or_slot) in &f.variables {
        if let ValueOrSlot::StackSlot(layout) = val_or_slot {
            let ptr = mem.allocate(layout.size());
            vars.insert(var.clone(), IrValue::Pointer(ptr));
        }
    }

    let mut program_counter = block_map[&f.entry_block];

    loop {
        let instruction = &instructions[program_counter];
        trace!("{:?}", &instruction);
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
            Instruction::ConstantAddress { to, name } => {
                let x = constants.get(name).unwrap();
                let x = x.ptr();
                mem.pointers.push(Pointer::Global(GlobalPointer {
                    ptr: x as *mut (),
                }));
                vars.insert(
                    to.clone(),
                    IrValue::Pointer(mem.pointers.len() - 1),
                );
            }
            Instruction::Call {
                to,
                ctx,
                func,
                args,
                return_ptr,
            } => {
                let f = p.iter().find(|f| f.name == *func).unwrap();

                mem.push_frame(program_counter, to.clone().map(|to| to.0));

                for (var, val_or_slot) in &f.variables {
                    if let ValueOrSlot::StackSlot(layout) = val_or_slot {
                        let ptr = mem.allocate(layout.size());
                        vars.insert(var.clone(), IrValue::Pointer(ptr));
                    }
                }

                if let Some(return_ptr) = return_ptr {
                    vars.insert(
                        Var {
                            scope: f.scope,
                            kind: VarKind::Return,
                        },
                        eval_operand(&vars, &return_ptr.clone().into())
                            .clone(),
                    );
                }
                let ctx_val = eval_operand(&vars, ctx);
                vars.insert(
                    Var {
                        scope: f.scope,
                        kind: VarKind::Context,
                    },
                    ctx_val.clone(),
                );

                let names = f.ir_signature.parameters.iter().map(|p| p.0);

                for (name, arg) in names.zip(args) {
                    let val = eval_operand(&vars, arg);
                    vars.insert(
                        Var {
                            scope: f.scope,
                            kind: VarKind::Explicit(name),
                        },
                        val.clone(),
                    );
                }
                program_counter = block_map[&f.entry_block];
                continue;
            }
            Instruction::CallRuntime { func, args } => {
                let args: Vec<_> = args
                    .iter()
                    .map(|a| eval_operand(&vars, a).clone())
                    .collect();
                call_runtime_function(rt, mem, *func, args);
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
            Instruction::IntCmp {
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
                    IntCmp::ULt => left.as_u64() < right.as_u64(),
                    IntCmp::ULe => left.as_u64() <= right.as_u64(),
                    IntCmp::UGt => left.as_u64() > right.as_u64(),
                    IntCmp::UGe => left.as_u64() >= right.as_u64(),
                    IntCmp::SLt => left.as_i64() < right.as_i64(),
                    IntCmp::SLe => left.as_i64() <= right.as_i64(),
                    IntCmp::SGt => left.as_i64() > right.as_i64(),
                    IntCmp::SGe => left.as_i64() >= right.as_i64(),
                };
                vars.insert(to.clone(), IrValue::Bool(res));
            }
            Instruction::FloatCmp {
                to,
                cmp,
                left,
                right,
            } => {
                let left = eval_operand(&vars, left);
                let right = eval_operand(&vars, right);
                let res = match cmp {
                    FloatCmp::Eq => left == right,
                    FloatCmp::Ne => left != right,
                    FloatCmp::Lt => left.as_f64() < right.as_f64(),
                    FloatCmp::Le => left.as_f64() <= right.as_f64(),
                    FloatCmp::Gt => left.as_f64() > right.as_f64(),
                    FloatCmp::Ge => left.as_f64() >= right.as_f64(),
                };
                vars.insert(to.clone(), IrValue::Bool(res));
            }
            Instruction::Not { to, val } => {
                let val = eval_operand(&vars, val).as_bool();
                vars.insert(to.clone(), IrValue::Bool(val));
            }
            Instruction::Negate { to, val } => {
                let val = eval_operand(&vars, val);
                let res = match val {
                    IrValue::I8(x) => IrValue::I8(-x),
                    IrValue::I16(x) => IrValue::I16(-x),
                    IrValue::I32(x) => IrValue::I32(-x),
                    IrValue::I64(x) => IrValue::I64(-x),
                    IrValue::F32(x) => IrValue::F32(-x),
                    IrValue::F64(x) => IrValue::F64(-x),
                    _ => panic!(),
                };
                vars.insert(to.clone(), res);
            }
            Instruction::Add { to, left, right } => {
                let left = eval_operand(&vars, left);
                let right = eval_operand(&vars, right);
                let res = match (left, right) {
                    (IrValue::U8(l), IrValue::U8(r)) => IrValue::U8(l + r),
                    (IrValue::U16(l), IrValue::U16(r)) => IrValue::U16(l + r),
                    (IrValue::U32(l), IrValue::U32(r)) => IrValue::U32(l + r),
                    (IrValue::U64(l), IrValue::U64(r)) => IrValue::U64(l + r),
                    (IrValue::I8(l), IrValue::I8(r)) => IrValue::I8(l + r),
                    (IrValue::I16(l), IrValue::I16(r)) => IrValue::I16(l + r),
                    (IrValue::I32(l), IrValue::I32(r)) => IrValue::I32(l + r),
                    (IrValue::I64(l), IrValue::I64(r)) => IrValue::I64(l + r),
                    _ => panic!(),
                };
                vars.insert(to.clone(), res);
            }
            Instruction::Sub { to, left, right } => {
                let left = eval_operand(&vars, left);
                let right = eval_operand(&vars, right);
                let res = match (left, right) {
                    (IrValue::U8(l), IrValue::U8(r)) => IrValue::U8(l - r),
                    (IrValue::U16(l), IrValue::U16(r)) => IrValue::U16(l - r),
                    (IrValue::U32(l), IrValue::U32(r)) => IrValue::U32(l - r),
                    (IrValue::U64(l), IrValue::U64(r)) => IrValue::U64(l - r),
                    (IrValue::I8(l), IrValue::I8(r)) => IrValue::I8(l - r),
                    (IrValue::I16(l), IrValue::I16(r)) => IrValue::I16(l - r),
                    (IrValue::I32(l), IrValue::I32(r)) => IrValue::I32(l - r),
                    (IrValue::I64(l), IrValue::I64(r)) => IrValue::I64(l - r),
                    _ => panic!(),
                };
                vars.insert(to.clone(), res);
            }
            Instruction::Mul { to, left, right } => {
                let left = eval_operand(&vars, left);
                let right = eval_operand(&vars, right);
                let res = match (left, right) {
                    (IrValue::U8(l), IrValue::U8(r)) => IrValue::U8(l * r),
                    (IrValue::U16(l), IrValue::U16(r)) => IrValue::U16(l * r),
                    (IrValue::U32(l), IrValue::U32(r)) => IrValue::U32(l * r),
                    (IrValue::U64(l), IrValue::U64(r)) => IrValue::U64(l * r),
                    (IrValue::I8(l), IrValue::I8(r)) => IrValue::I8(l * r),
                    (IrValue::I16(l), IrValue::I16(r)) => IrValue::I16(l * r),
                    (IrValue::I32(l), IrValue::I32(r)) => IrValue::I32(l * r),
                    (IrValue::I64(l), IrValue::I64(r)) => IrValue::I64(l * r),
                    _ => panic!(),
                };
                vars.insert(to.clone(), res);
            }
            Instruction::Div {
                to,
                signed: _,
                left,
                right,
            } => {
                let left = eval_operand(&vars, left);
                let right = eval_operand(&vars, right);
                let res = match (left, right) {
                    (IrValue::U8(l), IrValue::U8(r)) => IrValue::U8(l / r),
                    (IrValue::U16(l), IrValue::U16(r)) => IrValue::U16(l / r),
                    (IrValue::U32(l), IrValue::U32(r)) => IrValue::U32(l / r),
                    (IrValue::U64(l), IrValue::U64(r)) => IrValue::U64(l / r),
                    (IrValue::I8(l), IrValue::I8(r)) => IrValue::I8(l / r),
                    (IrValue::I16(l), IrValue::I16(r)) => IrValue::I16(l / r),
                    (IrValue::I32(l), IrValue::I32(r)) => IrValue::I32(l / r),
                    (IrValue::I64(l), IrValue::I64(r)) => IrValue::I64(l / r),
                    _ => panic!(),
                };
                vars.insert(to.clone(), res);
            }
            Instruction::FDiv { to, left, right } => {
                let left = eval_operand(&vars, left);
                let right = eval_operand(&vars, right);
                let res = match (left, right) {
                    (IrValue::F32(l), IrValue::F32(r)) => IrValue::F32(l / r),
                    (IrValue::F64(l), IrValue::F64(r)) => IrValue::F64(l / r),
                    _ => panic!(),
                };
                vars.insert(to.clone(), res);
            }
            Instruction::Offset { to, from, offset } => {
                let &IrValue::Pointer(from) = eval_operand(&vars, from)
                else {
                    panic!()
                };
                let new = mem.offset_by(from, *offset as usize);
                vars.insert(to.clone(), IrValue::Pointer(new));
            }
            Instruction::Initialize { to, bytes, layout } => {
                // There are many cases where we only want to initialize the
                // start of an allocation, but it needs to be in bounds of the
                // allocation.
                assert!(bytes.len() <= layout.size());

                let pointer = mem.allocate(layout.size());
                mem.write(pointer, bytes);
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
                let res = mem.read_slice(from, size);
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

                mem.copy(to, from, *size as usize)
            }
            Instruction::Clone { to, from, clone_fn } => {
                let &IrValue::Pointer(to) = eval_operand(&vars, to) else {
                    panic!()
                };

                let &IrValue::Pointer(from) = eval_operand(&vars, from)
                else {
                    panic!()
                };

                let to = mem.get(to);
                let from = mem.get(from);
                unsafe { (clone_fn)(from, to) }
            }
            Instruction::Drop { var, drop } => {
                if let Some(drop) = drop {
                    let &IrValue::Pointer(val) = eval_operand(&vars, var)
                    else {
                        panic!()
                    };

                    let p = mem.get(val);
                    unsafe { (drop)(p) }
                }
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
                let &IrValue::Pointer(size) = eval_operand(&vars, size)
                else {
                    panic!()
                };
                let left = mem.read_slice(left, size);
                let right = mem.read_slice(right, size);
                let res = match left.cmp(right) {
                    std::cmp::Ordering::Less => -1isize as usize,
                    std::cmp::Ordering::Equal => 0,
                    std::cmp::Ordering::Greater => 1,
                };
                vars.insert(to.clone(), IrValue::Pointer(res));
            }
            Instruction::InitString {
                to,
                string,
                init_func: _,
            } => {
                let layout = Primitive::String.layout();
                let ptr = mem.allocate(layout.size());
                let ptr_value = mem.get(ptr) as *mut Arc<str>;
                unsafe {
                    std::ptr::write(ptr_value, Arc::from(string.as_ref()))
                };
                vars.insert(to.clone(), IrValue::Pointer(ptr));
            }
        }

        program_counter += 1;
    }
}

fn call_runtime_function(
    rt: &Runtime,
    mem: &mut Memory,
    func: RuntimeFunctionRef,
    args: Vec<IrValue>,
) {
    let func = rt.get_function(func);

    // The number of passed arguments should be the number of arguments the
    // function takes plus 1 for the out pointer.
    assert_eq!(func.func.parameter_types().len() + 1, args.len());

    (func.func.ir_function())(mem, args)
}

fn eval_operand<'a>(
    mem: &'a HashMap<Var, IrValue>,
    op: &'a Operand,
) -> &'a IrValue {
    match op {
        Operand::Place(p) => {
            let Some(v) = mem.get(p) else {
                panic!(
                    "No value was found for place {p:?} in memory: {mem:#?}"
                )
            };
            v
        }
        Operand::Value(v) => v,
    }
}
