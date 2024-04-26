//! Machine code generation via cranelift

// Time to write some hints for myself for next week.
// 1. Translate basic functions with u32
// 2. Figure out translating Rust types to Roto types
// 3. Type check to a specific Rust function signature? Should be automatic
//    ideally, where we can give it `fn(u32) -> u32` and it will know what
//    types to expect on the Roto side.
// 4. Figure out sizes and more complicated instructions

use cranelift::{
    codegen::{
        entity::EntityRef,
        ir::{types::*, AbiParam, InstBuilder},
        settings,
    },
    frontend::{FunctionBuilder, FunctionBuilderContext, Variable},
};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{Linkage, Module};

use roto::

/// A compiled, ready-to-run Roto module
struct Module {
    /// The set of public functions and their signatures.
    _functions: HashMap<String, (FuncId, Signature)>,
    /// The inner cranelift module
    inner: JitModule,
}

/// A compiled, ready-to-run Roto module
struct ModuleBuilder {
    /// The set of public functions and their signatures.
    _functions: HashMap<String, (FuncId, Signature)>,
    /// The inner cranelift module
    inner: JitModule,
}

fn to_cranelift(ir: &[ir::Function], runtime: &Runtime) {
    /// The ISA is the Instruction Set Architecture. We always compile for
    /// the system we run on, so we use `cranelift_native` to get the ISA
    /// for the current system.
    let isa = cranelift_native::builder()
        .unwrap()
        .finish(settings::Flags::new(settings::builder()))
        .unwrap();

    let mut builder = JITBuilder::with_isa(
        isa.to_owned(),
        cranelift_module::default_libcall_names(),
    );

    // TODO: Declare runtime items here.

    let jit = JITModule::new(builder);

    /// Our functions might call each other, so we declare them before we
    /// define them. This is also when we start building the function
    /// hashmap.
    let module = Module {
        functions: HashMap::new(),
        inner: jit,
    };

    for func in ir {
        module.declare(func);
    }

    for func in ir {
        module.define(func);
    }

    let module = module.finalize();

    let func_id = module.get_function("main");

    let code_fn =
        unsafe { std::mem::transmute::<_, fn(i32) -> i32>(code_ptr) };
}

impl ModuleBuilder {
    fn declare(&mut self, func: &ir::Function) {
        todo!()
    }

    fn define(&mut self, func: &ir::Function) {
        todo!()
    }

    fn finalize(self) -> Module {
        self.inner.
    }
}

impl Module {
    fn get_function(&self, name: &str) -> Option<*const u8> {
        let id = self.functions.get(name)?;
        Some(self.inner.get_finalized_function[id])
    }
}
