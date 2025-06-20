use super::ir::{Block, Function, Instruction, Lir, Operand, Var, VarKind};
use crate::ir_printer::{IrPrinter, Printable};

impl Printable for Lir {
    fn print(&self, printer: &IrPrinter) -> String {
        let strings: Vec<_> =
            self.functions.iter().map(|f| f.print(printer)).collect();
        strings.join("\n")
    }
}

impl Printable for Function {
    fn print(&self, printer: &IrPrinter) -> String {
        use std::fmt::Write;
        let mut blocks = self.blocks.iter();

        let mut s = String::new();

        let Some(b) = blocks.next() else {
            write!(s, "<empty function>").unwrap();
            return s;
        };

        s.push_str(&b.print(printer));

        for b in blocks {
            writeln!(s).unwrap();
            s.push_str(&b.print(printer));
        }

        s
    }
}

impl Printable for Block {
    fn print(&self, printer: &IrPrinter) -> String {
        use std::fmt::Write;
        let mut s = String::new();
        writeln!(s, ".{}", &self.label.print(printer)).unwrap();
        for i in &self.instructions {
            writeln!(s, "  {}", i.print(printer)).unwrap();
        }
        s
    }
}

impl Printable for Var {
    fn print(&self, printer: &IrPrinter) -> String {
        let f = self.scope.print(printer);
        format!(
            "{}.{}",
            f,
            match &self.kind {
                VarKind::Explicit(name) => name.print(printer).to_string(),
                VarKind::Tmp(idx) => format!("$tmp-{idx}"),
                VarKind::NamedTmp(name, idx) =>
                    format!("${}-{idx}", name.print(printer)),
                VarKind::Return => "$return".to_string(),
                VarKind::Context => "$context".to_string(),
            }
        )
    }
}

impl Printable for Instruction {
    fn print(&self, printer: &IrPrinter) -> String {
        use Instruction::*;
        match self {
            Assign { to, val, ty } => {
                format!(
                    "{}: {ty} = {}",
                    to.print(printer),
                    val.print(printer),
                )
            }
            LoadConstant { to, name, ty } => {
                format!(
                    "{}: {ty} = LoadConstant(\"{}\")",
                    to.print(printer),
                    name
                )
            }
            Call {
                to: Some((to, ty)),
                ctx,
                func,
                args,
                return_ptr: _,
            } => format!(
                "{}: {ty} = {}({}, {})",
                to.print(printer),
                func.print(printer),
                ctx.print(printer),
                args.iter()
                    .map(|a| a.print(printer))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Call {
                to: None,
                ctx,
                func,
                args,
                return_ptr: Some(ret),
            } => format!(
                "{}({}, {}, {})",
                func.print(printer),
                ret.print(printer),
                ctx.print(printer),
                args.iter()
                    .map(|a| a.print(printer).to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Call {
                to: None,
                ctx,
                func,
                args,
                return_ptr: None,
            } => format!(
                "{}({}, {})",
                func.print(printer),
                ctx.print(printer),
                args.iter()
                    .map(|a| a.print(printer))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            CallRuntime { func, args } => format!(
                "<rust function {:?}>({})",
                func,
                args.iter()
                    .map(|a| a.print(printer))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            InitString {
                to,
                string,
                init_func: _,
            } => {
                format!("{}: String = \"{string}\"", to.print(printer),)
            }
            Return(None) => "return".to_string(),
            Return(Some(v)) => {
                format!("return {}", v.print(printer))
            }
            IntCmp {
                to,
                cmp,
                left,
                right,
            } => {
                format!(
                    "{} = {cmp}({}, {})",
                    to.print(printer),
                    left.print(printer),
                    right.print(printer),
                )
            }
            FloatCmp {
                to,
                cmp,
                left,
                right,
            } => {
                format!(
                    "{} = {cmp}({}, {})",
                    to.print(printer),
                    left.print(printer),
                    right.print(printer),
                )
            }
            Eq { to, left, right } => {
                format!(
                    "{} = {} == {}",
                    to.print(printer),
                    left.print(printer),
                    right.print(printer),
                )
            }
            Not { to, val } => {
                format!("{} = not({})", to.print(printer), val.print(printer))
            }
            And { to, left, right } => {
                format!(
                    "{} = {} & {}",
                    to.print(printer),
                    left.print(printer),
                    right.print(printer),
                )
            }
            Or { to, left, right } => {
                format!(
                    "{} = {} | {}",
                    to.print(printer),
                    left.print(printer),
                    right.print(printer),
                )
            }
            Add { to, left, right } => {
                format!(
                    "{} = {} + {}",
                    to.print(printer),
                    left.print(printer),
                    right.print(printer),
                )
            }
            Sub { to, left, right } => {
                format!(
                    "{} = {} - {}",
                    to.print(printer),
                    left.print(printer),
                    right.print(printer),
                )
            }
            Mul { to, left, right } => {
                format!(
                    "{} = {} * {}",
                    to.print(printer),
                    left.print(printer),
                    right.print(printer),
                )
            }
            Div {
                to,
                signed,
                left,
                right,
            } => {
                format!(
                    "{}: = {} / {} ({})",
                    to.print(printer),
                    left.print(printer),
                    right.print(printer),
                    if *signed { "signed" } else { "unsigned" },
                )
            }
            FDiv { to, left, right } => {
                format!(
                    "{}: = {} / {} (float)",
                    to.print(printer),
                    left.print(printer),
                    right.print(printer),
                )
            }
            Extend { to, ty, from } => {
                format!(
                    "{}: extend({ty}, {})",
                    to.print(printer),
                    from.print(printer),
                )
            }
            Jump(to) => {
                format!("jump {}", to.print(printer))
            }
            Switch {
                examinee,
                default,
                branches,
            } => {
                format!(
                    "switch {} [{}] else {}",
                    examinee.print(printer),
                    branches
                        .iter()
                        .map(|(i, b)| format!("{i} => {}", b.print(printer)))
                        .collect::<Vec<_>>()
                        .join(", "),
                    default.print(printer),
                )
            }
            Alloc { to, layout } => {
                format!(
                    "{} = mem::alloc(size={}, align={})",
                    to.print(printer),
                    layout.size(),
                    layout.align(),
                )
            }
            Initialize { to, bytes, layout } => {
                format!(
                    "{} = mem::initialize([{}], size={}, align={})",
                    to.print(printer),
                    bytes
                        .iter()
                        .map(|b| b.to_string())
                        .collect::<Vec<_>>()
                        .join(", "),
                    layout.size(),
                    layout.align(),
                )
            }
            Offset { to, from, offset } => {
                format!(
                    "{} = ptr::offset({}, {offset})",
                    to.print(printer),
                    from.print(printer),
                )
            }
            Read { to, from, ty } => {
                format!(
                    "{}: {ty} = mem::read({})",
                    to.print(printer),
                    from.print(printer),
                )
            }
            Write { to, val } => {
                format!(
                    "mem::write({}, {})",
                    to.print(printer),
                    val.print(printer),
                )
            }
            Copy {
                to,
                from,
                size,
                clone: None,
            } => {
                format!(
                    "mem::copy({}, {}, {size})",
                    to.print(printer),
                    from.print(printer),
                )
            }
            Copy {
                to,
                from,
                size,
                clone: Some(clone),
            } => {
                format!(
                    "mem::copy({}, {}, {size}, with={clone:?})",
                    to.print(printer),
                    from.print(printer),
                )
            }
            MemCmp {
                to,
                size,
                left,
                right,
            } => {
                format!(
                    "{} = mem::cmp({}, {}, {})",
                    to.print(printer),
                    left.print(printer),
                    right.print(printer),
                    size.print(printer),
                )
            }
            Drop {
                var,
                drop: Some(drop),
            } => {
                format!("mem::drop({}, with={drop:?})", var.print(printer))
            }
            Drop { var, drop: None } => {
                format!("mem::drop({}, noop)", var.print(printer))
            }
        }
    }
}

impl Printable for Operand {
    fn print(&self, printer: &IrPrinter) -> String {
        match self {
            Operand::Place(x) => x.print(printer),
            Operand::Value(x) => x.to_string(),
        }
    }
}
