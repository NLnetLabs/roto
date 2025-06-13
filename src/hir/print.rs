use crate::{
    ast::{BinOp, Literal},
    lir::ir::{IrPrinter, Printable},
    typechecker::{scope::ResolvedName, scoped_display::ScopedDisplay},
};

use super::ir::{
    Block, Field, Function, Hir, Instruction, Operand, Var, VarKind,
};

impl Printable for Hir {
    fn print(&self, printer: &crate::lir::ir::IrPrinter) -> String {
        let strings: Vec<_> =
            self.functions.iter().map(|f| f.print(printer)).collect();
        strings.join("\n")
    }
}

impl Printable for Function {
    fn print(&self, printer: &IrPrinter) -> String {
        let mut s = String::new();

        s.push_str(&format!(
            "fn {}({}) {{",
            self.name,
            self.parameters
                .iter()
                .map(|a| a.print(printer))
                .collect::<Vec<_>>()
                .join(", ")
        ));

        let blocks = self.blocks.iter();
        for b in blocks {
            s.push('\n');
            for line in b.print(printer).lines() {
                s.push_str(&format!("  {line}\n"));
            }
        }

        s.push_str("}\n\n");
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

impl Printable for Instruction {
    fn print(&self, printer: &IrPrinter) -> String {
        use Instruction::*;
        match self {
            Jump(to) => format!("jump {}", to.print(printer)),
            Switch {
                examinee,
                branches,
                default,
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
            MakeRecord { to, ty, fields } => {
                format!(
                    "{} = {} {{ {} }}",
                    to.print(printer),
                    ty.display(&printer.scope_graph),
                    fields
                        .iter()
                        .map(|(i, v)| {
                            format!("{i}: {}", v.print(printer))
                        })
                        .collect::<Vec<_>>()
                        .join(", "),
                )
            }
            MakeEnum {
                to,
                ty,
                variant,
                fields,
            } => {
                format!(
                    "{} = {}.{}({})",
                    to.print(printer),
                    ty.display(&printer.scope_graph),
                    variant,
                    fields
                        .iter()
                        .map(|v| { v.print(printer) })
                        .collect::<Vec<_>>()
                        .join(", "),
                )
            }
            ReadField { to, from, field } => {
                format!(
                    "{} = {}.{}",
                    to.print(printer),
                    from.print(printer),
                    field.print(printer),
                )
            }
            Assign {
                to,
                root_ty: _,
                fields,
                val,
            } => {
                format!(
                    "{}{} = {}",
                    to.print(printer),
                    fields
                        .iter()
                        .map(|(v, _)| v.print(printer))
                        .collect::<Vec<_>>()
                        .join("."),
                    val.print(printer),
                )
            }
            Call { to, func, args } => {
                format!(
                    "{} = {}({})",
                    to.print(printer),
                    func.print(printer),
                    args.iter()
                        .map(|a| a.print(printer))
                        .collect::<Vec<_>>()
                        .join(", "),
                )
            }
            Return(operand) => {
                format!("return {}", operand.print(printer))
            }
            Not { to, op } => {
                format!("{} = not({})", to.print(printer), op.print(printer))
            }
            BinOp {
                to,
                left,
                op,
                right,
            } => {
                let to = to.print(printer);
                let left = left.print(printer);
                let op = op.print(printer);
                let right = right.print(printer);
                format!("{to} = {left} {op} {right}")
            }
            Drop { value } => {
                let value = value.print(printer);
                format!("drop({value})")
            }
        }
    }
}

impl Printable for Field {
    fn print(&self, _printer: &IrPrinter) -> String {
        match self {
            Field::Numbered(n) => n.to_string(),
            Field::Named(identifier) => identifier.to_string(),
            Field::Discriminant => "$discriminant".into(),
        }
    }
}

impl Printable for Operand {
    fn print(&self, printer: &IrPrinter) -> String {
        match self {
            Operand::Place(x) => x.print(printer),
            Operand::Value(x) => x.print(printer),
        }
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
            }
        )
    }
}

impl Printable for BinOp {
    fn print(&self, _printer: &IrPrinter) -> String {
        self.to_string()
    }
}

impl Printable for ResolvedName {
    fn print(&self, printer: &IrPrinter) -> String {
        let f = self.scope.print(printer);
        format!("{f}.{}", self.ident)
    }
}

impl Printable for Literal {
    fn print(&self, _printer: &IrPrinter) -> String {
        match self {
            Literal::String(str) => str.into(),
            Literal::Asn(asn) => asn.to_string(),
            Literal::IpAddress(ip) => ip.to_string(),
            Literal::Integer(i) => i.to_string(),
            Literal::Float(f) => f.to_string(),
            Literal::Bool(b) => b.to_string(),
            Literal::Unit => "()".into(),
        }
    }
}
