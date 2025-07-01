use crate::{
    ast::{BinOp, Literal},
    ir_printer::{IrPrinter, Printable},
    typechecker::{scope::ResolvedName, scoped_display::TypeDisplay},
};

use super::{
    Block, Function, Instruction, Mir, Place, Projection, Value, Var, VarKind,
};

impl Printable for Mir {
    fn print(&self, printer: &IrPrinter) -> String {
        let strings: Vec<_> =
            self.functions.iter().map(|f| f.print(printer)).collect();
        strings.join("\n")
    }
}

impl Printable for Function {
    fn print(&self, printer: &IrPrinter) -> String {
        let mut s = String::new();

        let printer = IrPrinter {
            scope: Some(self.scope),
            type_info: printer.type_info,
            label_store: printer.label_store,
        };

        s.push_str(&format!(
            "fn {}({}) {{",
            self.name,
            self.parameters
                .iter()
                .map(|a| a.print(&printer))
                .collect::<Vec<_>>()
                .join(", ")
        ));

        s.push('\n');
        for (var, ty) in &self.variables {
            let var = var.print(&printer);
            let ty = ty.display(printer.type_info);
            s.push_str(&format!("  {var}: {ty}\n"));
        }

        let blocks = self.blocks.iter();
        for b in blocks {
            s.push('\n');
            for line in b.print(&printer).lines() {
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
        writeln!(s, "{}:", &self.label.print(printer)).unwrap();
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
            Assign { to, ty, value } => {
                let to = to.print(printer);
                let ty = ty.display(printer.type_info);
                let value = value.print(printer);
                format!("{to}: {ty} = {value}")
            }
            Switch {
                examinee,
                branches,
                default,
            } => {
                format!(
                    "switch {} [{}]{}",
                    examinee.print(printer),
                    branches
                        .iter()
                        .map(|(i, b)| format!("{i} => {}", b.print(printer)))
                        .collect::<Vec<_>>()
                        .join(", "),
                    if let Some(default) = default {
                        format!(" else {}", default.print(printer))
                    } else {
                        "".into()
                    },
                )
            }
            SetDiscriminant { to, ty, variant } => {
                format!(
                    "{}.$discriminant = {}.{}",
                    to.print(printer),
                    ty.display(printer.type_info),
                    variant.name
                )
            }
            Return { var: value } => {
                format!("return {}", value.print(printer))
            }
            Drop { val, ty: _ } => {
                let val = val.print(printer);
                format!("drop({val})")
            }
        }
    }
}

impl Printable for Place {
    fn print(&self, printer: &IrPrinter) -> String {
        use std::fmt::Write;
        let mut var = self.var.print(printer);
        for projection in &self.projection {
            match projection {
                Projection::VariantField(_, i) => write!(var, ".{i}"),
                Projection::Field(ident) => write!(var, ".{ident}"),
            }
            .unwrap();
        }
        var
    }
}

impl Printable for Value {
    fn print(&self, printer: &IrPrinter) -> String {
        match self {
            Value::Const(x, ty) => {
                let ty = ty.display(printer.type_info);
                let x = x.print(printer);
                format!("{ty}({x})")
            }
            Value::Constant(x, ty) => {
                let ty = ty.display(printer.type_info);
                format!("load_constant({x}: {ty})")
            }
            Value::Context(x) => {
                format!("load_context({x})")
            }
            Value::Clone(place) => {
                let place = place.print(printer);
                format!("clone({place})")
            }
            Value::Discriminant(var) => {
                let var = var.print(printer);
                format!("discriminant({var})")
            }
            Value::Not(var) => {
                let var = var.print(printer);
                format!("not({var})")
            }
            Value::Move(var) => {
                let var = var.print(printer);
                format!("move({var})")
            }
            Value::BinOp {
                left,
                binop,
                ty,
                right,
            } => {
                let left = left.print(printer);
                let ty = ty.display(printer.type_info);
                let right = right.print(printer);
                format!("{left} {binop}({ty}) {right}")
            }
            Value::Call { func, args } => {
                let func = func.print(printer);
                let args = args
                    .iter()
                    .map(|a| a.print(printer))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{func}({args})")
            }
            Value::CallRuntime { func_ref, args } => {
                let args = args
                    .iter()
                    .map(|a| a.print(printer))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("runtime_func_{func_ref}({args})")
            }
        }
    }
}

impl Printable for Var {
    fn print(&self, printer: &IrPrinter) -> String {
        let name = match &self.kind {
            VarKind::Explicit(name) => name.print(printer).to_string(),
            VarKind::Tmp(idx) => format!("${idx}"),
            VarKind::NamedTmp(name, idx) => {
                format!("${}-{idx}", name.print(printer))
            }
        };
        if Some(self.scope) != printer.scope {
            let f = self.scope.print(printer);
            format!("{}.{name}", f,)
        } else {
            name
        }
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
