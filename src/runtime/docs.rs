use std::any::TypeId;

use crate::{
    runtime::{
        context::ContextDescription, FunctionKind, RuntimeConstant,
        RuntimeFunction,
    },
    Runtime, RuntimeType,
};

impl Runtime {
    fn print_ty(&self, ty: TypeId) -> &str {
        let ty = self.get_runtime_type(ty).unwrap();
        ty.name.as_ref()
    }

    fn print_function(&self, f: &RuntimeFunction) {
        let RuntimeFunction {
            name,
            description,
            kind,
            id: _,
            docstring,
            argument_names,
        } = f;

        let params = description
            .parameter_types()
            .iter()
            .map(|ty| self.print_ty(*ty))
            .collect::<Vec<_>>();

        let ret = self.print_ty(description.return_type());

        let mut argument_names = argument_names.iter();
        let mut params = params.iter();
        let receiver = match *kind {
            FunctionKind::Method(_) => {
                // Discard the name of the receiver from the arguments
                let _ = argument_names.next();
                format!("{}.", params.next().unwrap())
            }
            FunctionKind::StaticMethod(id) => {
                format!("{}.", self.print_ty(id))
            }
            FunctionKind::Free => "".into(),
        };

        let mut parameter_string = String::new();
        let mut first = true;
        for param in params {
            if !first {
                parameter_string.push_str(", ");
            }
            let name = argument_names.next().map_or("_", |v| v);
            parameter_string.push_str(name);
            parameter_string.push_str(": ");
            parameter_string.push_str(param);
            first = false;
        }

        let kind = match kind {
            FunctionKind::Free => "function",
            FunctionKind::Method(_) => "method",
            FunctionKind::StaticMethod(_) => "static_method",
        };
        println!(
            "````{{roto:{kind}}} {receiver}{name}({parameter_string}) -> {ret}"
        );
        for line in docstring.lines() {
            println!("{line}")
        }
        println!("````");
        println!();
    }

    pub fn print_documentation(&self) {
        println!("# Standard Library");
        println!();

        for f in &self.functions {
            if f.kind != FunctionKind::Free {
                continue;
            }
            self.print_function(f);
        }

        if let Some(ContextDescription {
            type_id: _,
            type_name: _,
            fields,
        }) = &self.context
        {
            for crate::ContextField {
                name,
                offset: _,
                type_name: _,
                type_id,
                docstring,
            } in fields
            {
                println!(
                    "`````{{roto:context}} {name}: {}",
                    self.print_ty(*type_id)
                );
                for line in docstring.lines() {
                    println!("{line}");
                }
                println!("`````\n");
            }
        }

        for RuntimeConstant {
            name,
            ty,
            docstring,
            ..
        } in self.constants.values()
        {
            println!("`````{{roto:constant}} {name}: {}", self.print_ty(*ty));
            for line in docstring.lines() {
                println!("{line}");
            }
            println!("`````\n");
        }

        for RuntimeType {
            name,
            type_id,
            docstring,
            ..
        } in &self.types
        {
            println!("`````{{roto:type}} {name}");
            for line in docstring.lines() {
                println!("{line}");
            }
            println!();

            for f in &self.functions {
                let id = match f.kind {
                    FunctionKind::Free => continue,
                    FunctionKind::Method(id)
                    | FunctionKind::StaticMethod(id) => id,
                };
                if id != *type_id {
                    continue;
                }
                self.print_function(f);
            }

            println!("`````\n")
        }
    }
}
