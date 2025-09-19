use std::any::TypeId;

use super::{
    context::ContextDescription, FunctionKind, Runtime, RuntimeConstant,
    RuntimeFunction, RuntimeType,
};

impl Runtime {
    fn print_ty(&self, ty: TypeId) -> &str {
        if ty == TypeId::of::<()>() {
            "()"
        } else {
            let ty = self.get_runtime_type(ty).unwrap();
            // TODO: This ignores the scope
            ty.name.ident.as_str()
        }
    }

    fn print_function(&self, f: &RuntimeFunction) {
        let RuntimeFunction {
            name,
            func,
            id: _,
            doc,
            params,
        } = f;

        let params = func
            .parameter_types()
            .iter()
            .map(|ty| self.print_ty(*ty))
            .collect::<Vec<_>>();

        let ret = self.print_ty(func.return_type());

        let mut argument_names = params.iter();
        let params = params.iter();
        // let receiver = match *kind {
        //     FunctionKind::Method(_) => {
        //         // Discard the name of the receiver from the arguments
        //         let _ = argument_names.next();
        //         format!("{}.", params.next().unwrap())
        //     }
        //     FunctionKind::StaticMethod(id) => {
        //         format!("{}.", self.print_ty(id))
        //     }
        //     FunctionKind::Free => "".into(),
        // };
        let receiver = "";

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

        // let kind = match kind {
        //     FunctionKind::Free => "function",
        //     FunctionKind::Method(_) => "method",
        //     FunctionKind::StaticMethod(_) => "static_method",
        // };
        let kind = "function";
        let name = name.ident;

        println!(
            "````{{roto:{kind}}} {receiver}{name}({parameter_string}) -> {ret}"
        );
        for line in doc.lines() {
            println!("{line}")
        }
        println!("````");
        println!();
    }

    /// Print documentation for all the types registerd into this runtime.
    ///
    /// The format for the documentation is markdown that can be passed to Sphinx.
    pub fn print_documentation(&self) {
        println!("# Standard Library");
        println!();

        for f in &self.functions {
            self.print_function(f);
        }

        if let Some(ContextDescription {
            type_id: _,
            type_name: _,
            fields,
        }) = &self.context
        {
            for super::context::ContextField {
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
            type_id: _,
            docstring,
            ..
        } in &self.types
        {
            println!("`````{{roto:type}} {}", name.ident);
            for line in docstring.lines() {
                println!("{line}");
            }
            println!();

            for f in &self.functions {
                self.print_function(f);
            }

            println!("`````\n")
        }
    }
}
