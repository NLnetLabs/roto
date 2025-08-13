use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Token, Visibility};

#[proc_macro_derive(Context)]
pub fn roto_context(item: TokenStream) -> TokenStream {
    let item = parse_macro_input!(item as syn::DeriveInput);

    let struct_name = &item.ident;

    let syn::Data::Struct(s) = &item.data else {
        panic!("Only structs can be used as context");
    };

    let syn::Fields::Named(fields) = &s.fields else {
        panic!("Fields must be named");
    };

    let fields: Vec<_> = fields
        .named
        .iter()
        .map(|f| {
            if !matches!(f.vis, Visibility::Public(_)) {
                panic!("All fields must be marked pub")
            }

            let field_name = f.ident.as_ref().unwrap();
            let field_ty = &f.ty;
            let offset = quote!(std::mem::offset_of!(Self, #field_name));
            let type_name = quote!(std::any::type_name::<#field_ty>());
            let type_id = quote!(std::any::TypeId::of::<#field_ty>());
            let docstring = gather_docstring(&f.attrs);

            quote!(
                roto::__internal::ContextField {
                    name: stringify!(#field_name),
                    offset: #offset,
                    type_name: #type_name,
                    type_id: #type_id,
                    docstring: String::from(#docstring),
                }
            )
        })
        .collect();

    let expanded = quote!(
        unsafe impl Context for #struct_name {
            fn fields() -> Vec<roto::__internal::ContextField> {
                vec![
                    #(#fields),*
                ]
            }
        }
    );

    TokenStream::from(expanded)
}

struct Intermediate {
    function: proc_macro2::TokenStream,
    fn_ty: proc_macro2::TokenStream,
    ident: syn::Ident,
    docstring: String,
    parameter_names: proc_macro2::TokenStream,
}

struct FunctionArgs {
    runtime_ident: syn::Ident,
    name: Option<syn::Ident>,
}

impl syn::parse::Parse for FunctionArgs {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let runtime_ident = input.parse()?;

        let mut name = None;
        if input.peek(Token![,]) {
            input.parse::<Token![,]>()?;
            if input.peek(syn::Ident) {
                name = input.parse()?;
            }
        }

        Ok(Self {
            runtime_ident,
            name,
        })
    }
}

#[proc_macro_attribute]
pub fn roto_function(attr: TokenStream, item: TokenStream) -> TokenStream {
    let item = parse_macro_input!(item as syn::ItemFn);
    let Intermediate {
        function,
        fn_ty: ty,
        ident,
        docstring,
        parameter_names,
    } = generate_function(item);

    let FunctionArgs {
        runtime_ident,
        name,
    } = syn::parse(attr).unwrap();

    let name = name.unwrap_or(ident.clone());

    let expanded = quote! {
        #function

        #runtime_ident.register_function::<#ty>(
            stringify!(#name),
            #docstring.to_string(),
            #parameter_names,
            #ident,
        ).unwrap();
    };

    TokenStream::from(expanded)
}

struct MethodArgs {
    runtime_ident: syn::Ident,
    ty: syn::Type,
    name: Option<syn::Ident>,
}

impl syn::parse::Parse for MethodArgs {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let runtime_ident = input.parse()?;
        input.parse::<Token![,]>()?;
        let ty = input.parse()?;

        let mut name = None;
        if input.peek(Token![,]) {
            input.parse::<Token![,]>()?;
            if input.peek(syn::Ident) {
                name = input.parse()?;
            }
        }
        Ok(Self {
            runtime_ident,
            ty,
            name,
        })
    }
}

#[proc_macro_attribute]
pub fn roto_method(attr: TokenStream, item: TokenStream) -> TokenStream {
    let item = parse_macro_input!(item as syn::ItemFn);
    let Intermediate {
        function,
        fn_ty,
        ident,
        docstring,
        parameter_names,
    } = generate_function(item);

    let MethodArgs {
        runtime_ident,
        ty,
        name,
    } = parse_macro_input!(attr as MethodArgs);

    let name = name.unwrap_or(ident.clone());

    let expanded = quote! {
        #function

        #runtime_ident.register_method::<#ty, #fn_ty>(
            stringify!(#name),
            #docstring.to_string(),
            #parameter_names,
            #ident
        ).unwrap();
    };

    TokenStream::from(expanded)
}

#[proc_macro_attribute]
pub fn roto_static_method(
    attr: TokenStream,
    item: TokenStream,
) -> TokenStream {
    let item = parse_macro_input!(item as syn::ItemFn);
    let Intermediate {
        function,
        fn_ty,
        ident,
        docstring,
        parameter_names,
    } = generate_function(item);

    let MethodArgs {
        runtime_ident,
        ty,
        name,
    } = parse_macro_input!(attr as MethodArgs);

    let name = name.unwrap_or(ident.clone());

    let expanded = quote! {
        #function

        #runtime_ident.register_static_method::<#ty, #fn_ty>(
            stringify!(#name),
            #docstring.to_string(),
            #parameter_names,
            #ident
        ).unwrap();
    };

    TokenStream::from(expanded)
}

fn gather_docstring(attrs: &[syn::Attribute]) -> String {
    let mut docstring = String::new();

    for attr in attrs {
        if attr.path().is_ident("doc") {
            let value = match &attr.meta {
                syn::Meta::NameValue(name_value) => &name_value.value,
                _ => panic!("doc attribute must be a name and a value"),
            };
            let lit = match value {
                syn::Expr::Lit(expr_lit) => &expr_lit.lit,
                _ => panic!(
                    "argument to doc attribute must be a string literal"
                ),
            };
            let litstr = match lit {
                syn::Lit::Str(litstr) => litstr,
                _ => panic!(
                    "argument to doc attribute must be a string literal"
                ),
            };
            docstring.push_str(litstr.value().trim());
            docstring.push('\n');
        }
    }

    docstring
}

fn generate_function(item: syn::ItemFn) -> Intermediate {
    let syn::ItemFn {
        attrs,
        vis,
        sig,
        block: _,
    } = item.clone();

    let docstring = gather_docstring(&attrs);

    assert!(sig.unsafety.is_none());
    assert!(sig.variadic.is_none());

    let ident = sig.ident;
    let args: Vec<_> = sig
        .inputs
        .iter()
        .map(|i| {
            let syn::FnArg::Typed(syn::PatType { pat, .. }) = i else {
                panic!()
            };
            pat
        })
        .collect();

    let generics = sig.generics;
    let ret = match sig.output {
        syn::ReturnType::Default => quote!(()),
        syn::ReturnType::Type(_, t) => {
            quote!(#t)
        }
    };

    let input_types: Vec<_> = sig
        .inputs
        .clone()
        .into_iter()
        .map(|i| {
            let syn::FnArg::Typed(syn::PatType { ty, .. }) = i else {
                panic!()
            };
            ty
        })
        .collect();

    let mut transformed_params = Vec::new();
    let mut transformed_args = Vec::new();

    for (i, t) in input_types.iter().enumerate() {
        let ident =
            syn::Ident::new(&format!("_{i}"), proc_macro2::Span::call_site());
        transformed_params
            .push(quote!(#ident: <#t as roto::Reflect>::AsParam));
        transformed_args
            .push(quote!(<#t as roto::Reflect>::untransform(unsafe { <#t as roto::Reflect>::to_value(#ident) })));
    }

    let function = quote! {
        #(#attrs)*
        #vis extern "C" fn #ident #generics ( out: *mut <#ret as roto::Reflect>::Transformed, #(#transformed_params,)* ) {
            #item

            let res = #ident(#(#transformed_args),*);
            let res_transformed = <#ret as roto::Reflect>::transform(res);
            unsafe { std::ptr::write(out, res_transformed) };
        }
    };

    let ty = quote!( fn(#(#input_types),*) -> #ret );
    let parameter_names = quote!( &[#(stringify!(#args)),*] );

    Intermediate {
        function,
        fn_ty: ty,
        ident,
        docstring,
        parameter_names,
    }
}
